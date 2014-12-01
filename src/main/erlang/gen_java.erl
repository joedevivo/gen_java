-module(gen_java).

-behaviour(gen_server).

%% API
-export([start_link/1, start/1, stop/1, call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

%% only wait 1 second for rpc:calls
-define(RPC_TIMEOUT, 1000).
-define(DEFAULT_THREAD_COUNT, 10).

-record(gen_java_state, {
          module   = erlang:error({undefined, module})   :: atom(),
          config   = erlang:error({undefined, config})   :: [proplists:property()] | undefined,
          nodename = erlang:error({undefined, nodename}) :: atom(),
          port     = erlang:error({undefined, port})     :: port() | undefined
}).

start_link(Module) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module], []).

start(Module) ->
    gen_server:start({local, Module}, ?MODULE, [Module], []).

stop(ServerName) ->
    gen_server:cast(ServerName, stop).

call(ServerName, {Module, Function, Args}) ->
    gen_server:call(ServerName, {call, {Module, Function, Args}}).

%% gen_server callbacks
init([Module]) ->
    lager:info("[gen_java][~p] starting (pid: ~p)", [Module, self()]),

    Config = module_config(Module),
    lager:debug("[gen_java][~p] config: ~p", [Module, Config]),

    Jar = proplists:get_value(jar, Config),

    Nodename = list_to_atom("gen_java_" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(node())),
    process_flag(trap_exit, true),
    Port = start_jar(Nodename, Jar, Module, proplists:get_value(thread_count, Config, ?DEFAULT_THREAD_COUNT)),

    log_first_lines_from_port(Module, Port),
    %% Wait at most ten seconds for the node to come up
    case wait_until(
                    fun() ->
                        X = rpc:call(Nodename, erlang, node, [], 10000),
                        lager:debug("[gen_java][~p] rpc:call(~p, erlang, node, []) = ~p", [Module, Nodename, X]),
                        Nodename =:= X
                    end, 20, 1000) of
        ok ->
            rpc:call(Nodename, erlang, link, [self()]),
            erlang:monitor_node(Nodename, true),
            {ok, #gen_java_state{ module = Module, config = Config, nodename = Nodename, port = Port }};
        timeout ->
            {stop, timeout}
    end.

handle_call({call, {Module, Function, Args}}, _From, #gen_java_state{nodename = N} = State) ->
    {reply, rpc:call(N, Module, Function, Args), State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({nodedown, N}, State = #gen_java_state{nodename = N, module = M}) ->
    lager:error("[gen_java][~p] Java node has gone down", [M]),
    {stop, nodedown, State};
%% Log data from the port at a debug level
handle_info({Port, {data, {_Type, Data}}}, #gen_java_state { port = Port, module = M } = State) ->
    lager:info("[gen_java][~p] ~s", [M, Data]),
    {noreply, State};
handle_info({'EXIT', _, _}, #gen_java_state { port = Port, module = M } = State) ->
    %% TODO: I forget why it doesn't care. Investigate and document
    lager:info("[gen_java][~p] received an 'EXIT', but doesn't care", [M]),
    safe_port_close(Port),
    {stop, normal, State}.

terminate(_Reason, #gen_java_state { nodename = undefined }) ->
    %% Java server isn't running; nothing to do
    ok;
terminate(_Reason, #gen_java_state{ nodename = N, port = Port, module = M }) ->
    %% Java server appears to still be running; send kill signal and wait
    %% for it to shutdown.
    unlink(Port),
    lager:info("[gen_java][~p] Sending `rex ! stop` from terminate", [M]),
    {rex, N} ! stop,
    safe_port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
-spec start_jar(atom(), file:filename(), atom(), pos_integer()) -> port().
start_jar(NodeToStart, JarFile, Module, ThreadCount) ->
    %% Spin up the Java server
    JavaFormatString = "java -server "
        ++ "-cp " ++ JarFile ++ " "
        ++ "com.devivo.gen_java.ErlangServer ~s ~s ~p",

    Cmd = ?FMT(JavaFormatString, [NodeToStart, erlang:get_cookie(), ThreadCount]),
    lager:info("[gen_java][~p] cmd: ~p", [Module, Cmd, ThreadCount]),
    start_sh(Cmd).

-spec module_config(atom()) -> [proplists:property()].
module_config(Module) ->
    case application:get_env(gen_java, modules) of
        [] ->
            undefined;
        {ok, Modules} ->
            case proplists:get_value(Module, Modules, undefined) of
                undefined ->
                    undefined;
                ModuleConfig ->
                    ModuleConfig
            end
    end.

-spec start_sh(string()) -> port().
start_sh(Cmd) ->
    Env = case application:get_env(gen_java, java_home) of
              undefined -> [];
              {ok, JH} -> [{"JAVA_HOME", JH}]
          end,
    Port = open_port({spawn, ?FMT("/bin/sh -c \"echo $$; exec ~s\"", [Cmd])},
                     [
                      %%{cd, Dir},
                      {env, Env},
                      exit_status, {line, 16384},
                      use_stdio, stderr_to_stdout]),
    link(Port),
    Port.

safe_port_close(Port) when is_port(Port) ->
    port_close(Port);
safe_port_close(_Port) ->
    meh.

wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Res = Fun(),
    case Res of
        true ->
            ok;
        _ when Retry == 1 ->
            timeout;
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry-1, Delay)
    end.

log_first_lines_from_port(Module, Port) ->
    log_port_lines(Module, Port,3).

log_port_lines(_, _, 0) -> ok;
log_port_lines(M, Port, N) ->
    receive
        {Port, {data, {_Type, Data}}} ->
            lager:info("[gen_java][~p] startup: ~p", [M, Data])
    after
        20000 ->
            lager:info("[gen_java] didn't output anything on startup")
    end,
    log_port_lines(M, Port, N-1).
