-module(gen_java).

-behaviour(gen_server).

%% API
-export([start_link/1, start/1, stop/1, call/4, call/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(DEFAULT_THREAD_COUNT, 10).

-record(gen_java_state, {
          module   = erlang:error({undefined, module})   :: atom(),
          config   = erlang:error({undefined, config})   :: [proplists:property()] | undefined,
          nodename = erlang:error({undefined, nodename}) :: atom(),
          port     = erlang:error({undefined, port})     :: port() | undefined
}).

-type badrpc() :: {badrpc, term()}.
-export_type([badrpc/0]).

-spec start_link(module()) -> {ok, pid()}
                            | ignore
                            | {error, {already_started, pid()}}
                            | {error, term()}.
start_link(Module) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module], []).


-spec start(module()) -> {ok, pid()}
                       | ignore
                       | {error, {already_started, pid()}}
                       | {error, term()}.
start(Module) ->
    gen_server:start({local, Module}, ?MODULE, [Module], []).

-spec stop(module()) -> ok.
stop(ServerName) ->
    gen_server:cast(ServerName, stop).

-spec call(atom(), atom(), atom(), [term()]) -> term() | {badrpc, term()}.
call(ServerName, Module, Function, Args) ->
    gen_server:call(ServerName, {call, {Module, Function, Args}}).

-spec call(atom(), atom(), atom(), [term()], (pos_integer() | infinity)) -> term() | {badrpc, term()}.
call(ServerName, Module, Function, Args, Timeout) ->
    gen_server:call(ServerName, {call, {Module, Function, Args, Timeout}}).

%% gen_server callbacks
-spec init([module()]) -> {ok, #gen_java_state{}}
                        | {ok, #gen_java_state{}, (pos_integer() | infinity | hibernate)}.
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

-spec handle_call(
        ({call, {module(), atom(), [term()]}} |
         {call, {module(), atom(), [term()], timeout()}} |
         term()),
        {pid(), reference()},
        #gen_java_state{}) ->
                         {reply, (ignore | badrpc()| term()), #gen_java_state{}}.
handle_call({call, {Module, Function, Args}}, _From, #gen_java_state{nodename = N} = State) ->
    {reply, rpc:call(N, Module, Function, Args), State};
handle_call({call, {Module, Function, Args, Timeout}}, _From, #gen_java_state{nodename = N} = State) ->
    {reply, rpc:call(N, Module, Function, Args, Timeout), State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

-spec handle_cast((stop | term()), #gen_java_state{}) ->
                         {stop, normal, #gen_java_state{}}
                       | {noreply, #gen_java_state{}}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(
        ({nodedown, node()} |
         {port(), {data, {term(), term()}}} |
         {'EXIT', term(), term()}),
        #gen_java_state{}) ->
                         {stop, (nodedown | normal), #gen_java_state{}} |
                         {noreply, #gen_java_state{}}.
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
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), #gen_java_state{}) -> ok.
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

-spec code_change(term(), #gen_java_state{}, term()) -> {ok, #gen_java_state{}}.
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
    lager:info("[gen_java][~p] cmd: ~p", [Module, Cmd]),
    start_sh(Cmd, dir(Module)).

-spec module_config(atom()) -> [proplists:property()].
module_config(Module) ->
    case application:get_env(gen_java, modules) of
        undefined ->
            undefined;
        {ok, Modules} ->
            case proplists:get_value(Module, Modules, undefined) of
                undefined ->
                    undefined;
                ModuleConfig ->
                    ModuleConfig
            end
    end.

-spec start_sh(string(), file:filename_all()) -> port().
start_sh(Cmd, Dir) ->
    Env = case application:get_env(gen_java, java_home) of
              %% If undefined
              undefined -> [];
              %% If defined as 'undefined'
              {ok, undefined} -> [];
              %% If actually defined
              {ok, JH} -> [{"JAVA_HOME", JH}]
          end,
    Port = open_port({spawn, ?FMT("/bin/sh -c \"echo $$; exec ~s\"", [Cmd])},
                     [
                      {cd, Dir},
                      {env, Env},
                      exit_status, {line, 16384},
                      use_stdio, stderr_to_stdout]),
    link(Port),
    Port.

-spec dir(module()) -> file:filename_all().
dir(Module) ->
    Path0 = filename:dirname(code:which(Module)),
    Path1 = filename:absname_join(Path0, ".."),
    filename:absname(Path1).

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
