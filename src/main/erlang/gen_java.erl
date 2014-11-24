-module(gen_java).

-behaviour(gen_server).

%% API
-export([start_link/2, stop/0, call/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

%% only wait 1 second for rpc:calls
-define(RPC_TIMEOUT, 1000).

-record(state, { nodename,
                 port :: port()}).

start_link(Path, JarFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Path, JarFile], []).

stop() ->
    gen_server:cast(?MODULE, stop).

call(Module, Function, Args) ->
    gen_server:call(?MODULE, {call, {Module, Function, Args}}).

%% gen_server callbacks
init([Path, JarFile]) ->
    lager:info("starting gen_java (pid: ~p)", [self()]),

    %% this nodename limits us to only one java node per erlang node
    %% TODO: fix!
    Nodename = list_to_atom("gen_java_" ++ atom_to_list(node())),
    process_flag(trap_exit, true),
    Port = start_jar(Nodename, Path, JarFile),

    log_first_lines_from_port(Port),
    %% Wait at most ten seconds for the node to come up
    case wait_until(
                    fun() ->
                        X = rpc:call(Nodename, erlang, node, [], 10000),
                        lager:debug("rpc:call(~p, erlang, node, []) = ~p", [Nodename, X]),
                        Nodename =:= X
                    end, 20, 1000) of
        ok ->
            rpc:call(Nodename, erlang, link, [self()]),
            {ok, #state{ nodename = Nodename, port = Port }};
        timeout ->
            {stop, timeout}
    end.

handle_call({call, {Module, Function, Args}}, _From, #state{nodename = N} = State) ->
    {reply, rpc:call(N, Module, Function, Args), State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, #state { nodename = N } = State) ->
    {rex, N} ! stop,
    {stop, normal, State}.

%% Log data from the port at a debug level
handle_info({Port, {data, {_Type, Data}}}, #state { port = Port } = State) ->
    lager:info("[gen_java] ~s", [Data]),
    {noreply, State};
handle_info({'EXIT', _, _}, #state { port = Port } = State) ->
    %% TODO: I forget why it doesn't care. Investigate and document
    lager:info("gen_java received an 'EXIT', but doesn't care"),
    safe_port_close(Port),
    %%{noreply, State#state { port = undefined, pid = undefined }}.
    {stop, normal, State}.

terminate(_Reason, #state { nodename = undefined }) ->
    %% Java server isn't running; nothing to do
    ok;
terminate(_Reason, #state{ nodename = N }) ->
    %% Java server appears to still be running; send kill signal and wait
    %% for it to shutdown.
    {rex, N} ! stop,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
-spec start_jar(atom(), file:filename(), string()) -> port().
start_jar(NodeToStart, Path, JarFile) ->
    %% Spin up the Java server
    JavaFormatString = "java -server "
        ++ "-cp " ++ JarFile ++ " "
        ++ "com.devivo.gen_java.ErlangServer ~s ~s",

    Cmd = ?FMT(JavaFormatString, [NodeToStart, erlang:get_cookie()]),
    lager:info(Cmd),
    start_sh(Cmd, Path).

%-spec priv_dir() -> file:filename_all().
%priv_dir() ->
%    %% TODO: This priv_dir setting is wrong!
%    case code:priv_dir(gen_java) of
%        {error, bad_name} ->
%            Path0 = filename:dirname(code:which(?MODULE)),
%            Path1 = filename:absname_join(Path0, ".."),
%            filename:join([Path1, "priv"]);
%        Path ->
%            filename:absname(Path)
%    end.

-spec start_sh(string(), file:filename_all()) -> port().
start_sh(Cmd, Dir) ->
    Env = case application:get_env(gen_java, java_home) of
              undefined -> [];
              {ok, JH} -> [{"JAVA_HOME", JH}]
          end,
    Port = open_port({spawn, ?FMT("/bin/sh -c \"echo $$; exec ~s\"", [Cmd])},
                     [{cd, Dir},
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

log_first_lines_from_port(Port) ->
    log_port_lines(Port,3).

log_port_lines(_, 0) -> ok;
log_port_lines(Port, N) ->
    receive
        {Port, {data, {_Type, Data}}} ->
            lager:info("[gen_java] startup: ~p", [Data])
    after
        20000 ->
            lager:info("[gen_java] didn't output anything on startup")
    end,
    log_port_lines(Port, N-1).
