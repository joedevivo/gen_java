-module(gen_java_basic_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->  [erlang_abs_test].

init_per_testcase(_, Config) ->
    application:stop(sasl),
    ok = lager:start(),
    %% Bounce the lager backend for each test
    lager_common_test_backend:bounce(debug),
    Config.

erlang_abs_test(Config) ->
    ct:pal("Running erlang_abs_test"),
    Dir = jar_path(Config),
    ct:pal("Looking for jar in: ~p", [Dir]),
    gen_java:start_link(Dir, "gen_java-0.0.1-SNAPSHOT-jar-with-dependencies.jar"),

    2 = gen_java:call(erlang, abs, [-2]),
    2.0 = gen_java:call(erlang, abs, [-2.0]),

    %% Now test uncached versions
    2 = gen_java:call('com.devivo.gen_java.Erlang', abs, [-2]),
    2.0 = gen_java:call('com.devivo.gen_java.Erlang', abs, [-2.0]),

    ok.

jar_path(Config) ->
    Dir = lists:reverse(filename:split(?config(data_dir, Config))),
    {_, Good} = lists:split(4, Dir),
    filename:join(lists:reverse(Good) ++ ["target"]).
