-module(gen_java_basic_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(JARJAR, "gen_java-0.1.2-SNAPSHOT-jar-with-dependencies.jar").

all() ->  [erlang_abs_test, two_gen_javas_test, parse_transform_test].

init_per_testcase(_, Config) ->
    application:stop(sasl),
    ok = lager:start(),
    %% Bounce the lager backend for each test
    lager_common_test_backend:bounce(debug),

    Dir = jar_path(Config),
    ct:pal("Looking for jar in: ~p", [Dir]),

    [{java_path, Dir}|Config].

erlang_abs_test(Config) ->
    ct:pal("Running erlang_abs_test"),

    JavaModuleConfig = [{my_java, [{jar, filename:join([?config(java_path, Config), ?JARJAR])}]}],
    application:set_env(gen_java, modules, JavaModuleConfig),

    {ok, _} = gen_java:start_link(my_java),

    2 = my_java:call(erlang, abs, [-2]),
    2.0 = my_java:call(erlang, abs, [-2.0]),

    %% Now test uncached versions
    2 = my_java:call('com.devivo.gen_java.Erlang', abs, [-2]),
    2.0 = my_java:call('com.devivo.gen_java.Erlang', abs, [-2.0]),

    %% Test init call back
    <<"init">> = my_java:call('com.devivo.gen_java.Java', get_cookie, []),

    my_java:stop(),
    ok.

two_gen_javas_test(Config) ->

    JavaModuleConfig = [
                        {my_java, [{jar, filename:join([?config(java_path, Config), ?JARJAR])}]},
                        {my_java2, [{jar, filename:join([?config(java_path, Config),?JARJAR])}]}
                       ],
    application:set_env(gen_java, modules, JavaModuleConfig),

    {ok, _} = gen_java:start_link(my_java),
    {ok, _} = gen_java:start_link(my_java2),

    2 = my_java:call(erlang, abs, [-2]),
    2.0 = my_java:call(erlang, abs, [-2.0]),
    2 = my_java2:call(erlang, abs, [-2]),
    2.0 = my_java2:call(erlang, abs, [-2.0]),

    %% Now test uncached versions
    2 = my_java:call('com.devivo.gen_java.Erlang', abs, [-2]),
    2.0 = my_java:call('com.devivo.gen_java.Erlang', abs, [-2.0]),
    2 = my_java2:call('com.devivo.gen_java.Erlang', abs, [-2]),
    2.0 = my_java2:call('com.devivo.gen_java.Erlang', abs, [-2.0]),

    %% Now test node differences
    Node1 = atom_to_list(my_java:call(erlang, node, [])),
    ct:pal("Node1: ~p", [Node1]),
    Node2 = atom_to_list(my_java2:call(erlang, node, [])),
    ct:pal("Node2: ~p", [Node2]),

    %% Check the node names are correct
    "gen_java_my_java_" = string:substr(Node1, 1, 17),
    "gen_java_my_java2_" = string:substr(Node2, 1, 18),

    my_java:stop(),
    my_java2:stop(),
    ok.

parse_transform_test(Config) ->
    JavaModuleConfig = [{my_java, [{jar, filename:join([?config(java_path, Config),?JARJAR])}]}],
    application:set_env(gen_java, modules, JavaModuleConfig),

    {ok, _} = gen_java:start_link(my_java),

    2 = my_java:abs(-2),
    2.0 = my_java:abs(-2.0),
    2 = my_java:java_abs(-2),
    2.0 = my_java:java_abs(-2.0),
    Node = atom_to_list(my_java:call(erlang, node, [])),
    ct:pal("Node1: ~p", [Node]),

    "gen_java_my_java_" = string:substr(Node, 1, 17),
    ok.

jar_path(Config) ->
    Dir = lists:reverse(filename:split(?config(data_dir, Config))),
    {_, Good} = lists:split(4, Dir),
    filename:join(lists:reverse(Good) ++ ["target"]).
