-module(my_java).

-compile({parse_transform, gen_java_parse_transform}).

-export([abs/1, node/0, java_abs/1, abs_timeout/2, init/1]).

-spec abs(integer() | float()) -> integer() | float() | gen_java:badrpc().
abs(X) ->
    call(erlang, abs, [X]).

-spec abs_timeout(integer() | float(), timeout()) -> integer() | float() | gen_java:badrpc().
abs_timeout(X, Timeout) ->
    call(erlang, abs, [X], Timeout).

-spec java_abs(integer() | float()) -> integer() | float() | gen_java:badrpc().
java_abs(X) ->
    call('com.devivo.gen_java.Erlang', abs, [X]).

-spec node() -> node() | gen_java:badrpc().
node() ->
    call(erlang, node, []).

-spec init(atom()) -> ok.
init(Nodename) ->
    rpc:call(Nodename, 'com.devivo.gen_java.Java', set_cookie, [<<"init">>]).