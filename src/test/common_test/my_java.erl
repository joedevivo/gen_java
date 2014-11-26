-module(my_java).

-compile({parse_transform, gen_java_parse_transform}).

-export([abs/1, node/0, java_abs/1]).

abs(X) ->
    call(erlang, abs, [X]).

java_abs(X) ->
    call('com.devivo.gen_java.Erlang', abs, [X]).

node() ->
    call(erlang, node, []).
