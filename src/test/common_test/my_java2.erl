-module(my_java2).

-behaviour(gen_java).

-export([start_link/0, start/0, call/3, stop/0]).

start_link() ->
    gen_java:start_link(?MODULE).

start() ->
    gen_java:start(?MODULE).

%% gen_java callbacks
call(Module, Function, Args) ->
    gen_java:call(?MODULE, {Module, Function, Args}).

stop() ->
    gen_java:stop(?MODULE).
