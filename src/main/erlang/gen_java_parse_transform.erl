-module(gen_java_parse_transform).

-export([parse_transform/2, func/3]).

-record(state, {
    module :: atom(), %% What module did we find?
    exports = [
               {start_link, 0},
               {start, 0},
               {call, 3},
               {call, 4},
               {stop, 0}
              ]
}).

parse_transform(AST, _Options) ->
    walk_ast([], AST, #state{}).

%% @private
%%-spec walk_ast(Acc :: [syntaxTree()], [syntaxTree()], #state{}) -> {[syntaxTree()], #state{}}.
walk_ast(Acc, [], _TheState) ->
    lists:reverse(Acc);
%% Right after the module directive add our exports and behavior directive
walk_ast(Acc, [{attribute, L, module, Module}=H|T], TheState) ->
    Exports = {attribute, L, export, TheState#state.exports},
    %% Not sure it needs to be a behaviour
    %%Behaviour = {attribute, L, behaviour, gen_java},
    %%walk_ast([Exports|[Behaviour|[H|Acc]]], T, TheState#state{module=Module});
    walk_ast([Exports|[H|Acc]], T, TheState#state{module=Module});
walk_ast(Acc, [{eof, Line}=H], TheState) ->
    ASTAddOns = generate_our_functions(Line, TheState),
    walk_ast([H|ASTAddOns] ++ Acc, [], TheState);
walk_ast(Acc, [H|T], TheState) ->
    walk_ast([H|Acc], T, TheState).

generate_our_functions(Line, TheState) ->
    [
     func({FunctionName, Arity}, Line, TheState) || {FunctionName, Arity} <- TheState#state.exports
    ].

func({start_link, 0}, L, #state{module=Module}) ->
    {function,L,start_link,0,
          [{clause,L,[],[],
               [{call,L,
                    {remote,L,{atom,L,gen_java},{atom,L,start_link}},
                    [{atom,L,Module}]}]}]};
func({start, 0}, L, #state{module=Module}) ->
    {function,L,start,0,
          [{clause,L,[],[],
               [{call,L,
                    {remote,L,{atom,L,gen_java},{atom,L,start}},
                    [{atom,L,Module}]}]}]};
func({stop, 0}, L, #state{module=Module}) ->
    {function,L,stop,0,
          [{clause,L,[],[],
               [{call,L,
                    {remote,L,{atom,L,gen_java},{atom,L,stop}},
                    [{atom,L,Module}]}]}]};
func({call, 3}, L, #state{module=Module}) ->
      {function,L,call,3,
          [{clause,L,
               [{var,L,'Module'},{var,L,'Function'},{var,L,'Args'}],
               [],
               [{call,L,
                    {remote,L,{atom,L,gen_java},{atom,L,call}},
                    [{atom,L,Module},
                     {var,L,'Module'},
                     {var,L,'Function'},
                     {var,L,'Args'}]}]}]};
func({call, 4}, L, #state{module=Module}) ->
      {function,L,call,4,
          [{clause,L,
               [{var,L,'Module'},{var,L,'Function'},{var,L,'Args'},{var,L,'Timeout'}],
               [],
               [{call,L,
                    {remote,L,{atom,L,gen_java},{atom,L,call}},
                    [{atom,L,Module},
                     {var,L,'Module'},
                     {var,L,'Function'},
                     {var,L,'Args'},
                     {var,L,'Timeout'}]}]}]}.
