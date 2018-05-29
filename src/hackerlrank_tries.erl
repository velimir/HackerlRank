-module(hackerlrank_tries).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([new/0, add/2, find_all/2]).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    ets:new(node, []).

add(String, Node) ->
    add(String, Node, String).

find_all([] = _String, Node) ->
    swipe(Node);
find_all([Ch | Suffix], Node) ->
    case ch_lookup(Node, Ch) of
        undefined ->
            [];
        ChildNode ->
            find_all(Suffix, ChildNode)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ch_lookup(Node, Ch) ->
    case ets:lookup(Node, {ch, Ch}) of
        [{_, ChildNode}] ->
            ChildNode;
        [] ->
            undefined
    end.

set_children(Node, Ch, Value) ->
    ets:insert(Node, {{ch, Ch}, Value}).

set_value(Node, Value) ->
    ets:insert(Node, {value, Value}).

add([Ch | Suffix], Node, String) ->
    case ch_lookup(Node, Ch) of
        undefined ->
            ChildNode = new(),
            set_children(Node, Ch, ChildNode),
            add(Suffix, ChildNode, String);
        ChildNode ->
            add(Suffix, ChildNode, String)
    end;
add([] = _String, Node, String) ->
    set_value(Node, String).

swipe(Node) ->
    ets:foldl(
      fun
          ({{ch, _Ch}, ChildNode}, Acc) ->
              lists:append(swipe(ChildNode), Acc);
          ({value, Value}, Acc) ->
              [Value | Acc]
      end,
      [],
      Node
    ).

%%%===================================================================
%%% Eunit test
%%%===================================================================

find_test_def(Strings, Find, Expected) ->
    {setup,
     fun new/0,
     fun(Node) ->
             Add = fun(Str) -> add(Str, Node) end,
             lists:foreach(Add, Strings),
             {lists:flatten(io_lib:format("Add: ~p Find: ~p", [Strings, Find])),
              ?_assertEqual(Expected, find_all(Find, Node))}
     end}.

find_all_test_() ->
    [find_test_def(Strings, Find, Expected)
     || {Strings, Find, Expected}
            <- [{[],                     "",      []},
                {[""],                   "",      [""]},
                {["a"],                  "",      ["a"]},
                {["a"],                  "a",     ["a"]},
                {["a"],                  "b",     []},
                {["aa"],                 "a",     ["aa"]},
                {["aa", "ab", "aaac"],   "a",     ["aaac", "aa", "ab"]},
                {["aa", "ab", "aaac"],   "aa",    ["aaac", "aa"]},
                {["aa", "ab", "aaac"],   "aaa",   ["aaac"]},
                {["aa", "ab", "aaac"],   "aaac",  ["aaac"]},
                {["aa", "ab", "aaac"],   "aaacb", []},
                {["hackerrank", "hack"], "hac",   ["hackerrank", "hack"]},
                {["hackerrank", "hack"], "hak",   []}]].
