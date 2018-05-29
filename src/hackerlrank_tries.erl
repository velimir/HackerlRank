-module(hackerlrank_tries).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([count/2, new/0, add/2, find_all/2]).

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

count([] = _String, Node) ->
    lookup_size(Node);
count([Ch | Suffix], Node) ->
    case ch_lookup(Node, Ch) of
        undefined ->
            0;
        ChildNode ->
            count(Suffix, ChildNode)
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

inc_size(Node) ->
    ets:update_counter(Node, size, 1, {size, 0}).

lookup_size(Node) ->
    case ets:lookup(Node, size) of
        [{size, Size}] ->
            Size;
        _ ->
            0
    end.

add([Ch | Suffix], Node, String) ->
    inc_size(Node),
    case ch_lookup(Node, Ch) of
        undefined ->
            ChildNode = new(),
            set_children(Node, Ch, ChildNode),
            add(Suffix, ChildNode, String);
        ChildNode ->
            add(Suffix, ChildNode, String)
    end;
add([] = _String, Node, String) ->
    inc_size(Node),
    set_value(Node, String).

swipe(Node) ->
    ets:foldl(
      fun
          ({{ch, _Ch}, ChildNode}, Acc) ->
              lists:append(swipe(ChildNode), Acc);
          ({value, Value}, Acc) ->
              [Value | Acc];
          (_, Acc) ->
              Acc
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
              ?_assertEqual(lists:sort(Expected), lists:sort(find_all(Find, Node)))}
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


count_def(Strings, Find, Expected) ->
    {setup,
     fun new/0,
     fun(Node) ->
             Add = fun(Str) -> add(Str, Node) end,
             lists:foreach(Add, Strings),
             {lists:flatten(io_lib:format("Add: ~p Find: ~p", [Strings, Find])),
              ?_assertEqual(Expected, count(Find, Node))}
     end}.

count_test_() ->
    [count_def(Strings, Find, Expected)
     || {Strings, Find, Expected}
            <- [{[],                     "",      0},
                {[""],                   "",      1},
                {["a"],                  "",      1},
                {["a"],                  "a",     1},
                {["a"],                  "b",     0},
                {["aa"],                 "a",     1},
                {["aa", "ab", "aaac"],   "a",     3},
                {["aa", "ab", "aaac"],   "aa",    2},
                {["aa", "ab", "aaac"],   "aaa",   1},
                {["aa", "ab", "aaac"],   "aaac",  1},
                {["aa", "ab", "aaac"],   "aaacb", 0},
                {["hackerrank", "hack"], "hac",   2},
                {["hackerrank", "hack"], "hak",   0}]].
