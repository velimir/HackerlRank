-module(hackerlrank_tries).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([main/0, new/0, add/2, find_all/2]).

-record(node, {children = #{} :: #{char() := #node{}},
               value :: string()}).

%%%===================================================================
%%% API
%%%===================================================================

main() ->
    {N, _} = string:to_integer(string:chomp(io:get_line(""))),

    lists:foldl(
      fun(_NItr, Trie) ->
              case read_contact_op() of
                  ["add", Contact] ->
                      add(Contact, Trie);
                  ["find", Contact] ->
                      Contacts = find_all(Contact, Trie),
                      io:format("~p~n", [length(Contacts)]),
                      Trie
              end
      end,
      new(),
      lists:seq(1, N)),

    ok.

new() ->
    #node{}.

add(String, Trie) ->
    add(String, Trie, String).


find_all([] = _String, #node{value = undefined, children = Children}) ->
    swipe(Children);
find_all([] = _String, #node{value = Value, children = Children}) ->
    [Value | swipe(Children)];
find_all([Ch | Suffix], #node{children = Children}) ->
    case Children of
        #{Ch := Node} ->
            find_all(Suffix, Node);
        _ ->
            []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add([Ch | Suffix], #node{children = Children} = Node, String) ->
    case Children of
        #{Ch := ChildNode} ->
            NewNode = add(Suffix, ChildNode, String),
            Node#node{children = Children#{Ch := NewNode}};
        _ ->
            NewNode = add(Suffix, #node{}, String),
            Node#node{children = Children#{Ch => NewNode}}
    end;
add([] = _String, Node, String) ->
    Node#node{value = String}.

swipe(Children) ->
    maps:fold(
      fun
          (_K, #node{value = undefined, children = NodeChildren}, Acc) ->
              swipe(NodeChildren) ++ Acc;
          (_K, #node{value = Value, children = NodeChildren}, Acc) ->
              [Value | swipe(NodeChildren)] ++ Acc
      end,
      [],
      Children
     ).


read_contact_op() ->
    Line = string:chomp(io:get_line("")),
    re:split(Line, "\\s+", [{return, list}, trim]).

%%%===================================================================
%%% Eunit test
%%%===================================================================

tries_add_test_() ->
    [
     ?_assertEqual(#node{value = ""}, add("", #node{})),
     ?_assertEqual(#node{children = #{$a => #node{value = "a"}}},
                   add("a", #node{})),
     ?_assertEqual(#node{children =
                             #{$a => #node{children =
                                               #{$a => #node{value = "aa"}}}}},
                   add("aa", #node{})),
     ?_assertEqual(#node{children =
                             #{$a => #node{children =
                                               #{$b => #node{value = "ab"}}}}},
                   add("ab", #node{})),
     ?_assertEqual(#node{children = #{$a => #node{value = "a"},
                                      $b => #node{value = "b"}}},
                   add("b", add("a", #node{}))),
     ?_assertEqual(#node{children =
                             #{$a =>
                                   #node{
                                      value = "a",
                                      children = #{$b => #node{value = "ab"}}}}},
                   add("ab", add("a", #node{}))),
     ?_assertEqual(#node{children =
                             #{$a =>
                                   #node{
                                      value = "a",
                                      children = #{$b => #node{value = "ab"}}}}},
                   add("a", add("ab", #node{}))),
     ?_assertEqual(#node{children =
                             #{$a =>
                                   #node{value = "a"},
                               $b =>
                                   #node{children = #{$a => #node{value = "ba"}}}}},
                   add("a", add("ba", #node{})))
    ].

find_all_test_() ->
    [
     ?_assertEqual([], find_all("", #node{})),
     ?_assertEqual([""], find_all("", add("", #node{}))),
     ?_assertEqual(["a"], find_all("", add("a", #node{}))),
     ?_assertEqual(["a"], find_all("a", add("a", #node{}))),
     ?_assertEqual([], find_all("b", add("a", #node{}))),
     ?_assertEqual(["aa"], find_all("a", add("aa", #node{}))),
     ?_assertEqual(["ab","aa","aaac"],
                   find_all("a",
                            add("aaac",
                                add("ab",
                                    add("aa", #node{}))))),
     ?_assertEqual(["aa","aaac"],
                   find_all("aa",
                            add("aaac",
                                add("ab",
                                    add("aa", #node{}))))),
     ?_assertEqual(["aaac"],
                   find_all("aaa",
                            add("aaac",
                                add("ab",
                                    add("aa", #node{}))))),
     ?_assertEqual(["aaac"],
                   find_all("aaac",
                            add("aaac",
                                add("ab",
                                    add("aa", #node{}))))),
     ?_assertEqual([],
                   find_all("aaacb",
                            add("aaac",
                                add("ab",
                                    add("aa", #node{}))))),
     ?_assertEqual(["hack", "hackerrank"],
                   find_all("hac",
                            add("hackerrank",
                                add("hack", #node{})))),
     ?_assertEqual([],
                   find_all("hak",
                            add("hackerrank",
                                add("hack", #node{}))))
    ].
