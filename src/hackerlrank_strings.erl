-module(hackerlrank_strings).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([anagram_distance/2]).

%%%===================================================================
%%% API
%%%===================================================================

anagram_distance(StrA, StrB) ->
    anagram_distance(lists:sort(StrA), lists:sort(StrB), 0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

anagram_distance([H | TailA], [H | TailB], Acc) ->
    anagram_distance(TailA, TailB, Acc);
anagram_distance([HA | TailA], [HB | _TailB] = B, Acc)
  when HA < HB ->
    anagram_distance(TailA, B, Acc + 1);
anagram_distance([HA | _TailA] = A, [HB | TailB], Acc)
  when HA > HB ->
    anagram_distance(A, TailB, Acc + 1);
anagram_distance([] = _A, B, Acc) ->
    Acc + length(B);
anagram_distance(A, [] = _B, Acc) ->
    Acc + length(A).

%%%===================================================================
%%% Unit tests
%%%===================================================================

anagram_distance_test_() ->
    [
     ?_assertEqual(0, anagram_distance("abc", "abc")),
     ?_assertEqual(2, anagram_distance("abb", "abc")),
     ?_assertEqual(4, anagram_distance("cde", "abc")),
     ?_assertEqual(0, anagram_distance("", "")),
     ?_assertEqual(3, anagram_distance("abc", "")),
     ?_assertEqual(3, anagram_distance("", "abc"))
    ].
