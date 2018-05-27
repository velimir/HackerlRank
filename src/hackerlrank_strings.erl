-module(hackerlrank_strings).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([anagram_distance/2,
         anagram_distance2/2]).

%%%===================================================================
%%% API
%%%===================================================================

anagram_distance(StrA, StrB) ->
    anagram_distance(lists:sort(StrA), lists:sort(StrB), 0).

anagram_distance2(StrA, StrB) ->
    Counters = dict:new(),
    Counters2 = lists:foldl(fun inc_count/2, Counters, StrA),
    Counters3 = lists:foldl(fun dec_count/2, Counters2, StrB),
    reduce_diff(Counters3).

%%%===================================================================
%%% Internal functions
%%%===================================================================
inc_count(Char, Counters) ->
    update_counter(Char, 1, Counters).

dec_count(Char, Counters) ->
    update_counter(Char, -1, Counters).

update_counter(Char, Increment, Counters) ->
    dict:update_counter(Char, Increment, Counters).

reduce_diff(Counters) ->
    dict:fold(
      fun
          (_Key, 0 = _Value, Acc) ->
              Acc;
          (_Key, Value, Acc) ->
              abs(Value) + Acc
      end,
      0,
      Counters).

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


anagram_distance2_test_() ->
    [
     ?_assertEqual(0, anagram_distance2("abc", "abc")),
     ?_assertEqual(2, anagram_distance2("abb", "abc")),
     ?_assertEqual(4, anagram_distance2("cde", "abc")),
     ?_assertEqual(0, anagram_distance2("", "")),
     ?_assertEqual(3, anagram_distance2("abc", "")),
     ?_assertEqual(3, anagram_distance2("", "abc"))
    ].
