-module(hackerlrank_arrays).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([rotate/2, rotate_left/2]).

%%%===================================================================
%%% API
%%%===================================================================

rotate_left(Size, Shift) ->
    rotate(Size, -Shift).

rotate(Size, Shift) when is_integer(Size) ->
    rotate(new(Size), Shift);
rotate(Array, 0) ->
    Array;
rotate(Array, Shift) ->
    ArraySize = array:size(Array),
    NewArray = array:new(ArraySize),
    lists:foldl(
      fun({Index, Value}, AccArray) ->
              NewIndex = new_index(Index, Shift, ArraySize),
              array:set(NewIndex, Value, AccArray)
      end,
      NewArray,
      array:to_orddict(Array)
     ).


%%%===================================================================
%%% Internal functions
%%%===================================================================

new_index(Index, Shift, ArraySize) ->
    (((Index + Shift) rem ArraySize) + ArraySize) rem ArraySize.

new(Size) ->
    array:from_list([I || I <- lists:seq(1, Size)]).

%%%===================================================================
%%% Tests
%%%===================================================================

new_test_() ->
    [
     ?_assertEqual([1], array:to_list(new(1))),
     ?_assertEqual([1, 2], array:to_list(new(2))),
     ?_assertEqual([1, 2, 3, 4, 5], array:to_list(new(5))),
     ?_assertEqual([], array:to_list(new(0)))
    ].

new_index_test_() ->
    [
     ?_assertEqual(0, new_index(1, -1, 3)),
     ?_assertEqual(2, new_index(1, 1, 3)),
     ?_assertEqual(2, new_index(0, -1, 3)),
     ?_assertEqual(0, new_index(2, 1, 3)),
     ?_assertEqual(2, new_index(1, 4, 3)),
     ?_assertEqual(0, new_index(1, -4, 3)),
     ?_assertEqual(2, new_index(0, -4, 3)),
     ?_assertEqual(1, new_index(1, 0, 3))
    ].

rotate_test_() ->
    [
     ?_assertEqual([1, 2, 3], array:to_list(rotate(3, 0))),
     ?_assertEqual([2, 3, 1], array:to_list(rotate(3, -1))),
     ?_assertEqual([3, 1, 2], array:to_list(rotate(3, 1)))
    ].
