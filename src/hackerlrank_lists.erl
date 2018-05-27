-module(hackerlrank_lists).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([rotate/2]).

%%%===================================================================
%%% API
%%%===================================================================

rotate(Size, Shift) when is_integer(Size) ->
    rotate(new(Size), Shift);
rotate(List, Shift) when is_list(List) ->
    IndexDistance = head_index(List, Shift),
    rotate_list(List, IndexDistance).

%%%===================================================================
%%% Internal functions
%%%===================================================================
head_index([] = _List, _Shift) ->
    0;
head_index(List, Shift) ->
    Len = length(List),
    (Len - (Shift rem Len)) rem Len.

rotate_list(List, IndexDistance) ->
    rotate_list(List, IndexDistance, []).

rotate_list(List, 0 = _IndexDistance, Acc) ->
    lists:append(List, lists:reverse(Acc));
rotate_list([Head | Tail], IndexDistance, Acc) ->
    rotate_list(Tail, IndexDistance - 1, [Head | Acc]);
rotate_list([] = _List, _IndexDistance, Acc) ->
    Acc.

new(Size) ->
    lists:seq(1, Size).

%%%===================================================================
%%% Unit tests
%%%===================================================================
new_test_() ->
    [
     ?_assertEqual([1, 2, 3], new(3)),
     ?_assertEqual([1], new(1)),
     ?_assertEqual([], new(0))
    ].

head_index_test_() ->
    [
     ?_assertEqual(1, head_index([1, 2, 3], -1)),
     ?_assertEqual(2, head_index([1, 2, 3], -2)),
     ?_assertEqual(2, head_index([1, 2, 3], 1)),
     ?_assertEqual(1, head_index([1, 2, 3], 2)),
     ?_assertEqual(0, head_index([1, 2, 3], 0)),
     ?_assertEqual(0, head_index([], 1))
    ].

rotate_list_test_() ->
    [
     ?_assertEqual([2, 3, 1], rotate([1, 2, 3], -1)),
     ?_assertEqual([3, 1, 2], rotate([1, 2, 3], 1)),
     ?_assertEqual([3, 1, 2], rotate([1, 2, 3], 1)),
     ?_assertEqual([1, 2, 3], rotate([1, 2, 3], 0)),
     ?_assertEqual([2, 3, 1], rotate([1, 2, 3], -4)),
     ?_assertEqual([3, 1, 2], rotate([1, 2, 3], 4))
    ].
