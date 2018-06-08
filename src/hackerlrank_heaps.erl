-module(hackerlrank_heaps).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([new/0, new/1, push/2, pop/1, head/1, to_list/1]).

%% HackerRank API
-export([main/0]).


-record(node, {data :: integer(),
               left :: #node{},
               right :: #node{}}).

-type cmp_func() :: fun((#node{}, #node{}) -> boolean()).

-record(heap, {root :: #node{},
               size = 0 :: pos_integer(),
               cmp = fun less_than/2 :: cmp_func()}).

%%%===================================================================
%%% API
%%%===================================================================

main() ->
    {N, _} = string:to_integer(string:chomp(io:get_line(""))),

    MaxHeap = new(fun greater_than/2),
    MinHeap = new(),

    running_median(MaxHeap, MinHeap, 1, N).

running_median(_MaxHeap, _MinHeap, ItNum, Max) when ItNum > Max ->
    ok;
running_median(MaxHeap, MinHeap, ItNum, Max) ->
    {N, _} = string:to_integer(string:chomp(io:get_line(""))),
    {Median, NewMaxHeap, NewMinHeap} = push(N, MaxHeap, MinHeap),
    io:format("~.1f~n", [float(Median)]),
    running_median(NewMaxHeap, NewMinHeap, ItNum + 1, Max).

push(N, MaxHeap, MinHeap) ->
    {MaxHeap1, MinHeap1} =
        case median(MaxHeap, MinHeap) of
            Median when N >= Median; Median == undefined ->
                {NewMinHeap, NewMaxHeap} = pre_even(MinHeap, MaxHeap),
                {NewMaxHeap, push(N, NewMinHeap)};
            _ ->
                {NewMaxHeap, NewMinHeap} = pre_even(MaxHeap, MinHeap),
                {push(N, NewMaxHeap), NewMinHeap}
        end,
    {median(MaxHeap1, MinHeap1), MaxHeap1, MinHeap1}.

pre_even(From = #heap{size = Size}, To = #heap{size = Size}) ->
    {From, To};
pre_even(From, To) when From#heap.size > To#heap.size ->
    move(From, To);
pre_even(From, To) ->
    {From, To}.

move(From, To) ->
    {ok, {Value, NewFrom}} = pop(From),
    {NewFrom, push(Value, To)}.

median(#heap{size = 0}, #heap{size = 0}) ->
    undefined;
median(MaxHeap, MinHeap)
  when MaxHeap#heap.size == MinHeap#heap.size ->
    {ok, LH} = head(MaxHeap),
    {ok, RH} = head(MinHeap),
    (LH + RH) / 2;
median(MaxHeap, MinHeap) ->
    {_, Heap} = max({MaxHeap#heap.size, MaxHeap},
                    {MinHeap#heap.size, MinHeap}),
    {ok, Value} = head(Heap),
    Value.

new() ->
    #heap{}.

new(CmpFun) ->
    #heap{cmp = CmpFun}.

head(#heap{root = undefined}) ->
    {error, empty};
head(#heap{root = Node}) ->
    {ok, Node#node.data}.

push(Value, #heap{root = undefined} = Heap) ->
    Heap#heap{root = #node{data = Value}, size = 1};
push(Value, #heap{root = Node, cmp = CmpFun} = Heap) ->
    NewRoot = merge(#node{data = Value}, Node, CmpFun),
    Heap#heap{root = NewRoot, size = Heap#heap.size + 1}.

pop(#heap{root = undefined}) ->
    {error, empty};
pop(#heap{root = Node, cmp = CmpFun} = Heap) ->
    NewRoot = merge(Node#node.left, Node#node.right, CmpFun),
    NewHeap = Heap#heap{root = NewRoot, size = Heap#heap.size - 1},
    {ok, {Node#node.data, NewHeap}}.

to_list(Heap) ->
    to_list(Heap, []).

to_list(Heap, Acc) ->
    case pop(Heap) of
        {ok, {Value, NewHeap}} ->
            to_list(NewHeap, [Value | Acc]);
        {error, empty} ->
            lists:reverse(Acc)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

greater_than(#node{data = LH}, #node{data = RH}) -> LH > RH.

less_than(#node{data = LH}, #node{data = RH}) -> LH < RH.

merge(undefined, RH, _CmpFun) ->
    RH;
merge(LH, undefined, _CmpFun) ->
    LH;
merge(LH, RH, CmpFun) ->
    case CmpFun(LH, RH) of
        true ->
            #node{data = LH#node.data,
                  left = merge(LH#node.right, RH, CmpFun),
                  right = LH#node.left};
        false ->
            #node{data = RH#node.data,
                  left = merge(RH#node.right, LH, CmpFun),
                  right = RH#node.left}
    end.


merge_test_() ->
    [
     ?_assertEqual(#node{data = 1},
                   merge(#node{data = 1}, undefined, fun less_than/2)),
     ?_assertEqual(#node{data = 1},
                   merge(undefined, #node{data = 1}, fun less_than/2)),
     ?_assertEqual(#node{data = 1,
                         left = #node{data = 2}},
                   merge(#node{data = 1}, #node{data = 2}, fun less_than/2)),
     ?_assertEqual(#node{data = 1,
                         left = #node{data = 2}},
                   merge(#node{data = 2}, #node{data = 1}, fun less_than/2)),
     ?_assertEqual(#node{data = 1,
                         left = #node{data = 2, right = #node{data = 3}},
                         right = #node{data = 4}},
                   merge(#node{data = 1,
                               left = #node{data = 4}},
                         #node{data = 2,
                               right = #node{data = 3}},
                         fun less_than/2))
    ].


do([pop | Commands], Heap) ->
    case pop(Heap) of
        {ok, {_Value, NewHeap}} ->
            do(Commands, NewHeap);
        {error, empty} = Error ->
            Error
    end;
do([{push, Value} | Commands], Heap) ->
    NewHeap = push(Value, Heap),
    do(Commands, NewHeap);
do([], Heap) ->
    Heap.

heap_test_() ->
    MaxHeap = new(fun greater_than/2),
    [
     ?_assertEqual([1], to_list(do([{push, 1}], new()))),
     ?_assertEqual([1, 2], to_list(do([{push, 1}, {push, 2}], new()))),
     ?_assertEqual([1, 2], to_list(do([{push, 2}, {push, 1}], new()))),
     ?_assertEqual([0, 1, 2, 3, 4],
                   to_list(do([{push, 2}, {push, 1}, {push, 3}, {push, 0},
                               {push, 4}],
                              new()))),
     ?_assertEqual([-1, 2, 3, 4, 5],
                   to_list(do([{push, 2}, {push, 1}, {push, 3}, {push, 0},
                               {push, 4}, pop, {push, 5}, pop, {push, -1}],
                              new()))),
     ?_assertEqual([4, 3, 2, 1, 0],
                   to_list(do([{push, 2}, {push, 1}, {push, 3}, {push, 0},
                               {push, 4}],
                              MaxHeap))),
     ?_assertEqual([3, 2, 1, 0, -1],
                   to_list(do([{push, 2}, {push, 1}, {push, 3}, {push, 0},
                               {push, 4}, pop, {push, 5}, pop, {push, -1}],
                              MaxHeap)))
    ].

running_median_test() ->
    lists:foldl(
      fun
          ({N, ExpMedia}, {MaxHeap, MinHeap}) ->
              {Median, NewMaxHeap, NewMinHeap} = push(N, MaxHeap, MinHeap),
              ?assertEqual(ExpMedia, Median),
              {NewMaxHeap, NewMinHeap}
      end,
      {new(fun greater_than/2), new()},
      [{12, 12}, {4, 8.0}, {5, 5}, {3, 4.5}, {8, 5}, {7, 6.0}]
     ).


running_median_sample_test() ->
    lists:foldl(
      fun
          ({N, ExpMedia}, {MaxHeap, MinHeap}) ->
              {Median, NewMaxHeap, NewMinHeap} = push(N, MaxHeap, MinHeap),
              ?assertEqual(ExpMedia, float(Median)),
              {NewMaxHeap, NewMinHeap}
      end,
      {new(fun greater_than/2), new()},
      [{1,  1.0},
       {2,  1.5},
       {3,  2.0},
       {4,  2.5},
       {5,  3.0},
       {6,  3.5},
       {7,  4.0},
       {8,  4.5},
       {9,  5.0},
       {10, 5.5}]
     ).
