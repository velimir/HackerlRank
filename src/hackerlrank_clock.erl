%% Given an integer consisting of 4 digits, we need to maximize it in
%% 24 hour format. For example, 4372 should return a String of the
%% form 23:47, which is the maximum 24 hour value that can be obtained
%% from the given integer. Assume the given integer always contains
%% exactly 4 digits.
-module(hackerlrank_clock).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([max/1]).

%%%===================================================================
%%% API
%%%===================================================================

max(Digits) ->
    case split_hours(Digits) of
        {ok, {Hours, SuggestedMin}} ->
            case minutes(SuggestedMin) of
                {ok, Minutes} ->
                    lists:flatten(io_lib:format("~p~p:~p~p", Hours ++ Minutes));
                {error, unsplittable} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
split_hours(Digits) ->
    Patterns = [{2, 3}, {1, 9}, {0, 9}],
    Sorted = lists:sort(Digits),
    split_hours(Sorted, Patterns).

split_hours(_Digits, [] = _Patterns) ->
    {error, unsplittable};
split_hours(Digits, [{HourDec, HourMax} | Patterns]) ->
    case split_val(Digits, HourDec) of
        {_, []} ->
            split_hours(Digits, Patterns);
        {HDigits, [HourDec | Tail]} ->
            RevDigits = lists:reverse(Tail) ++ HDigits,
            case split_max(RevDigits, HourMax) of
                {_, []} ->
                    split_hours(Digits, Patterns);
                {MinDigits, [Max | MinTail]} ->
                    {ok, {[HourDec, Max], MinDigits ++ MinTail}}
            end;
        _ ->
            split_hours(Digits, Patterns)
    end.

split_val(Digits, Val) ->
    split_val(Digits, Val, []).

split_val([Head | _Tail] = Digits, Val, Acc) when Head >= Val ->
    {Acc, Digits};
split_val([Head | Tail] = _Digits, Val, Acc) ->
    split_val(Tail, Val, [Head | Acc]);
split_val([] = Digits, _Val, Acc) ->
    {Acc, Digits}.

split_max(Digits, Max) ->
    split_max(Digits, Max, []).

split_max([Digit | Digits], Max, Acc) when Digit > Max ->
    split_max(Digits, Max, [Digit | Acc]);
split_max(Digits, _Max, Acc)  ->
    {Acc, Digits}.

minutes([X, Y]) ->
    MinSorted = lists:reverse(lists:sort([{X, Y}, {Y, X}])),
    max_minutes(MinSorted).

max_minutes([{X, Y} | _Tail]) when X * 10 + Y < 60 ->
    {ok, [X, Y]};
max_minutes([_Pair | Tail]) ->
    max_minutes(Tail);
max_minutes([]) ->
    {error, unsplittable}.


split_val_test_() ->
    [
     ?_assertEqual({[1], [2, 3, 4]},   split_val([1, 2, 3, 4], 2)),
     ?_assertEqual({[], [1, 2, 3, 4]}, split_val([1, 2, 3, 4], 1)),
     ?_assertEqual({[3, 2, 1], [4]},   split_val([1, 2, 3, 4], 4)),
     ?_assertEqual({[4, 3, 2, 1], []}, split_val([1, 2, 3, 4], 5))
    ].

split_max_test_() ->
    [
     ?_assertEqual({[], [4, 3, 2, 1]}, split_max([4, 3, 2, 1], 4)),
     ?_assertEqual({[], [4, 3, 2, 1]}, split_max([4, 3, 2, 1], 5)),
     ?_assertEqual({[4], [3, 2, 1]},   split_max([4, 3, 2, 1], 3)),
     ?_assertEqual({[2, 3, 4], [1]},   split_max([4, 3, 2, 1], 1)),
     ?_assertEqual({[1, 2, 3, 4], []}, split_max([4, 3, 2, 1], 0))
    ].

split_hours_test_() ->
    [
     ?_assertEqual({ok, {[2, 3], [4, 1]}}, split_hours([1, 2, 3, 4])),
     ?_assertEqual({ok, {[2, 3], [4, 7]}}, split_hours([4, 3, 7, 2])),
     ?_assertEqual({ok, {[1, 9], [9, 5]}}, split_hours([9, 9, 5, 1])),
     ?_assertEqual({ok, {[0, 0], [0, 0]}}, split_hours([0, 0, 0, 0])),
     ?_assertEqual({ok, {[1, 0], [0, 0]}}, split_hours([0, 0, 0, 1])),
     ?_assertEqual({ok, {[2, 0], [0, 0]}}, split_hours([0, 0, 0, 2])),
     ?_assertEqual({ok, {[2, 0], [4, 0]}}, split_hours([0, 2, 0, 4])),
     ?_assertEqual({ok, {[1, 9], [0, 0]}}, split_hours([0, 1, 0, 9])),
     ?_assertEqual({error, unsplittable}, split_hours([9, 9, 5, 2])),
     ?_assertEqual({error, unsplittable}, split_hours([7, 6, 4, 2]))
    ].

minutes_test_() ->
    [
     ?_assertEqual({ok, [5, 9]}, minutes([5, 9])),
     ?_assertEqual({ok, [5, 9]}, minutes([9, 5])),
     ?_assertEqual({ok, [0, 6]}, minutes([6, 0])),
     ?_assertEqual({error, unsplittable}, minutes([6, 7]))
    ].

max_test_() ->
    [
     ?_assertEqual("23:47", max([4, 3, 7, 2])),
     ?_assertEqual("19:59", max([9, 9, 5, 1])),
     ?_assertEqual({error, unsplittable}, max([7, 6, 4, 2])),
     ?_assertEqual({error, unsplittable}, max([7, 6, 3, 2]))
    ].
