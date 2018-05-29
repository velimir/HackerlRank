-module(hackerlrank_queues).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([is_balanced/1]).

%% Queue API
-export([new/0, in/2, out/1, head/1]).

%% HR API
-export([main/0]).


-define(is_opening(B), B == ${; B == $[; B == $().
-define(is_closing(B), B == $}; B == $]; B == $)).
-define(is_matching(O, C), O == $}, C == ${;
                           O == $), C == $(;
                           O == $], C == $[).

%%%===================================================================
%%% API
%%%===================================================================

main() ->
    {N, _} = string:to_integer(string:chomp(io:get_line(""))),
    lists:foldl(
      fun(_It, Queue) ->
              case get_command() of
                  {enqueue, Num} ->
                      in(Num, Queue);
                  dequeue ->
                      {_, Queue1} = out(Queue),
                      Queue1;
                  print_head ->
                      case head(Queue) of
                          {{value, Value}, Queue1} ->
                              io:format("~p~n", [Value]),
                              Queue1;
                          {empty, Queue1} ->
                              Queue1
                      end
              end
      end,
      new(),
      lists:seq(1, N)
    ).

get_command() ->
    Line = string:chomp(io:get_line("")),
    case re:split(Line, "\\s+", [trim, {return, list}]) of
        ["1", NumStr] ->
            {Num, _} = string:to_integer(NumStr),
            {enqueue, Num};
        ["2"] ->
            dequeue;
        ["3"] ->
            print_head
    end.

new() ->
    {[], []}.

in(Item, {[_] = Rear, [] = _Front}) ->
    {[Item], Rear};
in(Item, {Rear, Front}) ->
    {[Item | Rear], Front}.

out({[] = _Rear, [] = _Front} = Queue) ->
    {empty, Queue};
out({[Value] = _Rear, [] = _Front}) ->
    {{value, Value}, {[], []}};
out({Rear, [Value] = _Front}) ->
    {{value, Value}, r2q(Rear)};
out({Rear, [Item | Tail] = _Front}) ->
    {{value, Item}, {Rear, Tail}}.

r2q([]) ->
    {[], []};
r2q([_] = Rear) ->
    {[], Rear};
r2q([X, Y]) ->
    {[X], [Y]};
r2q(List) ->
    {Front, Rear} = lists:split(length(List) div 2 + 1, List),
    {Front, lists:reverse(Rear, [])}.


head({[] = _Rear, [] = _Tail} = Queue) ->
    {empty, Queue};
head({[Value] = _Rear, [] = _Front} = Queue) ->
    {{value, Value}, Queue};
head({_Rear, [Value]} = Queue) ->
    {{value, Value}, Queue};
head({_Rear, [H | _Tail]} = Queue) ->
    {{value, H}, Queue}.

is_balanced(Brackets) ->
    is_balanced(Brackets, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_balanced([] = _Brackets, [] = _Stack) ->
    true;
is_balanced([] = _Brackets, _Stack) ->
    false;
is_balanced([Bracket | _Brackets], [] = _Stack)
  when ?is_closing(Bracket) ->
    false;
is_balanced([Bracket | Brackets], Stack)
  when ?is_opening(Bracket) ->
    is_balanced(Brackets, [Bracket | Stack]);
is_balanced([ClosingBracket | Brackets], [OpeningBrackets | Stack])
  when ?is_matching(ClosingBracket, OpeningBrackets) ->
    is_balanced(Brackets, Stack);
is_balanced(_Brackets, _Stack) ->
    false.

q2l({Rear, Front}) ->
    Front ++ lists:reverse(Rear).

%%%===================================================================
%%% Unit tests
%%%===================================================================
is_balanced_test_() ->
    [
     ?_assert(is_balanced("")),
     ?_assert(is_balanced("{}")),
     ?_assert(is_balanced("[]")),
     ?_assert(is_balanced("()")),
     ?_assert(is_balanced("({})")),
     ?_assert(is_balanced("({[]})")),
     ?_assert(is_balanced("[{()}]")),
     ?_assert(is_balanced("[]{}()")),
     ?_assert(is_balanced("[({})]{}()")),
     ?_assert(is_balanced("({(){}[]})[]")),
     ?_assertNot(is_balanced("[")),
     ?_assertNot(is_balanced("{")),
     ?_assertNot(is_balanced("(")),
     ?_assertNot(is_balanced("]")),
     ?_assertNot(is_balanced("}")),
     ?_assertNot(is_balanced(")")),
     ?_assertNot(is_balanced("{)")),
     ?_assertNot(is_balanced("{})")),
     ?_assertNot(is_balanced("[{{{{{}}}}")),
     ?_assertNot(is_balanced("{[(])}"))
    ].


queue_test_() ->
    [
     ?_assertEqual([], q2l(new())),
     ?_assertEqual([2], q2l(in(2, new()))),
     ?_assertEqual([2, 1], q2l(in(1, (in(2, new()))))),
     ?_assertEqual([1, 2, 3], q2l(in(3, in(2, in(1, new()))))),
     ?_assertEqual({empty,{[],[]}}, head(new())),
     ?_assertEqual({{value,1}, {[1],[]}}, head(in(1, new()))),
     ?_assertEqual({{value, 1}, new()}, out(in(1, new())))
    ].
