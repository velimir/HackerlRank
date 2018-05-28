-module(hackerlrank_queues).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([is_balanced/1]).

-define(is_opening(B), B == ${; B == $[; B == $().
-define(is_closing(B), B == $}; B == $]; B == $)).
-define(is_matching(O, C), O == $}, C == ${;
                           O == $), C == $(;
                           O == $], C == $[).

%%%===================================================================
%%% API
%%%===================================================================

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
