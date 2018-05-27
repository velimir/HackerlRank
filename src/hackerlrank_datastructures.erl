-module(hackerlrank_datastructures).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([ransom_note/2]).

%%%===================================================================
%%% API
%%%===================================================================

ransom_note(Magazine, Note) ->
    Table = init_dict(Magazine),
    is_magazine_suitable(Table, Note).


%%%===================================================================
%%% Internal functions
%%%===================================================================
init_dict(Magazine) ->
    Table = ets:new(magazine, [public]),
    init_dict(Table, Magazine),
    Table.

init_dict(Table, Magazine) ->
    lists:foreach(
      fun(Word) -> ets:update_counter(Table, Word, 1, {Word, 0}) end,
      Magazine).

is_magazine_suitable(_Table, []) ->
    true;
is_magazine_suitable(Table, [Word | Note]) ->
    case ets:update_counter(Table, Word, {2, -1, 0, -1}, {Word, 0}) of
        -1 ->
            false;
        _Counter ->
            is_magazine_suitable(Table, Note)
    end.

%%%===================================================================
%%% Unit tests
%%%===================================================================

ransom_note_test_() ->
    [
     ?_assert(ransom_note("a b c", "a b")),
     ?_assert(ransom_note("give me one grand today night", "give one grand today")),
     ?_assertNot(ransom_note("give me one", "give one grand today")),
     ?_assertNot(ransom_note("", "give one grand today")),
     ?_assert(ransom_note("give one grand today", ""))
    ].
