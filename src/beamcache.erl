-module(beamcache).

-export([init/2,
         forms/2,
         module/2, module/3]).

init(M, B) when is_atom(M), is_binary(B) ->
    code:purge(M),
    code:load_binary(M, atom_to_list(M) ++ ".erl", B);
init(M, D) when is_atom(M), is_list(D) orelse is_map(D) ->
    case module(M, D) of
        {ok, M, B} -> init(M, B);
        Error -> Error
    end.

module(M, D) when is_atom(M), is_list(D) orelse is_map(D) -> compile:forms(forms(M, D)).

module(M, D, O)  when is_atom(M), is_list(D) orelse is_map(D), is_list(O) -> compile:forms(forms(M, D, O)).

forms(M, D) when is_atom(M), is_list(D) -> forms(M, D, D);
forms(M, D) when is_atom(M), is_map(D) -> forms(M, D, maps:to_list(D)).

forms(M, D, L) ->
    I = 12 + length(L),
    [{attribute, 1, file, {atom_to_list(M) ++ ".erl", 1}},
     {attribute, 1, module, M},
     {attribute, 2, export, [{get, 0}, {get, 1}, {get, 2}]},
     {attribute, 4, compile, {no_auto_import, [{get, 1}]}},
     {function, 6, get, 0, [{clause, 6, [], [], [make_term(D, 6)]}]},
     {function, 8, get, 2,
      [{clause, 8, [{var, 8, 'K'}, {var, 8, 'D'}], [],
        [{'try', 9,
          [{call, 9, {atom, 9, get}, [{var, 9, 'K'}]}], [],
          [{clause, 10,
            [{tuple, 10, [{atom, 10, error}, {tuple, 10, [{atom, 10, badkey}, {var, 10, 'K'}]}, {var, 10, '_'}]}],
            [],
            [{var, 10, 'D'}]}],
          []}]}]},
     {function, 12, get, 1,
      clause(I - 1, lists:sort(fun erlang:'>='/2, L)) ++
      [{clause, I, [{var, I, 'K'}], [],
        [{call, I, {atom, I, error}, [{tuple, I, [{atom, I, badkey}, {var, I, 'K'}]}]}]}]},
     {eof, I + 1}].

clause(I, L) -> clause(I, L, []).

clause(I, [{K, V}|L], F) -> clause(I - 1, L, [{clause, I, [make_term(K, I)], [], [make_term(V, I)]}|F]);
clause(I, [K|L], F) -> clause(I, [{K, true}|L], F);
clause(_I, [], F) -> F.

-ifdef(USE_ERL_SYNTAX).
make_term(D, I) -> erl_syntax:revert(erl_syntax_lib:map(fun(T) -> erl_syntax:set_pos(T, I) end, erl_syntax:abstract(D))).
-else.
make_term(D, I) -> set_pos(erl_parse:abstract(D), I).

set_pos({T, _}, I) -> {T, I};
set_pos([_|_] = L, I) -> [set_pos(T, I) || T <- L];
set_pos(T, I) when tuple_size(T) >= 3 -> setelement(2, setelement(3, T, set_pos(element(3, T), I)), I);
set_pos(T, _I) -> T.
-endif.
