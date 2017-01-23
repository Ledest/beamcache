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
     {function, 6, get, 0, [{clause, 6, [], [], [erl_parse:abstract(D, 6)]}]},
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

clause(I, [{K, V}|L], F) -> clause(I - 1, L, [{clause, I, [erl_parse:abstract(K, I)], [], [erl_parse:abstract(V, I)]}|F]);
clause(_I, [], F) -> F.
