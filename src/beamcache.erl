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

forms(M, L) when is_atom(M), is_list(L) ->
    forms(M, L,
          try
              maps:from_list(L)
          catch
              error:badarg -> error(badarg, [L])
          end);
forms(M, D) when is_atom(M), is_map(D) -> forms(M, D, D).

forms(M, D, Map) ->
    [{attribute, 1, file, {atom_to_list(M) ++ ".erl", 1}},
     {attribute, 1, module, M},
     {attribute, 2, compile, {no_auto_import, [{get, 1}]}},
     {attribute, 4, export, [{get, 0}, {get, 1}, {get, 2}]},
     {function, 6, get, 0, [{clause, 6, [], [], [erl_parse:abstract(D, 6)]}]},
     {function, 8, get, 1,
      [{clause, 8, [{var, 8, 'K'}], [],
        [{'try', 9,
          [{call, 10, {remote, 10, {atom, 10, maps}, {atom, 10, get}},
            [{var, 10, 'K'}, erl_parse:abstract(Map, 10)]}],
          [],
          [{clause, 11,
            [{tuple, 12, [{atom, 12, error}, {tuple, 12, [{atom, 12, badkey}, {var, 12, '_'}]}, {var, 12, '_'}]}],
            [],
            [{call, 12, {atom, 12, error},
              [{tuple, 12, [{atom, 12, badkey}, {var, 12, 'K'}]}, {cons, 12, {var, 12, 'K'}, {nil, 12}}]}]}],
          []}]}]},
     {function, 15, get, 2,
      [{clause, 15, [{var, 15, 'K'}, {var, 15, 'D'}], [],
        [{call, 15, {remote, 15, {atom, 15, maps}, {atom, 15, get}},
          [{var, 15, 'K'}, erl_parse:abstract(Map, 15), {var, 15, 'D'}]}]}]},
     {eof, 16}].
