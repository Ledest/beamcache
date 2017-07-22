-module(beamcache).

-export([init/2, init/3,
         forms/2,
         load/2,
         module/2, module/3]).

-spec init(M::module(), B::binary()) -> {module, module()} | {error, badarg | code:load_error_rsn()};
          (M::module(), D::map()) -> {module, module()} | {error, badarg | code:load_error_rsn()} | error.
init(M, B) when is_atom(M), is_binary(B) -> load(M, B);
init(M, D) when is_atom(M), is_map(D) ->
    case module(M, D) of
        {ok, M, B} -> init(M, B, []);
        Error -> Error
    end.

-spec init(M::module(), B::binary(), O::code:options()) ->
          {module, module()} | {error, badarg | code:load_error_rsn()};
          (M::module(), D::map(), O::code:options()) ->
          {module, module()} | {error, badarg | code:load_error_rsn()} | error.
init(M, B, O) when is_atom(M), is_binary(B), is_list(O) ->
    case proplists:get_value(protected, O, true) of
        true ->
            true = code:unstick_mod(M),
            R = load(M, B),
            true = code:stick_mod(M),
            R;
        _ -> load(M, B)
    end;
init(M, D, O) when is_atom(M), is_map(D), is_list(O) ->
    case module(M, D, proplists:delete(protected, O)) of
        {ok, M, B} -> init(M, B, O);
        Error -> Error
    end.

-spec module(M::module(), D::[{_, _}]|map()) -> {ok, module(), binary()} | error.
module(M, D) -> module(M, D, []).

-spec module(M::module(), D::map(), O::code:options()) -> {ok, module(), binary()} | error.
module(M, D, O) when is_atom(M), is_map(D), is_list(O) ->
    compile:noenv_forms(forms(M, D), [no_line_info, slim|O]).

-spec forms(M::module(), D::map()) -> compile:abstract_code().
forms(M, D) ->
    [{attribute, 1, file, {atom_to_list(M) ++ ".erl", 1}},
     {attribute, 1, module, M},
     {attribute, 2, compile, {no_auto_import, [{get, 1}]}},
     {attribute, 4, export, [{get, 0}, {get, 1}, {get, 2}]},
     {function, 6, get, 0, [{clause, 6, [], [], [erl_parse:abstract(D, 6)]}]},
     {function, 8, get, 1,
      [{clause, 8, [{var, 8, 'K'}], [],
        [{'try', 9,
          [{call, 10, {remote, 10, {atom, 10, maps}, {atom, 10, get}}, [{var, 10, 'K'}, erl_parse:abstract(D, 10)]}],
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
          [{var, 15, 'K'}, erl_parse:abstract(D, 15), {var, 15, 'D'}]}]}]},
     {eof, 16}].

-spec load(M::module(), B::binary()) -> {module, module()} | {error, badarg | code:load_error_rsn()}.
load(M, B) ->
    code:purge(M),
    code:load_binary(M, atom_to_list(M) ++ ".erl", B).
