[![Build Status](https://github.com/Ledest/beamcache/actions/workflows/erlang.yml/badge.svg)](https://github.com/Ledest/beamcache/actions/workflows/erlang.yml/badge.svg)

# beamcache: Erlang BEAM Cache

## Usage

```erlang
CacheName = my_cache,
Cache = #{a => 1, b => true, <<"key">> => "TEST"},
{module, CacheName} = beamcache:init(CacheName, Cache),
Cache = CacheName:get(),
true = CacheName:get(b),
false = CacheName:get(c, false).
```

To shut up xref and dialyzer add such module to sources:

```erlang
-module(testbc).
-export([get/0, get/1, get/2]).

get() -> erlang:nif_error(undef).
get(_) -> erlang:nif_error(undef).
get(_, _) -> erlang:nif_error(undef).
erase(_) -> erlang:nif_error(undef).
put(_, _) -> erlang:nif_error(undef).
```

To protect against using uninitialized cache add such module to sources:

```erlang
-module(testbc).
-export(['$handle_undefined_function'/2]).

'$handle_undefined_function'(F, A) ->
    {module, ?MODULE} = beamcache:init(?MODULE, #{a => 1, b => true, <<"key">> => "TEST"}),
    apply(?MODULE, F, A).
```

Or come up with a combination of the above examples...
