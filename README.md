beamcache: Erlang BEAM Cache
============================

Usage
-----

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
-export(['$handle_undefined_function'/2]).

'$handle_undefined_function'(F, A) ->
    {module, ?MODULE} = beamcache:init(?MODULE, #{a => 1, b => true, <<"key">> => "TEST"}),
    apply(?MODULE, F, A).
```
