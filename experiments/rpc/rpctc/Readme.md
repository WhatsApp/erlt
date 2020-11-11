## RPCTC

How many letters can we cram into our acronym without it becoming ridiculous.
The name is not important right now, lets focus on choosing and coming up with alternatives and revisit the name when we have made choice.

RPCTC generates code from a hacked in `-rpc` syntax in `erlt` files.

## RPC Syntax

We wanted a minimal change, that would allow a user to specify a service, which we can then use to experiment with different code generation models.
**Syntax is not the focus of the project at this stage**, it is simply something that is easy to document.
It looks exactly like callbacks, except with the rpc keyword:

```erlang
-rpc order_cat(string()) -> string(). %% call
-rpc return_cat(string()) -> no_return(). %% no_return() => cast
-rpc name() -> service_name. %% This is how to specify the service name.
-rpc singleton() -> true. %% Specify that ?MODULE is the name.
```

Here is the minimal change to the erlt parser `erltc/src/erlt_parse.yrl`:

```diff
-'spec' 'callback' struct_like type_like % helper
+'spec' 'rpc' 'callback' struct_like type_like % helper

+attribute -> '-' 'rpc' type_spec            : ?set_anno(build_type_spec('$2', '$3'), ?anno('$1','$3')).

--type spec_attr() :: 'callback' | 'spec'.
+-type spec_attr() :: 'callback' | 'spec' | 'rpc'.

+parse_form([{'-', A1}, {atom, A2, rpc} | Tokens]) ->
+    NewTokens = [{'-', A1}, {'rpc', A2} | Tokens],
+    ?ANNO_CHECK(NewTokens),
+    parse(NewTokens);

-build_type_spec({Kind, Aa}, {type_spec, _TA, SpecFun, TypeSpecs}) when Kind =:= spec; Kind =:= callback ->
+build_type_spec({Kind, Aa}, {type_spec, _TA, SpecFun, TypeSpecs}) when Kind =:= spec; Kind =:= callback; Kind =:= rpc ->

+modify_anno1({attribute, A, rpc, {Fun, Types}}, Ac, Mf) ->
+    {A1, Ac1} = Mf(A, Ac),
+    {Types1, Ac2} = modify_anno1(Types, Ac1, Mf),
+    {{attribute, A1, rpc, {Fun, Types1}}, Ac2};
```

## How to run

RPCTC takes a mode and an input file to parse and outputs to standard out the generate file, which you can then pipe to the appropriate location:

```
$ rpctc <mode> <input file> > <output file>
```

Currently there are two modes:

  - [header](./src/header.erl), see [count_headert](../count_headert) for an example
  - [impl](./src/impl.erl), see [count_implt](../count_implt) for an example

## Future Work

  - Timeout and hibernation handling
  - Handling continues, where the server schedules some work to be handled later
  - Linking and/or monitoring of the server, this proposal just uses linking
  - Asynchronous client API, e.g. the ability to get back a fun which can be evaluated in order to wait for the result.
  - Multicalls
  - Out-of-band replies, e.g. using gen_server:reply to answer a call, potentially from a different process.
  - Backwards and forwards compatibility support
  - Clarifying semantics of local calls/casts vs calls to another Erlang node
  - Hot code update support
  - Handle info, generic messages sent to the server.
  - Different return conventions (reply right away, reply later and noreply)
  - timer (reply type is overloaded by arity)
  - terminate
  - Migration:
    * Can this translation be mechanical
    * Is there a multi step approach, type some parts and then migrate the rest.
      For Example split dispatch and callbacks, type callbacks, leave dispatch untyped and then refactor the middle.
  - Generate some extra attributes to make IDE click through easier