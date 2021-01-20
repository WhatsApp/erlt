# eqwalizer

A type-checker for Erlang

## WIP status

Eqwalizer is work in progress.
Right now it handles a subset of Erlang in a very simplistic way.

## Testing and experimenting

Prerequisites: `sbt` and `rebar`.

### Testing 

```
sbt test
```

### Experimenting

The current development setup allows experimenting with Erlang code inside 
`test_projects`. `test_projects` folder is used for integration tests
(snapshot testing), and it has two rebar subprojects: `check` - for snapshot
testing of how the type-checker behaves on different simple examples, `wip` - 
to keep an eye on stuff which is not fully implemented yet. If you would like 
to play with eqwalizer - the simplest way would be to add an Erlang module in
`test_projects/check/src` and then invoke the type-checker. 

Example:

`test_projects/check/src/foo.erl`

```erlang
-module(foo).

-spec bar(number()) -> atom().
bar(X) -> X.
```  

```
% sbt
[info] welcome to sbt 1.4.6 (AdoptOpenJDK Java 11.0.9.1)
...
[info] started sbt server
sbt:eqwalizer> test:run check foo
[info] test_projects / rebar3 compile
...
[info] running com.whatsapp.eqwalizer.test.Main check foo
Loading forms from test_projects/_build/default/lib/check/ebin/foo.beam
  
  1 -module(foo).                              |         |
  2                                            |         |
  3 -spec bar(number()) -> atom().             |         |
  4 bar(X) -> X.                               | ERROR   | X. Expected: atom(), Got: number()
```

So, to check a module `foo` from sbt console:

```
sbt:eqwalizer> test:run check foo
```

From command line:

```
sbt 'test:run check foo'
```

#### Debugging

Eqwalizer doesn't support full set of Erlang expressions yet.
It also has some other restrictions for now: for example, - it doesn't work 
(yet) with recursive data structures defined vi `type()`s. In this case if 
a function uses something that is not supported yet, it will be skipped.

An example:

```erlang
-module(foo).

-export([list_concat_opt/2]).

-spec
list_concat_opt([A], [B]) -> [A | B].
list_concat_opt(X, []) -> X;
list_concat_opt(X, Y) -> X ++ Y.
```

```
sbt:eqwalizer> test:run check foo
...
Loading forms from test_projects/_build/default/lib/check/ebin/foo.beam
  1 -module(foo).                              |         |
  2                                            |         |
  3 -export([list_concat_opt/2]).              |         |
  4                                            |         |
  5 -spec                                      |         |
  6 list_concat_opt([A], [B]) -> [A | B].      |         |
  7 list_concat_opt(X, []) -> X;               | SKIP    |
  8 list_concat_opt(X, Y) -> X ++ Y.           |         |
```

You can get more info about why it was skipped by running `test:run debug foo`:

```
sbt:eqwalizer> test:run debug foo
...
Loading forms from test_projects/_build/default/lib/check/ebin/foo.beam
  1 -module(foo).                            | LOADED  |                                |
  2                                          |         |                                |
  3 -export([list_concat_opt/2]).            | LOADED  |                                |
  4                                          |         |                                |
  5 -spec                                    | LOADED  |                                |
  6 list_concat_opt([A], [B]) -> [A | B].    |         |                                |
  7 list_concat_opt(X, []) -> X;             | SKIPPED |                                |
  8 list_concat_opt(X, Y) -> X ++ Y.         |         | E: _ ++ _                      |
```

It indicates that the binary operation of list concatenation (`++`) is not
supported yet.

### Under the hood

Eqwalizer uses beam files with debug info to get Erlang AST 
([abstract forms](https://erlang.org/doc/apps/erts/absform.html)). - This is 
much simpler than processing source files since all header files are already
preprocessed and macros are expanded.

The downside is that debug info has only line locations, it doesn't provide 
precise ranges of all the expressions, so eqwalizer is not able to highlight
an erroneous expression exactly. Instead, it puts some diagnostics on the same
line, prints the erroneous expression, and some information about type mismatches.
In the above example, it complains about the variable `X`, saying that it has 
the type `number()`, but it is expected to be of the type `atom()` according to 
the provided spec.

## Semantics of type-checking

Eqwalizer checks Erlang function [specifications](http://erlang.org/doc/reference_manual/typespec.html).

### The goal

The goal is to ensure that the code doesn't violate contracts expressed via 
function specifications.

### What does it mean to break a contract?

A function specification forms a contract: it states that it expects to receive
arguments of certain types, and it states that it would return 
a value of a certain type (if it completes normally). 

There are two cases for breaking a contract:

(1) A function returns a value which contradicts the provided spec. In the 
above example the function `bar` promises to return an atom, but returns 
a number, this is breaking the contract.

(2) A provided argument for a function is not of the type that the function 
expects.

An example:

```
  6 -spec double_me(atom()) -> atom().         |         |
  7 double_me(A) ->                            | ERROR   |
  8     my_plus(A, A),                         |         | A. Expected: number(), Got: atom()
  9     ok.                                    |         |
 10                                            |         |
 11 -spec my_plus                              |         |
 12     (number(), number()) -> number().      |         |
 13 my_plus(X, Y) -> X + Y.                    | OK      |
 ``` 

The variable `A` is of the type `atom()`, the first argument of `my_plus/2` 
should be a `number()`, - so passing `A` violates the contract of using `my_plus/2`.

### Non-goals

At the current stage of development ensuring the correct usage of specs/contracts
is the main focus.

Preventing from other errors is a NON-GOAL for now.

Such errors include

**Detecting probably dead code**

This is OK code for eqwalizer:

```erlang
-spec bar(atom() | number()) ->
    atom | number.
bar(A) when is_atom(A) -> atom;
bar(N) when is_number(N) -> number;
bar(A) when is_atom(A) -> atom.
```  

While the third clause can never be matched, - eqwalizer says nothing about it,
since it well-typed.

**Detecting missing patterns**

(Or exhaustivity checks)

Similar case is OK for eqwalizer:

```erlang
-spec bar(atom() | number()) ->
    atom | number.
bar(A) when is_atom(A) -> atom.
```  

While the case when the argument is a number is not handled eqwalizer doesn't
treat this as an error.
