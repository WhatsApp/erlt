    Author: Max Heiber
    Status: Draft
    Type: Standards Track
    Created: 03-Jan-2021
    Erlang-Version: OTP-24
    Post-History:
    Replaces: 2, 3
****
EEP XXX: Type Hint Expressions
----


- [ ] only allow upcasts? How do we do unsafe stuff?
- [ ] less-awkward syntax for funs?


Abstract
========

This EEP proposes expression-level syntax for specifying the type of an expression. This syntax will be completely ignored by the Erlang Runtime System but will be helpful for tools (such as Dialyzer and Gradualizer) that pay attention to static types.

Potential syntax for a type hint expression:

```
( Expression :: Type )
```

For example:

```erl
(X :: number()),
```

The above has the same runtime semantics as:

```erl
X
```

The suggested interpretation of this syntax for static analysis tools is to give the type hint expression type `Type`, regardless of the inferred type of `Expression`.

Rationale
=========

Erlang functions and expressions already have types, but the language currently only gives users the ability to describe the types of functions.

One use case is intentionally widening the return type of a function.

Dialyzer currently rejects code in the pattern match below, because regardless of the value of the ?ENV macro, at least one of the cases won't match:

```erl
environment() ->
    ?ENV.

main() ->
    case environment() of
        dev -> prod_stuff();
        prod -> dev_stuff()
    end.
```

A type hint expression resolves the problem:

```erl
environment() ->
    (?ENV :: dev | prod).
```

Widening return types enables developers to future-proof their APIs. Dialyzer currently rejects the following code, which can similarly be fixed with a type hint expression:

```erl
-spec mascot() -> atom().
mascot() ->
    case os() of
        linux -> penguin;
        mac -> dolphin;
        Other -> handle_other(Other)
    end.

os() ->
    [linux, mac].
```


Type hint expressions enable typing funs:

```erl
-type num_num() :: fun ((number()) -> number()).

main(_Args) ->
    F = (fun (X) -> X end :: num_num()),
    ....
```

Copyright
=========

This document has been placed in the public domain.


[EmacsVar]: <> "Local Variables:"
[EmacsVar]: <> "mode: indented-text"
[EmacsVar]: <> "indent-tabs-mode: nil"
[EmacsVar]: <> "sentence-end-double-space: t"
[EmacsVar]: <> "fill-column: 70"
[EmacsVar]: <> "coding: utf-8"
[EmacsVar]: <> "End:"
[VimVar]: <> " vim: set fileencoding=utf-8 expandtab shiftwidth=4 softtabstop=4: "
