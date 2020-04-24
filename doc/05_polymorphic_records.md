# 5. Polymorphic records

Erl1+ ST introduces a new concept - polymorphic record. It re-uses the syntax
of classic Erlang for maps, but gives it a different interpretation in
the context of the typing discipline.

## What are polymorphic records?

A record is a lightweight labeled data structure, or:
polymorphic records are maps with atomic keys.
For now records are represented as maps at runtime.

**Similar constructs in other languages**

* Elm records - https://elm-lang.org/docs/records -
  quite a good description of records
* Purescript - https://git.io/JfLsm
* Hugs/TREX - https://www.haskell.org/hugs/pages/hugsman/exts.html#sect7.2

Some background

* https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf

## Operations

### Creating records

Just use map literals:

```erlang
Date = #{year => 2020, month => "April"},
Mode = #{language => "Erl2", dialect => "st"},
Point = #{x => 1, y => 1},
```

### Access

Erl1+ introduces the "." - dot operator for accessing records.

```erlang
Date.year
Mode.dialect
```

As in Elm, the accessor can be used only for a record that has a required field.

```erlang
Date = #{year => 2020, month => "April"},
% OK
Date.year,
% Not OK
Date.language
```

### Update

The same as updating maps in Erlang.

```erlang
Point1 = Point#{x := 2},
Mode2 = #{language := "Erl2", dialect := "dt"},
```

Only existing fields can be updated (`:=` operation).
The previous and new values of the field should be of the same type.

## Types

The syntax of maps with mandatory associations is used for representing
record types in Erl1+ ST.

```erlang
-spec mk_date() -> #{year := integer(), month := string()}.
mk_date() -> #{year => 2020, month => "April"}.

-type point() :: {x := integer(), y := integer()}.

-spec get_x(point()) -> integer().
get_x(Point) -> Point.x.
```

## Extensible (open) records

It is possible to write a function which can read a particular field `x` of
any record (other fields are irrelevant).


```erlang
-spec get_x(#{x := integer(), _ := _}) -> integer().
get_x(GenericPoint) -> GenericPoint.x.

example() ->
    P1 = #{x => 1},
    P2 = #{x => 2, y => 3},
    P3 = #{x => 3, z => 4},
    Xs = [get_x(P1), get_x(P2), get_x(P3)],
    Xs.
```

To denote an open record the special magic syntax for the last field `_ := _`
is used in the current implementation.

## Current limitations of polymorphic records in Erl1+ ST

**Pattern matching of records is not supported** (for now)

The following code is not supported in Erl1+ ST.

```erlang
get_x(#{x := X, y := Y}) ->
  x.
```

**Representation of open records in the language of types is quite limiting.**

It is possible to express a generic update for a "closed" record:

```erlang
-spec update_x(#{ x: = A}, A) -> #{x := A}.
update_x(R, X) ->
    R#{ x := X }.
```

But it is not possible (for now) to express a generic update for an open record:

```erlang
%% Does not work
-spec update_x(#{ x: = A, _ := _ }, A) -> #{x := A, _ := _}.
update_x(R, X) ->
    R#{ x := X }.
```

## Notes

In DT code all mentioned constructs behave as usual (as in classic Erlang).
The only extension (syntactic sugar) is dot operation for accessing atomic
associations.

Ideally (to be consistent), map literals should use `:=`,  so that record
creation and record update are symmetrical in syntax.

Ideally:

```erlang
Point1 = #{ x := 1, y:= 2},
Point2 = Point1#{ x := 2 }.
```
