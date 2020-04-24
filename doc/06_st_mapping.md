# 6. ST mapping

ST part of the language is type-checked. In the current implementation it
happens via translation of ST code into OCaml code and then invoking `ocamlc`.

Since the OCaml semantics it well-known, the most concise way to describe the
ST semantics is by describing how ST code maps onto OCaml.
(the mapping implementation is
https://github.com/WhatsApp/erl2/blob/master/erl2c/src/erl2ocaml.erl)

(During translation, Erlang names are mangled a little bit, in the current
document the examples are presented without mangling).

# Types

See https://erlang.org/doc/reference_manual/typespec.html for Erlang types.

## Type aliases


ST type aliases are mapped on OCaml type aliases.

```erlang
%% Erlang
-type integer_alias() :: integer().
```

```ocaml
(* OCaml *)
type integer_alias = int
```

## Built-in Erlang types

### Mapping of basic types - 1

The following basic Erlang types are mapped onto basic OCaml types.
The only subtle point is that float type and integer types are both mapped onto
OCaml `int` type.

```erlang
%% Erlang
-type float_alias() :: float().
-type some_fun(A, B) :: fun((A) -> B).
-type list_alias(A) :: [A].
-type tuple2_alias(A, B) :: {A, B}.
-type tuple3_alias(A, B, C) :: {A, B, C}.
-type string_alias() :: string().
-type integer_alias() :: integer().
-type char_alias() :: char().
-type boolean_alias() :: boolean().
```

```ocaml
(* OCaml *)
type float_alias = int
type ('A, 'B) some_fun = 'A -> 'B
type 'A list_alias = 'A list
type ('A, 'B) tuple2_alias = 'A * 'B
type ('A, 'B, 'C) tuple3_alias = 'A * 'B * 'C
type string_alias = string
type integer_alias = int
type char_alias = char
type boolean_alias = bool
```

### Mapping of basic types - 2

The following Erlang types do not have "native" representation in OCaml.
They are mapped "opaquely". This means that they are represented by some
abstract OCaml types.


```erlang
%% Erlang
-type atom_alias() :: atom().
-type pid_alias() :: pid().
-type port_alias() :: port().
-type reference_alias() :: reference().
-type neg_integer_alias() :: neg_integer().
-type non_neg_integer_alias() :: non_neg_integer().
-type pos_integer_alias() :: pos_integer().
-type any_alias() :: any().
-type none_alias() :: none().
-type term_alias() :: term().
-type binary_alias() :: binary().
-type bitstring_alias() :: bitstring().
-type byte_alias() :: byte().
-type number_alias() :: number().
-type iodata_alias() :: iodata().
-type iolist_alias() :: iolist().
-type identifier_alias() :: identifier().
-type node_alias() :: node().
-type timeout_alias() :: timeout().
-type no_return_alias() :: no_return().
```

```ocaml
(* OCaml *)
type atom_alias = Ffi.atom
type pid_alias = Ffi.pid
type port_alias = Ffi.port
type reference_alias = Ffi.reference
type neg_integer_alias = Ffi.neg_integer
type non_neg_integer_alias = Ffi.non_neg_integer
type pos_integer_alias = Ffi.pos_integer
type any_alias = Ffi.any
type none_alias = Ffi.none
type term_alias = Ffi.term
type binary_alias = Ffi.binary
type bitstring_alias = Ffi.bitstring
type byte_alias = Ffi.byte
type number_alias = Ffi.number
type iodata_alias = Ffi.iodata
type iolist_alias = Ffi.iolist
type identifier_alias = Ffi.identifier
type node_alias = Ffi.node
type timeout_alias = Ffi.timeout
type no_return_alias = Ffi.no_return
```

`Ffi.atom`, `Ffi.pid`, ... are just abstract types in a special (secret) OCaml
module.

### "Single" and empty tuples

An empty Erlang tuple is mapped onto OCaml `unit`. OCaml doesn't have tuples
with a single element, so such tuples are translated into a special transparent
type:

```erlang
(* Erlang *)
-type tuple0_alias() :: {}.
-type tuple1_alias(A) :: {A}.
```

```ocaml
(* OCaml *)
type tuple0_alias = unit
type 'A tuple1_alias = 'A Ffi.tuple1
```

In ffi.mli:

```
type 'a tuple1 = Tuple1 of 'a
```

### Homogeneous maps

Erlang maps with "optional" associations `#{K => V}` are interpreted as
**homogeneous maps**.

```erlang
%% Erlang
-type map_alias(A, B) :: #{A => B}.
-type map_string_int() :: #{string() => integer()}.
```

```ocaml
(* OCaml *)
type ('A, 'B) map_alias = ('A, 'B) Ffi.map
type map_string_int = (string, int) Ffi.map
```

It means that ``#{...}`` can have only one optional association.
`#{A => B, B => A}` is incorrect.

### Polymorphic records

Erlang maps with mandatory associations and atomic keys are interpreted as
polymorphic records. More details about polymorphic records are in
[Polymorphic records](05_polymorphic_records.md).

They are translated into OCaml objects with corresponding get and set methods.


```erlang
%% Erlang
-type rec_with_int_id() :: #{id := integer()}.
-type rec_with_generic_id(A) :: #{id := A}.
-type date() :: #{year := integer(), month := string(), day := integer()}.
```

```ocaml
(* OCaml *)
type rec_with_int_id =
  < get_id : int
  ; set_id : int -> 'rec_tv__1 >
  as
  'rec_tv__1

type 'A rec_with_generic_id =
  < get_id : 'A
  ; set_id : 'A -> 'rec_tv__2 >
  as 'rec_tv__2

type date =
  < get_year : int
  ; set_year : int -> 'rec_tv__3
  ; get_month : string
  ; set_month : string -> 'rec_tv__3
  ; get_day : int
  ; set_day : int -> 'rec_tv__3 >
  as
  'rec_tv__3
```

### **Enums**

More details about enums are in [Enum syntax and semantics](04_enums.md).
Enums are mapped onto OCaml variants.

```erlang
%% Erlang
-enum either(A, B) :: left{A} | right {B}.
```

```ocaml
(* OCaml *)
type ('A, 'B) either = Left of 'A | Right of 'A
```

# Expressions

Mapping of expressions is mostly 1:1 in the majority of cases:

* Integer literals are mapped onto integer literals
* Float literals are mapped onto integer literals as well
* Number (unary and binary) operations are mapped onto corresponding *integer*
  operations in OCaml.
* Char literals → char literals
* Boolean operations → Boolean operations
* ...
* Assignment `X = Expr` -> `let x = expr in ...`
* Erlang patterns are mapped into corresponding OCaml patterns

## Mapping of enums

Mapping of enums is straightforward.

Example

```erlang
%% Erlang

-enum option(A) :: none{} | some{A}.

-spec option_to_list(option(A)) -> [A].
option_to_list(option.none{}) -> [];
option_to_list(option.some{A}) -> [A].
```

```ocaml
(* Ocaml *)
type 'A option = None | Some of 'A

let option_to_list : 'A option -> 'A list = function
  | None -> []
  | Some v_A -> [ v_A ]
```

## Mapping of polymorphic records

OCaml doesn't have polymorphic records in the first place. Polymorphic records
are "modelled" to some extent via
[OCaml objects](https://dev.realworldocaml.org/objects.html).

**Creating an empty record**

```erlang
%% Erlang
rec1() ->
    #{}.
```

```ocaml
(* Ocaml *)
let rec1 = function () -> object end
```

**Creating a more interesting record**

```erlang
%% Erlang
rec2() ->
    #{year => 2020, month => "April", day => 17}.
```

```ocaml
(* OCaml *)
let rec2 = function
  | () ->
      object
        val val_year = 2020
        method get_year = val_year
        method set_year new_val_year = {<val_year = new_val_year>}

        val val_month = "April"
        method get_month = val_month
        method set_month new_val_month = {<val_month = new_val_month>}

        val val_day = 17
        method get_day = val_day
        method set_day new_val_day = {<val_day = new_val_day>}
      end
```

**Accessing fields**

```erlang
%% Erlang
rec3(Rec) ->
    Rec.id.
```

```ocaml
(* OCaml *)
let rec3 = function v_rec -> v_rec#get_id
```

**Updating**

```erlang
%% Erlang
rec4(Rec) ->
    Rec#{year := 2046}.
```

```ocaml
(* OCaml *)
let rec rec4 = function
  | v_Rec ->
      let _ = v_Rec = v_Rec#set_year 2046 in
      let _ = 2046 = v_Rec#get_year in
      v_Rec#set_year 2046
```

As you can see - for updates some extra code is generated - just to record our
requirement that the old and new values of the field should be of the same type.

**Specifying types in specs**

"Closed record"

```erlang
%% Erlang
-spec rec5(#{id := A}) -> A.
rec5(Rec) ->
    Rec.id.
-spec rec6(#{year := integer()}) -> #{year := integer()}.
rec6(Rec) ->
    Rec#{year := 2046}.

```

```ocaml
(* OCaml *)
let rec rec5 :
    (< get_id : 'A ; set_id : 'A -> 'rec_tv__1 > as 'rec_tv__1) -> 'A = function
  | v_Rec -> v_Rec#get_id

let rec rec6 :
    (< get_year : int ; set_year : int -> 'rec_tv__2 > as 'rec_tv__2) ->
    (< get_year : int ; set_year : int -> 'rec_tv__3 > as 'rec_tv__3) = function
  | v_Rec ->
      let _ = v_Rec = v_Rec#set_year 2046 in
      let _ = 2046 = v_Rec#get_year in
      v_Rec#set_year 2046  
```

As you can see - modelling or records via objects is quite verbose.

There is also some support to specify the type of an "open" record:

```erlang
%% Erlang
-spec rec7(#{id := A, _ := _}) -> A.
rec7(Rec) ->
    Rec.id.
```

```ocaml
(* OCaml *)
let rec rec7 :
    (< get_id : 'A ; set_id : 'A -> 'rec_tv__4 ; .. > as 'rec_tv__4) -> 'A =
  function
  | v_Rec -> v_Rec#get_id
```

Unfortunately, in the current scheme you can only express that a particular
record is open (via magic of `_ := _`). But you cannot express equality between
types of two open records.

This is not expressible:

```erlang
%% Erlang
-spec rec8(#{year := integer(), _ := _}) -> #{year := integer(), _ := _}.
rec8(Rec) ->
    Rec#{year := 2046}.
```

# Modules

**TL;DR** - Modules are type-checked against each other with respect to to their
public API (public specs). - That is: calling a non-existent function from
another module is a compile-time error. Also calling a private function from
another module is a compile-time error. Opaqueness of types is translated
properly.
