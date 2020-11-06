(*
Modeling type-safe Erlang exceptions via OCaml's extensible GADTs.

Here, we use GADTs to model exceptions dependent on throw|error|exit exception
classes

The benefit is that we get unqualified syntax for exception values, *and* safety
against respective classes.


To typecheck, run: ocamlc -c exceptions_gadt.ml


Discussion thread: https://forum.erl2.org/t/design-of-exceptions-for-erlt/88/19

How extensible variants (aka open datatypes) work in OCaml:

  https://caml.inria.fr/pub/docs/manual-ocaml/extensiblevariants.html
  https://sites.google.com/site/ocamlopen/#:~:text=Constructors%20for%20open%20types%20have,be%20Generalized%20Algebraic%20Data%20Types.
*)

(* ocaml model of built-in exception type represented as an extensible GADT *)
module ErlangException =
  struct
    type throw  (* exception classes *)
    type error
    type exit

    (* exception represented as extensible GADT -- allows to define module-scoped exceptions *)
    type _ exn = ..

    (* example of a standard/global 'error.badarg' exception *)
    type _ exn += Badard : error exn

    (* wrapper for exception with classes *)
    type t =
      | Throw of throw exn
      | Error of error exn
      | Exit of exit exn

    let throw (e: throw exn) = raise Not_found  (* dummy implementation *)
    let error (e: error exn) = raise Not_found
    let exit (e: exit exn) = raise Not_found

    (* takes a correct type safe combination of class & reason and throws it

       NOTE: this particilar may not make sense -- but the interface is valid
       NOTE: stacktrace is not modeled in this interface
    *)
    let raise (e : t) =
      match e with
        | Throw e -> throw e
        | Error e -> error e
        | Exit e -> exit e
  end


(* examples on how to use the model *)
module E = ErlangException


(* ocaml model for these ErlT definition

NOTE: here, using some imaginary ErlT syntax for exception definitions and values --
see forum thread for details: https://forum.erl2.org/t/design-of-exceptions-for-erlt/88/19

-exception error.error1.
-exception throw.throw1.
-exception throw.throw2.
*)
type _ E.exn += Error1 : E.error E.exn
type _ E.exn += Throw1 : E.throw E.exn
type _ E.exn += Throw2 : E.throw E.exn


(* this typechecks:

throw(throw1).
error(error1).

[throw1, throw2].
*)
let _ = E.throw Throw1

let _ = E.error Error1

let _ = [Throw1; Throw2]



(* extra wrapping is needed to mix exceptions of different types -- this includes raise and catch

raise(exception:error{error1}).

raise(exception:throw{throw1}).

[exception:error{error1}, exception:throw{throw1}].
*)
let _ = E.raise (E.Error Error1)

let _ = E.raise (E.Throw Throw1)

let _ = [E.Error Error1; E.Throw Throw1]


(* this doesn't typecheck even though constructors belong to the same exception enum!

throw(error1).

let _ = E.throw (Error1)

Error: This expression has type E.error E.exn = E.error ErlangException.exn
       but an expression was expected of type
         E.throw E.exn = E.throw ErlangException.exn
       Type E.error = ErlangException.error is not compatible with type
         E.throw = ErlangException.throw
*)


(* this doesn't typecheck even though constructors belong to the same exception enum!

[error1, throw1].

let _ = [Error1; Throw1]

Error: This expression has type E.throw E.exn = E.throw ErlangException.exn
       but an expression was expected of type
         E.error E.exn = E.error ErlangException.exn
       Type E.throw = ErlangException.throw is not compatible with type
         E.error = ErlangException.error
*)
