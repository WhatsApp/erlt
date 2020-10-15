-file("dev/src/mod03.erlt", 1).

-module(mod03).

-export_type([unit0/0,
              boxed/1,
              either/2,
              rgb/0,
              pair/2,
              triple/3,
              my_list/1,
              option/1]).

-export([mk_rgb/0,
         mk_triple/1,
         mk_none/0,
         mk_unit/0,
         mk_box/1,
         mk_left/1,
         mk_right/1,
         zero/2,
         unbox/1,
         un_either/1,
         un_pair/1,
         first/1,
         second/1,
         call_this_mod/1]).

-type unit0() :: {969696, mod03, unit0, unit0}.

-type boxed(A) :: {969696, mod03, boxed, boxed, A}.

-type either(A, B) :: {969696, mod03, either, left, A} |
                      {969696, mod03, either, right, B}.

-type rgb() :: {969696, mod03, rgb, r} |
               {969696, mod03, rgb, g} |
               {969696, mod03, rgb, b}.

-type pair(A, B) :: {969696, mod03, pair, pair, A, B}.

-type triple(A, B, C) :: {969696,
                          mod03,
                          triple,
                          triple,
                          A,
                          B,
                          C}.

-type my_list(A) :: {969696,
                     mod03,
                     my_list,
                     cons,
                     A,
                     my_list(A)} |
                    {969696, mod03, my_list, nil}.

-type option(A) :: {969696, mod03, option, none} |
                   {969696, mod03, option, some, A}.

-spec mk_rgb() -> rgb().

mk_rgb() -> {969696, mod03, rgb, r}.

-spec mk_triple(A) -> triple(A, A, A).

mk_triple(A) ->
    {969696, mod03, triple, triple, A, A, A}.

-spec mk_none() -> option(_).

mk_none() -> {969696, mod03, option, none}.

-spec mk_unit() -> unit0().

mk_unit() -> {969696, mod03, unit0, unit0}.

-spec mk_box(A) -> boxed(A).

mk_box(A) -> {969696, mod03, boxed, boxed, A}.

-spec mk_left(A) -> either(A, _).

mk_left(A) -> {969696, mod03, either, left, A}.

-spec mk_right(B) -> either(_, B).

mk_right(B) -> {969696, mod03, either, right, B}.

-spec zero(unit0(), V) -> V.

zero({969696, mod03, unit0, unit0}, Val) -> Val.

-spec unbox(boxed(E)) -> E.

unbox(Boxed) ->
    case Boxed of
        {969696, mod03, boxed, boxed, Elem} -> Elem
    end.

-spec un_either(either(A, A)) -> A.

un_either(Either) ->
    case Either of
        {969696, mod03, either, left, Elem} -> Elem;
        {969696, mod03, either, right, Elem} -> Elem
    end.

-spec un_pair(pair(A, B)) -> {A, B}.

un_pair(Pair) ->
    case Pair of
        {969696, mod03, pair, pair, A, B} -> {A, B}
    end.

-spec first(pair(A, _)) -> A.

first({969696, mod03, pair, pair, F, _}) -> F.

-spec second(pair(_, B)) -> B.

second({969696, mod03, pair, pair, _, S}) -> S.

-spec call_this_mod(pair(A, B)) -> pair(A, B).

call_this_mod(P) ->
    case P of
        {969696, mod03, pair, pair, A, B} ->
            {969696, mod03, pair, pair, A, B}
    end.


