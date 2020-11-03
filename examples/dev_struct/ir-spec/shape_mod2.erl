-file("dev_struct/src/shape_mod2.erlt", 1).

-module(shape_mod2).

-export([shape_var_unused_f/2,
         shape_var_used_f/2,
         misc/5]).

-export_type([shape_var_unused_o/2,
              shape_var_used_o/2,
              shape_var_unused_s/3,
              shape_var_used_s/2,
              shape_var_unused_e/2,
              shape_var_used_e/2,
              conflict/2]).

-opaque shape_var_unused_o(A,
                           __ERLT_SHAPE_VAR_R) :: #{id := A, atom() => any()}.

-opaque shape_var_used_o(A, __ERLT_SHAPE_VAR_R) :: {A,
                                                    #{id := A,
                                                      atom() => any()}}.

-spec shape_var_unused_f(A, R) -> #{id := A,
                                    atom() => any()} when R :: #{atom() =>
                                                                     any()}.

shape_var_unused_f(A, R) -> R#{id => A}.

-spec shape_var_used_f(A, R) -> {R,
                                 #{id := A, atom() => any()}} when R :: #{atom()
                                                                              =>
                                                                              any()}.

shape_var_used_f(A, R) -> {R, R#{id => A}}.

-type shape_var_unused_e(A,
                         __ERLT_SHAPE_VAR_R) :: {'$#shape_mod2:shape_var_unused_e.shape',
                                                 #{id := A, atom() => any()}}.

-type shape_var_used_e(A,
                       R) :: {'$#shape_mod2:shape_var_used_e.shape',
                              #{id := A, atom() => any()}} |
                             {'$#shape_mod2:shape_var_used_e.var', R}.

-type shape_var_unused_s(A1, A2,
                         __ERLT_SHAPE_VAR_R) :: {'$#shape_mod2:shape_var_unused_s',
                                                 A1,
                                                 #{id := A2, atom() => any()}}.

-type shape_var_used_s(A,
                       R) :: {'$#shape_mod2:shape_var_used_s',
                              R,
                              #{id := A, atom() => any()}}.

-spec misc(A1, R1, A2, R2, R3) -> {R1,
                                   #{id := A1, atom() => any()},
                                   R2,
                                   #{id := A2, atom() => any()},
                                   #{id := A2, atom() => any()}} when R1 ::
                                                                          #{atom()
                                                                                =>
                                                                                any()},
                                                                      R2 ::
                                                                          #{atom()
                                                                                =>
                                                                                any()},
                                                                      R3 ::
                                                                          #{atom()
                                                                                =>
                                                                                any()}.

misc(A1, R1, A2, R2, R3) ->
    {R1, R1#{id => 1}, R2, R2#{id => 2}, R3#{id => 3}}.

-type conflict(__ERLT_SHAPE_VAR_A,
               ___ERLT_SHAPE_VAR_A) :: #{id := __ERLT_SHAPE_VAR_A,
                                         atom() => any()}.



