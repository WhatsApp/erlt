-file("dev/src/exn.erlt", 1).

-module(exn).

-unchecked([{mk_error1, 0}]).

-export([mk_error1/0, mk_error2/0]).

-export_type([my_error/0]).

-type error1() :: {'$#exn:error1'}.

-type error2() :: {'$#exn:error2', string()}.

-type my_error() :: {'$#exn:my_error.my_error', any()}.

mk_error1() -> {'$#exn:error1'}.

-spec mk_error2() -> any().

mk_error2() -> {'$#exn:error2', "bad error"}.



