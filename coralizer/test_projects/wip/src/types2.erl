-module(types2).

-export_type([
    my_remote_number/0,
    unknown_remote_number/0,
    my_boolean_unary_fun/0
]).

-type my_remote_number() ::
    forms1:my_number().

-type unknown_remote_number() ::
    unknown:my_number().

-type my_boolean_unary_fun() ::
types1:my_unary_fun(boolean(), boolean()).
