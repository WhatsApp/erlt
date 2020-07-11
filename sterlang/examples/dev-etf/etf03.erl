-lang([erl2, st]).

-module(etf03).

-export([
    int_literal/0,
    float_literal/0,
    char_literal/0,
    string_literal1/0,
    string_literal2/0,
    true_literal/0,
    false_literal/0,
    int_literal/1,
    float_literal/1,
    string_literal1/1,
    string_literal2/1,
    true_literal/1,
    false_literal/1
]).

-spec int_literal() -> integer().
int_literal() -> 0.

-spec int_literal(integer()) -> boolean().
int_literal(1) -> true.

% NB. - stErlang uses integers everywhere for now!
-spec float_literal() -> integer().
float_literal() -> 0.1.

-spec float_literal(integer()) -> boolean().
float_literal(0.1) -> true.

-spec char_literal() -> char().
char_literal() -> $c.

%%  TODO
%%char_literal($c) -> true.

-spec string_literal1() -> string().
string_literal1() -> "".

-spec string_literal1(string()) -> boolean().
string_literal1("") -> true.

-spec string_literal2() -> string().
string_literal2() -> "another string".

-spec string_literal2(string()) -> boolean().
string_literal2("another string") -> true.

-spec true_literal() -> boolean().
true_literal() -> true.

-spec true_literal(boolean()) -> boolean().
true_literal(true) -> true.

-spec false_literal() -> boolean().
false_literal() -> false.

-spec false_literal(boolean()) -> boolean().
false_literal(false) -> false.
