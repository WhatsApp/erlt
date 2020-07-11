-lang([erl2, st]).

-module(etf07).

-export([get/1, put/2, get2/1, put2/2]).

-enum option(A) :: some{A} | none{}.

get(option.some{A}, _) -> A;
get(option.none{}, Default) -> Default.

put(false, _) -> option.none{};
put(true, X) -> option.some{X}.

get2(maybe.maybe.just{A}, _) -> A;
get2(maybe.maybe.nothing{}, Default) -> Default.

put2(false, _) -> maybe.maybe.nothing{};
put2(true, X) -> maybe.maybe.just{A}.
