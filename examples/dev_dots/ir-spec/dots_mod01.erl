-file("dev_dots/src/dots_mod01.erlt", 1).

-module(dots_mod01).

-export([f/0, g/0, p/1, q/1, r/1, s/1, t/2, u/2, v/1]).

-type foo() :: 'ab.bc.cd' | 'de.ef'.

-type bar() :: a:b().

-type baz() :: xx:y(boolean(), integer()).

-type mbar() :: c:def().

-type mbaz() :: yy:z(boolean(), integer()).

-export_type([foo/0, bar/0, baz/0, mbar/0, mbaz/0]).

-xyz('fee.fie.foo.fum').

-spec f() -> any().

f() -> {ok, 'abc.d.efg'}.

-spec g() -> any().

g() -> 'Foo.x.y.$'.

-spec p(list()) -> list().

p(X) -> 'erlang.lists':reverse(X).

-spec q(list()) -> list().

q(X) -> lists:'reverse.me'(X).

-spec r(list()) -> list().

r(X) -> lists:reverse(X).

-spec s(list()) -> list().

s(X) -> 'erlang.lists':reverse(X).

-spec t(list(), list()) -> list().

t(X, [_] = Y) -> lists:append(X, Y);
t(X, Y) -> lists:append(X, Y).

u(X, 'foo.bar') -> {ok, 'abc.de.fg', _} = X.

v(X) ->
    try lists:reverse(X) of
        {ok, 'abc.d.ef.g'} -> ok
    catch
        exit:'foo.bar.baz':Trace -> {caught, Trace}
    end.



