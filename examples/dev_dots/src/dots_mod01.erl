%% Copyright (c) Facebook, Inc. and its affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-lang([erl2]).
-module(dots_mod01).

-export([f/0, g/0, p/1, q/1, r/1, s/1, t/2, u/2, v/1]).

-type foo() :: ab.bc.cd | de.ef.

%% types may use dots as qualifier, just like function calls
-type bar() :: a.b(). % type b in module a
-type baz() :: xx.y(boolean(), integer()). % type y in module zz
-type mbar() :: c:def().
-type mbaz() :: yy:z(boolean(), integer()).

-export_type([foo/0,bar/0,baz/0,mbar/0,mbaz/0]).

%% terms in wild attributes may have dots
-xyz(fee.fie.foo.fum).


-spec f() -> any().
f() ->
    %% dotted atoms as plain values
    {ok, abc.d.efg}.

-spec g() -> any().
g() ->
    %% generic atom concatenation
    'Foo'.'x.y'.'$'.


-spec p(list()) -> list().
p(X) ->
    %% dotted module name with colon before function name
    erlang.lists:reverse(X).

-spec q(list()) -> list().
q(X) ->
    %% dotted name is allowed to the right of colon but has no special meaning
    %% (only because M:F(...) is a generic expression, so `M:a.b(...)` should
    %% be the same thing as `M:(a.b)(...)`, i.e. `M:'a.b'(...)`)
    lists:reverse.me(X).

-spec r(list()) -> list().
r(X) ->
    %% dot instead of colon with a non-dotted module name
    lists.reverse(X).

-spec s(list()) -> list().
s(X) ->
    %% dot instead of colon with a dotted module name
    erlang.lists.reverse(X).

%% prefix dots are allowed in function call context
-spec t(list(), list()) -> list().
t(X, [_]=Y) ->
    .lists.append(X, Y);
t(X, Y) ->
    .lists:append(X, Y).

%% dotted atoms in function heads and match patterns
u(X, foo.bar) ->
    {ok, abc.de.fg, _} = X.

v(X) ->
    %% dotted atoms in case-like patterns and catch clause patterns
    try lists:reverse(X) of
        {ok, abc.d.ef.g} -> ok
    catch
        exit, foo.bar.baz, Trace -> {caught, Trace}
    end.
