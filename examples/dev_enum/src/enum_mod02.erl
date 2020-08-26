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

%% tests for remote constructors
-lang([erl2]).
-module(enum_mod02).

-export([p/0, q/1, r/1, s/1]).

%% alias for remote type using dot-qualified name
-type possibly(T) :: enum_mod01.possibly(T).

%% local type using remote constructors
-type perhaps(T) :: enum_mod01.possibly.some{T} | enum_mod01.possibly.none{}.


-spec p() -> possibly(any()).

p() ->
    %% only dots can be used for constructor module qualifiers, not colon
    enum_mod01.possibly.none{}.


-spec q(T) -> perhaps(T).

q(X) ->
    enum_mod01.possibly.some{X}.


-spec r(perhaps(T)) -> [T].

%% remote constructors in patterns
r(enum_mod01.possibly.some{X=41}) ->
    X+1;
r(E) ->
    case E of
        enum_mod01.possibly.none{} ->
            [];
        enum_mod01.possibly.some{X} ->
            [X]
    end.

-spec s(perhaps(T)) -> [T].

s(E) ->
    %% remote constructors in try-patterns
    try E of
        enum_mod01.possibly.none{} ->
            [];
        enum_mod01.possibly.some{X} ->
            [X]
    catch
        throw, enum_mod01.possibly.some{thing}, Trace ->
            {caught, Trace}
    end.
