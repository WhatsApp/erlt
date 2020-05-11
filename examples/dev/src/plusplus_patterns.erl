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
-module(plusplus_patterns).

-export([standard/1, extended/1]).

standard(X) ->
    %% Standard Erlang allows "..."++R, [$c|T]++R, and [97|T]++R, where T
    %% can be another "...", [$c|T], or [97|T], recursively. The recursion
    %% must end with a constant string or a list (possibly empty), not a
    %% variable, since the list length must be constant. Multi-element cons
    %% patterns like [$c,$d,$e|T] are allowed, since they are parsed as
    %% [$c|[$d|[$e|T]]].
    %% However, standard Erlang does not allow any other kind of list
    %% element than chars or integers, even though that doesn't affect the
    %% matching complexity, nor are variables allowed in element positions,
    %% or as aliases on subpatterns, as in e.g. [$H=Char|...] ++ Rest.
    %%
    %% Principle: the LHS of a ++ must be a known nil or cons, allowing
    %% compile time unrolling of the ++ operator. This is done as [H|T]++R
    %% -> [H|(T++R)], and []++R -> R when is_list(R). There are no
    %% additional requirements on H. Note however that ("FORM"=Tag)++Rest
    %% or the equivalent [$F | [$O | [$R | [$M | []]]]]=Tag ++ Rest are not
    %% possible, because they reduce to [$F | [$O | [$R | [$M | Rest]]]],
    %% and the desired prefix Tag does not actually exist as a
    %% nil-terminated sequence in the input.

    %% NOTE: Currently in Erlang/OTP, []++Rest reduces to simply Rest,
    %% which skips the check for list-ness and becomes a catch-all.
    case X of
        "FORM" ++ Rest -> {form, Rest};
        [$X, $Y, $Z] ++ Rest -> {xyz, Rest};
        [$A, $B | "C"] ++ Rest -> {abc, Rest};
        [$H, $E | [$L | [$O | ""]]] ++ Rest -> {helo, Rest};
        [65, 67, 75 | []] ++ Rest -> {ack, Rest};
        [78 | [65, 75]] ++ Rest -> {nak, Rest};
        [] ++ Rest -> {nil, Rest};  % bug: the next clause is unreachable
        _ -> not_a_list
    end.

extended(X) ->
    case X of
        %% variables allowed in element position
        [A, 67, 75] ++ Rest -> {1, A, Rest};
        [65, B, 75] ++ Rest -> {2, B, Rest};
        [A, B, 75] ++ Rest -> {3, [A,B], Rest};
        [_ | [A, 88, B]] ++ Rest -> {x3, [A,B], Rest};

        %% any constant allowed in element position
        [3.14, <<"HELO">>, fubar | [1,2,3]] ++ Rest -> {const, Rest};

        %% any subpattern allowed in element position
        [{foo, A}, [{bar,B}, {baz, C}]] ++ Rest -> {any, {A, B, C}, Rest};

        %% alias variables allowed on elements
        [{a, A}=A0 | [{b,B}=B0]] ++ Rest -> {alias, {A, A0}, {B, B0}, Rest};
        [[{q,Q}=Tuple]=List] ++ Rest -> {alias2, Q, Tuple, List, Rest};

        _ -> other
    end.
