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

-lang(st).
-module(guards).

any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> true;
        false -> any(Pred, Tail)
    end;
any(Pred, []) -> false.

is_even(N) -> N rem 2 == 0.

any_even_or_empty([]) -> true;
any_even_or_empty(L) when any(fun is_even/1, L) -> true;
any_even_or_empty(_) -> false.

