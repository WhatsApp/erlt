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

-lang([erl2, st]).
-module(mod04).

-export([
  mk_unit_remote_dot/0, mk_left_remote_dot/1, mk_right_remote_dot/1,
  zero_remote_dot/2, zero_remote1_dot/2,
  un_pair_remote_dot/1, funs/0, remote_dot2/0, import1/0
]).

-import(mod03, [mk_unit/0]).

-spec mk_unit_remote_dot() -> mod03:unit0().
mk_unit_remote_dot() -> mod03.unit0.unit0{}.

-spec mk_left_remote_dot(A) -> mod03:either(A, _).
mk_left_remote_dot(A) -> mod03.either.left{A}.

-spec mk_right_remote_dot(B) -> mod03:either(_, B).
mk_right_remote_dot(B) -> mod03.either.right{B}.

-spec zero_remote_dot(mod03:unit0(), V) -> V.
zero_remote_dot(U, Val) ->
  case U of
    mod03.unit0.unit0{} -> Val
end.

%% This is not supported - see https://github.com/WhatsApp/erl2/issues/3
%%-spec zero_remote1(mod03:unit0(), V) -> V.
%%zero_remote1(mod03:unit0{}, Val) -> Val.

-spec zero_remote1_dot(mod03:unit0(), V) -> V.
zero_remote1_dot(mod03.unit0.unit0{}, Val) -> Val.

-spec un_pair_remote_dot(mod03:pair(A,B)) -> {A,B}.
un_pair_remote_dot(Pair) ->
  case Pair of
    mod03.pair.pair{A, B} -> {A, B}
end.

-spec funs() -> list(fun(() -> mod03:unit0())).
funs() -> [
  fun mod03:mk_unit/0,
  fun mod04:mk_unit_remote_dot/0,
  fun mk_unit_remote_dot/0,
  fun () -> mod03.unit0.unit0{} end
].

-spec remote_dot2() -> mod03.unit0().
remote_dot2() -> mod03.mk_unit().

-spec import1() -> mod03.unit0().
import1() -> mk_unit().

%% Should work some day - see https://github.com/erlang/otp/blob/9a6e292336cb36561f9a993f86ddee363f1d0850/lib/stdlib/src/erl_lint.erl#L2427-L2435
%%-spec import2() -> fun(() -> mod03.unit0()).
%%import2() -> fun mk_unit/0.
