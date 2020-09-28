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
-module(core).
-export_type([option/1, tuple1/1]).
-enum option(A) :: none{} | some{A}.
-opaque tuple1(A) :: {A}.

getOrElse(Opt, Alt) ->
  case Opt of
      option.some{X} -> X;
      option.none{} -> Alt
  end.
