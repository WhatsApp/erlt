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
-module(exceptions).

-exception(not_found, {}).
-exception(invocation_target_exception, {target :: exception()}).

-enum option(A) :: none{} | some{A}.

mk_exn(option.none{}) ->
    #not_found{};
mk_exn(option.some{Target}) ->
    #invocation_target_exception{target = Target}.

get_target(#not_found{}) ->
    option.none{};
get_target(#invocation_target_exception{target = Target}) ->
    option.some{Target}.
