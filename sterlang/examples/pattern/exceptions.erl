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
-export([test1/0, test2/1, test3/0, test4/0, test5/0, test6/0, test7/0, test8/1]).

-exception(e0, {}).
-exception(e1, {a :: boolean()}).

% We don't check for exhaustiveness in catch clauses.
-spec test1() -> {}.
test1() ->
  try
    {}
  catch
    #e0{} -> {}
  end.

% We do check for exhaustiveness in try-of clauses.
-spec test2(boolean()) -> {}.
test2(X) ->
  try X of
    true -> {};
    false -> {}
  catch
    #e0{} -> {}
  end.

% We can match on multiple exception types.
-spec test3() -> {}.
test3() ->
  try
    {}
  catch
    #e0{} -> {};
    #e1{a = _} -> {}
  end.

% We can do nested pattern matching.
-spec test4() -> {}.
test4() ->
  try
    {}
  catch
    #e1{a = false} -> {};
    #e1{a = true} -> {}
  end.

% Nested pattern matching does not have to be exhaustive either.
-spec test5() -> {}.
test5() ->
  try
    {}
  catch
    #e1{a = false} -> {}
  end.

% Catch allows wildcards.
-spec test6() -> {}.
test6() ->
  try
    {}
  catch
    _ -> {}
  end.

% Catch allows mixing patterns with wildcards.
-spec test7() -> {}.
test7() ->
  try
    {}
  catch
    #e0{} -> {};
    #e1{a = false} -> {};
    #e1{a = true} -> {};
    _ -> {}
  end.


% Exceptions can be manipulated as data.
-spec test8(exception()) -> {}.
test8(#e0{}) -> {};
test8(#e1{a = false}) -> {};
test8(_) -> {}.
