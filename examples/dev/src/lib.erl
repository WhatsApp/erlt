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
-module(lib).

-export([id/1]).
-export_type([public_enum/0, opaque_enum_alias/0]).

-enum public_enum() :: public_ctr{}.
-enum private_enum() :: private_ctr{}.
-enum semi_private_enum() :: semi_private_ctr{}.

-opaque opaque_enum_alias() :: semi_private_enum().

-spec id(A) -> A.
id(X) -> priv_id(X).

-spec priv_id(A) -> A.
priv_id(X) -> X.
