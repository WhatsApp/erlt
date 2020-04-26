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
-module(polymorphic_records).

-export_type([rec1/0, rec2/1, idRec/1]).

-type rec1() :: #{}.

%% This is (correctly) is not allowed.
%%-type someRec(R) :: #{ _:= _}.

-type rec2(A) :: #{ a := A }.
-type idRec(IdType) :: #{id := IdType}.

%% This is (correctly) is not allowed.
%%-type someRecWithId(IdType) :: #{ id := IdType, _ := _}.
