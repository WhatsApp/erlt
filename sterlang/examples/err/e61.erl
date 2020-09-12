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
-module(e61).
-enum box(A) :: box{A}.
-type box_alias(A) :: box(A).
-type box_alias2(A) :: box_alias(box_alias(A)).

-enum box2(A, B) :: box2{A, B}.
-type box2_alias(A) :: box2(A, A).
% cyclic type alias
-type cyclic_box(A, B) :: box2(box(box(A)), cyclic_box(box2_alias(box_alias2(B)), box2_alias(box_alias2(B)))).
