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
-module(binaries).

b00() ->
    <<"literal">>.

b01() ->
    <<>>.

b02(X) ->
    <<X>>.

b03(X) ->
    <<X/integer>>.

b04(X) ->
    <<X/float>>.

b05(X) ->
    <<X/binary>>.

b06(X) ->
    <<X/bytes>>.

b07(X) ->
    <<X/bitstring>>.

b08(X) ->
    <<X/bits>>.

b09(X) ->
    <<X/utf8>>.

b10(X) ->
    <<X/utf16>>.

b11(X) ->
    <<X/utf32>>.

b12() ->
    <<"literal"/integer>>.

b13() ->
    <<"literal"/utf8>>.

b14() ->
    <<"literal"/utf16>>.

b15() ->
    <<"literal"/utf32>>.
