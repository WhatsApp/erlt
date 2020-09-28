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

b16(<<>>) -> true.

b17(<<>>, <<"literal">>, <<X>>, X) -> true.

b18(<<>>, <<"literal">>, X, <<X>>) -> true.

b19(<<X, Y/bitstring>>) -> {X, Y}.

b20(<<X, Y/binary>>) -> {X, Y}.

b21(<<Size1, Binary1:Size1/binary, Size2, Binary2:Size2/binary>>) -> {Size1, Binary1, Size2, Binary2}.

b22(<<UTF8/utf8, UTF16/utf16, UTF32/utf32>>) -> {UTF8, UTF16, UTF32}.

b23(<<"utf8"/utf8, "utf16"/utf16, "utf32"/utf32>>) -> {}.

b24(<<X/utf8, X/utf16>>) -> {}.

b25(Binary1, Size1, Binary2, Size2) when <<Binary1:Size1/binary>> == <<Binary2:Size2/binary>> -> true;
b25(Binary1, Size1, Binary2, Size2) -> false.

b26(Binary1, Size1, Binary2, Size2) ->
    if
        <<Binary1:Size1/binary>> == <<Binary2:Size2/binary>> -> true;
        true -> false
    end.

b27(Binary) ->
    << <<X>> || <<X>> <= Binary, X > 100 >>.

b28(<<Float/float>>) -> {Float}.
