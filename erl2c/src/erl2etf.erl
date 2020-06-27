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
-module(erl2etf).

-export([main/1]).

main(["-erl", Filename, "-etf", EtfFileName]) ->
    {ok, Forms} = erl2_epp:parse_file(Filename, [{location, {1, 1}}, {scan_opts, [text]}]),
    CodeETF = erlang:term_to_binary(Forms),
    ok = filelib:ensure_dir(EtfFileName),
    ok = file:write_file(EtfFileName, CodeETF);
main(["-ast", Filename]) ->
    {ok, Forms} = erl2_epp:parse_file(Filename, [{location, {1, 1}}, {scan_opts, [text]}]),
    io:format("Forms:\n~p\n", [Forms]).