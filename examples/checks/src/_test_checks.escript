#!/usr/bin/env escript

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


main([Erl2File]) ->
    Erl2cBin = "../../../erl2c/bin/erl2c",
    ExpOutputFile = Erl2File ++ ".exp",
    {ok, ExpOutput} =  file:read_file(ExpOutputFile),
    ExpOutPutStr = binary_to_list(ExpOutput),
    {ExitCode, ActualOutPut} = eunit_lib:command(Erl2cBin ++ " " ++ Erl2File),
    case ExitCode of
        0 ->
            io:format("`erl2c ~s` has not failed", [Erl2File]),
            halt(2);
        _ ->
            ok
    end,
    case string:str(ActualOutPut, ExpOutPutStr) of
        0 ->
            io:format(
                "`erl2c ~s`~nExpected to see an output with:~n  ~s~nGot:~n  ~s",
                [Erl2File, ExpOutPutStr, ActualOutPut]
            ),
            halt(2);
        _ ->
            io:format("OK (~s)~n", [Erl2File]),
            ok
    end.
