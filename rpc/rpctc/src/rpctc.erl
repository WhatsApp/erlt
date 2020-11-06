%%
%% %CopyrightBegin%
%%
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
%%
%% %CopyrightEnd%

-module(rpctc).

-export([main/1, parse_service/1, fun_str/1, parse_props/1]).

-include("service.hrl").

main(["header", Input]) ->
    io:format(standard_error, "parsing: ~s~n", [Input]),
    {ok, Service} = parse_service(Input),
    io:format(standard_error, "generating~n", []),
    GenStr = header:gen(Service),
    io:format("~s", [GenStr]);
main(["impl", Input]) ->
    io:format(standard_error, "parsing: ~s~n", [Input]),
    {ok, Service} = parse_service(Input),
    io:format(standard_error, "generating~n", []),
    GenStr = impl:gen(Service),
    io:format("~s", [GenStr]);
main(["gadtimpl", Input]) ->
    io:format(standard_error, "parsing: ~s~n", [Input]),
    {ok, Service} = parse_service(Input),
    io:format(standard_error, "generating~n", []),
    GenStr = gadt_impl:gen(Service),
    io:format("~s", [GenStr]);
main(["gadtheader", Input]) ->
    io:format(standard_error, "parsing: ~s~n", [Input]),
    {ok, Service} = parse_service(Input),
    io:format(standard_error, "generating~n", []),
    GenStr = gadt_header:gen(Service),
    io:format("~s", [GenStr]).

%% Copied from do_parse_module in erlt_compile.erl
parse_service(
    File
) ->
    R = erlt_epp:parse_file(File, [
        % {includes, [".", Dir | inc_paths(Opts)]},
        {includes, []},
        {default_encoding, utf8},
        {location, {1, 1}},
        {scan_opts, [text]},
        extra
    ]),
    case R of
        {ok, Forms, _Extra} ->
            % io:format(user, "Forms: ~p~n Extra~p~n", [Forms, Extra]),
            {_, Props} = erlt_ast:prewalk(Forms, [], fun walk_rpc/3),
            % io:format(user, "Calls: ~p~n", [Calls]),
            RPCs = parse_props(Props),
            {ok, RPCs};
        {error, E} ->
            % io:format(user, "Error: ~p~n", [E]),
            {error, E}
    end.

parse_props(Props) ->
    parse_props(Props, #service{}).

parse_props([], Service) ->
    Service;
parse_props([#cast{} = C | Props], #service{casts = Casts} = Service) ->
    parse_props(Props, Service#service{casts = [C | Casts]});
parse_props([#call{} = C | Props], #service{calls = Calls} = Service) ->
    parse_props(Props, Service#service{calls = [C | Calls]});
parse_props([{name, Name} | Props], Service) ->
    parse_props(Props, Service#service{name = Name});
parse_props([singleton | Props], Service) ->
    parse_props(Props, Service#service{singleton = true}).

walk_rpc({attribute, _, rpc, {{name, _Rank}, [Type]}} = Node, Acc, form) ->
    {_Params, Return} = fun_type(Type),
    Result = {name, type_str(Return)},
    {Node, [Result | Acc]};
walk_rpc({attribute, _, rpc, {{singleton, _Rank}, [Type]}} = Node, Acc, form) ->
    {_Params, Return} = fun_type(Type),
    case type_str(Return) of
        "true" -> {Node, [singleton | Acc]};
        _ -> {Node, Acc}
    end;
walk_rpc({attribute, _, rpc, {{Name, _Rank}, [Type]}} = Node, Acc, form) ->
    {Params, Return} = fun_type(Type),
    Result =
        case type_str(Return) of
            "no_return()" ->
                #cast{name = atom_to_list(Name), params = lists:map(fun type_str/1, Params)};
            _ ->
                #call{
                    name = atom_to_list(Name),
                    params = lists:map(fun type_str/1, Params),
                    return = type_str(Return)
                }
        end,
    {Node, [Result | Acc]};
walk_rpc(Node, Acc, _Ctx) ->
    {Node, Acc}.

fun_type({type, _Metas, 'fun', [{type, _Metas2, product, Params}, Return]}) ->
    {Params, Return}.

fun_str(#call{name = Name, params = Params, return = Return}) ->
    lists:flatten(Name ++ "(" ++ lists:join(", ", Params) ++ ") -> " ++ Return);
fun_str(#cast{name = Name, params = Params}) ->
    lists:flatten(Name ++ "(" ++ lists:join(", ", Params) ++ ") -> no_return()").

type_str({type, _, _, _} = T) ->
    lists:flatten(erlt_pp:type(T));
type_str({atom, _, _} = A) ->
    lists:flatten(erlt_pp:expr(A)).
