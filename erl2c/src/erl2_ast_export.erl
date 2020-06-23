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

-module('erl2_ast_export').

-export([from_abstract/1, file/1]).

-type line() :: integer().
-type line_and_col() :: {integer(), integer()}.
-type location() :: line() | line_and_col().
-type extra() :: [   {'user', [term()]}
                   | {'anno', term()}
                   | {pre_comm, [comment()]}
                   | {post_comm, [comment()]}
                 ].

-type tree() :: {tree, StartLoc::location(), EndLoc::location(), extra(),
                 NodeType::atom(), groups()}
              | {literal, StartLoc::location(), EndLoc::location(), extra(),
                 NodeType::atom(), Value :: term()}.
-type groups() :: {subtrees()}.
-type subtrees() :: {tree()}.

-type comment() :: {Text::binary(), padding()}.
-type padding() :: 'none' | integer().


%% demo function - reads a file and prints the export format
file(Filename) ->
    %% Note that the text option to the scanner is needed in order to
    %% compute the actual end position of individual tokens; without it,
    %% the start and end of literals will always be the same.
    %% Also, the start location must be seeded with {1,1} otherwise
    %% the scanner will default to line numbers only.
    {ok, Forms} = erl2_epp:parse_file(Filename, [{location,{1,1}},
                                                 {scan_opts, [text]}]),
    %% To test extended functionality, we add comments to the tree
    Comments = edoc:read_comments(Filename),
    Program = erl_syntax:form_list(Forms),
    Tree = erl_recomment:recomment_forms(Program, Comments),
    io:format("~p\n", [from_abstract(Tree)]).



%% Produces the following uniform syntax tree representation
%% more suitable for passing to a Java or C node.

-spec from_abstract(SyntaxTree :: erl_syntax:tree()) -> tree().

from_abstract(Tree) ->
    %% "pos" is actually a generic annotation these days
    Anno0 = erl_syntax:get_pos(Tree),
    StartLoc = erl_anno:location(Anno0),
    EndLoc = case erl2_parse:get_end_location(Anno0) of
                 undefined -> StartLoc;
                 L2 -> L2
             end,
    Extras =
        [
         %% "ann" are additional user annotations added by the erl_syntax layer
         case filter_anno(Anno0) of
             [] -> [];
             Anno -> [{anno, Anno}]
         end,
         case erl_syntax:get_ann(Tree) of
             [] -> [];
             UserAnno -> [{user, UserAnno}]
         end,
         %% erl_syntax also adds pre- and postcomments
         case erl_syntax:get_precomments(Tree) of
             [] -> [];
             PreCs -> [{pre_comm, comment_data(PreCs)}]
         end,
         case erl_syntax:get_postcomments(Tree) of
             [] -> [];
             PostCs -> [{post_comm, comment_data(PostCs)}]
         end
        ],
    Extra = lists:append(Extras),  % flatten one level

    Type = erl_syntax:type(Tree),
    case erl_syntax:is_literal(Tree) of
        true ->
            {literal, StartLoc, EndLoc, Extra, Type,
             erl_syntax:concrete(Tree)};
        false ->
            Subtrees = [list_to_tuple([from_abstract(Subtree)
                                       || Subtree <- Group])
                        || Group <- erl_syntax:subtrees(Tree)],
            {tree, StartLoc, EndLoc, Extra, Type,
             list_to_tuple(Subtrees)}
    end.

comment_data(Cs) ->
    [{ erl_syntax:comment_padding(C),
       [unicode:characters_to_binary(T) || T <- erl_syntax:comment_text(C)] }
     || C <- Cs].

filter_anno([{file, _}=A | As]) ->
    [A | filter_anno(As)];
filter_anno([_Other | As]) ->
    filter_anno(As);
filter_anno(_Other) ->
    [].
