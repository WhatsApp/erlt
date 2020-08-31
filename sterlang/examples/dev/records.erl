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
-module(records).

-record #user{name :: string(), id :: integer()}.
-record #manager{user :: #user{}}.
-record #ok{}.

-spec test(#user{}) -> #user{}.
test(X) -> X.

mk_user(Name, Id) ->
    #user{name = Name, id = Id}.

update_user(User, Name, Id) ->
    User#user{name = Name, id = Id}.

index() ->
    #user.name.

get_id(User) ->
    User#user.id.

get_man_id(Manager) ->
    (Manager#manager.user)#user.id.

check_user(User) when User == #user{name = "anonymous", id = -1} ->
    #ok{}.

check_id(Id) when Id > #user.id ->
    #ok{}.

check_users(User1, User2) when User1#user.id == User2#user.id ->
    #ok{};
check_users(User1, User2) ->
    #ok{}.

is_user(#user{}) -> true.

match_users(#user{id = Id}, #user{id = Id}) -> true.

foo(#manager{user = User}) ->
    User.

bar(#manager.user) ->
    #ok{}.
