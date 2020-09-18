-lang([erlt]).
-module(struct_fields).

-export([local/0, duplicate/1]).

-export_type([struct/0]).

-struct struct :: (a = 5 :: integer(), b :: float()).

-struct duplicate :: (a :: integer(), a :: integer()).

local() ->
    {
        #struct{},
        #struct{a = 10},
        #struct{b = 1.0, does_not_exist = 10},
        #struct{does_not_exist = _} = #struct{b = 1.0}
    }.

duplicate(#struct{a = 1, a = 2, b = 3}) ->
    #struct{a = 1, a = 2, b = 3}.

%% we won't ever get remote definitons, since local check will fail above
%% and scan never produces output
% remote() ->
%     {
%         #struct{},
%         #struct{a = 10},
%         #struct{b = 1.0, does_not_exist = 10},
%         #struct{does_not_exist = _} = #struct{b = 1.0}
%     }.
