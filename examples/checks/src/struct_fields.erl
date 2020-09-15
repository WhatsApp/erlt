-lang([erlt]).
-module(struct_fields).

-export([local/0]).

-export_type([struct/0]).

-struct struct :: (a = 5 :: integer(), b :: float()).

local() ->
    {
        #struct{},
        #struct{a = 10},
        #struct{b = 1.0, does_not_exist = 10},
        #struct{does_not_exist = _} = #struct{b = 1.0}
    }.

%% we won't ever get remote definitons, since local check will fail above
%% and scan never produces output
% remote() ->
%     {
%         #struct{},
%         #struct{a = 10},
%         #struct{b = 1.0, does_not_exist = 10},
%         #struct{does_not_exist = _} = #struct{b = 1.0}
%     }.
