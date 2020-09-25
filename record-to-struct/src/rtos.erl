-module(rtos).

-export([make_erlt/1,field_erlt/1,update_erlt/2,update_erlt/3]).

-export([make_erl1/1,field_erl1/1,update_erl1/2,update_erl1/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This section would be in the include file for the ErlT module.

-compile({parse_transform,record_to_struct}).

%% Define which records are to be converted.

-records_to_convert_to_structs([erlt]).

%% The ErlT interface records

-record(erlt, {a,b=1+2,c}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% "Local" Erl1 records which are not converted.

-record(erl1, {x,y=2*4 :: integer(),z}).

%% And the test functions.
%% Note that the make functions are cross over.

make_erlt(Val) ->
    #erlt{b=Val,c=#erl1{}}.

field_erlt(Str) ->
    Str#erlt.a.

update_erlt(Str, Val) ->
    Str#erlt{a=Val}.

update_erlt(Str, Val1, Val2) ->
    Str#erlt{a=Val1,c=Val2}.

make_erl1(Val) ->
    #erl1{y=Val,z=#erlt{}}.

field_erl1(Rec) ->
    Rec#erl1.x.

update_erl1(Rec, Val) ->
    Rec#erl1{x=Val}.

update_erl1(Rec, Val1, Val2) ->
    Rec#erl1{x=Val1,z=Val2}.
