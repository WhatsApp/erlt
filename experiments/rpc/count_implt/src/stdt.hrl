%% Standard erlt Types for helping to type gen server

[opaque, unchecked]
-type atom_ok_value(Value) :: term(). % {ok, Value}.

[opaque, unchecked]
-type atom_ok() :: term(). % ok.

[opaque, unchecked]
-type atom_reply(Value, State) :: term(). % {reply, Value, State}.

[opaque, unchecked]
-type atom_noreply(State) :: term(). % {noreply, State}.

-export_type([ok_value/1]).

-enum ok_value(Value) :: (
    ok{Value}
).

-export_type([ok/0]).

-enum ok() :: (
    ok{}
).

-export_type([registration/0]).

-enum registration() :: (
    local{},
    global{}
).