%% Standard erlt Types for helping to type gen server

[opaque, unchecked]
-type okvalue(Value) :: term(). % {ok, Value}.

[opaque, unchecked]
-type ok() :: term(). % ok.

[opaque, unchecked]
-type reply(Value, State) :: term(). % {reply, Value, State}.

[opaque, unchecked]
-type noreply(State) :: term(). % {noreply, State}.