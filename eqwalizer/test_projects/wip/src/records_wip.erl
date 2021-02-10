-module(records_wip).

-export([fun_call/1]).

-record(func_rec, {
    func :: fun(() -> 3)
}).

-record(func_rec1, {
    func =
        fun() -> 3 end
        :: fun(() -> 3)
}).

-spec fun_call(#func_rec{}) -> number().
fun_call(X) -> (X#func_rec.func)().

-record(any_box, {inner :: any()}).

%% "Refined" record type
-type int_box() ::
    #any_box{inner :: integer()}.
