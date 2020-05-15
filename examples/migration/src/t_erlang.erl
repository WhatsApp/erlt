-lang([erl2, ffi]).
-module(t_erlang).

-export_type([encoding/0]).
-enum encoding() :: latin1{} | unicode{} | utf8{}.

-export_type([cancel_timer_option/0]).
-enum cancel_timer_option() :: async{boolean()} | info{boolean()}.

-export_type([maybe/1]).
-enum maybe(A) :: just{A} | nothing{}.

-export_type([timer_dst/0]).
-enum timer_dst() :: pid{pid()} | named{atom()}.

-export_type([timer_option/0]).
-enum timer_option() :: abs{boolean()}.

-export_type([check_process_code_option/0]).
-enum check_process_code_option() :: async{term()} | allow_gc{boolean()}.

-export_type([check_process_code_result/0]).
-enum check_process_code_result() :: old{boolean()} | abort{} | async{}.

-export_type([time_unit/0]).
-enum time_unit() :: per_second{integer()}
                   | second{}
                   | millisecond{}
                   | microsecond{}
                   | native{}
                   | perf_counter{}.

-export_type([demonitor_option/0]).
-enum demonitor_option() :: flush{} | info{}.

-export_type([port_or_pid/0]).
-enum port_or_pid() :: port{port()} | pid{pid()}.

-export([append_element0/2, append_element1/2, append_element2/2, append_element3/2]).

-export([apply0/2, apply1/2, apply2/2, apply3/2]).
-export([binary_to_atom/2, binary_to_existing_atom/2]).

-export([cancel_timer/1, cancel_timer/2]).

-export([check_process_code/3]).
-export([convert_time_unit/3]).
-export([demonitor/2, exit/2]).

-export([start_timer/3, start_timer/4]).

-spec append_element0({}, El) -> {El}.
append_element0(Tuple, El) ->
    erlang:append_element(Tuple, El).

-spec append_element1({El1}, ExtraEl) -> {El1, ExtraEl}.
append_element1(Tuple, ExtraEl) ->
    erlang:append_element(Tuple, ExtraEl).

-spec append_element2({El1, El2}, ExtraEl) -> {El1, El2, ExtraEl}.
append_element2(Tuple, ExtraEl) ->
    erlang:append_element(Tuple, ExtraEl).

-spec append_element3({El1, El2, El3}, ExtraEl) -> {El1, El2, El3, ExtraEl}.
append_element3(Tuple, ExtraEl) ->
    erlang:append_element(Tuple, ExtraEl).

-spec apply0(fun(() -> A), {}) -> A.
apply0(F, _) ->
    erlang:apply(F, []).

-spec apply1(fun((El1) -> A), {El1}) -> A.
apply1(F, T) ->
    erlang:apply(F, erlang:tuple_to_list(T)).

-spec apply2(fun((El1, El2) -> A), {El1, El2}) -> A.
apply2(F, T) ->
    erlang:apply(F, erlang:tuple_to_list(T)).

-spec apply3(fun((El1, El2, El3) -> A), {El1, El2, El3}) -> A.
apply3(F, T) ->
    erlang:apply(F, erlang:tuple_to_list(T)).

-spec binary_to_atom(binary(), encoding()) -> atom().
binary_to_atom(Binary, encoding.latin1{}) ->
    erlang:binary_to_atom(Binary, 'latin1');
binary_to_atom(Binary, encoding.unicode{}) ->
    erlang:binary_to_atom(Binary, 'unicode');
binary_to_atom(Binary, encoding.utf8{}) ->
    erlang:binary_to_atom(Binary, 'utf8').

-spec binary_to_existing_atom(binary(), encoding()) -> atom().
binary_to_existing_atom(Binary, encoding.latin1{}) ->
    erlang:binary_to_existing_atom(Binary, 'latin1');
binary_to_existing_atom(Binary, encoding.unicode{}) ->
    erlang:binary_to_existing_atom(Binary, 'unicode');
binary_to_existing_atom(Binary, encoding.utf8{}) ->
    erlang:binary_to_existing_atom(Binary, 'utf8').

-spec cancel_timer(reference()) -> maybe(integer()).
cancel_timer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
        false -> maybe.nothing{};
        Time -> maybe.just{Time}
    end.

-spec cancel_timer(reference(), [cancel_timer_option()]) -> maybe(integer()).
cancel_timer(TimerRef, Options) ->
    ConvertOpt =
        fun (cancel_timer_option.async{B}) -> {async, B};
            (cancel_timer_option.info{B}) -> {info, B}
        end,
    Options1 = lists:map(ConvertOpt, Options),
    case erlang:cancel_timer(TimerRef, Options1) of
        false -> maybe.nothing{};
        Time -> maybe.just{Time}
    end.

-spec check_process_code(pid(), module(), [check_process_code_option()]) -> check_process_code_result().
check_process_code(Pid, Module, Options) ->
    case erlang:check_process_code(Pid, Module, Options) of
        true -> check_process_code_result.old{true};
        false -> check_process_code_result.old{false};
        abort -> check_process_code_result.abort{};
        async -> check_process_code_result.async{}
    end.


-spec convert_time_unit(integer(), time_unit(), time_unit()) -> integer().
convert_time_unit(Time, FromUnit, ToUnit) ->
    erlang:convert_time_unit(Time, unpack_time_unit(FromUnit), unpack_time_unit(ToUnit)).

unpack_time_unit(FromUnit) ->
    case FromUnit of
        time_unit.per_second{I} -> I;
        time_unit.second{} -> second;
        time_unit.millisecond{} -> millisecond;
        time_unit.microsecond{} -> microsecond;
        time_unit.native{} -> native;
        time_unit.perf_counter{} -> perf_counter
    end.

-spec demonitor(reference(), [demonitor_option()]) -> boolean().
demonitor(MonitorRef, Options) ->
    ConvertOpt =
        fun (demonitor_option.flush{}) -> flush;
            (demonitor_option.info{}) -> info
    end,
    Options1 = lists:map(ConvertOpt, Options),
    erlang:demonitor(MonitorRef, Options1).

-spec exit(port_or_pid(), term()) -> boolean().
exit(port_or_pid.port{Port}, Reason) -> erlang:exit(Port, Reason);
exit(port_or_pid.pid{Pid}, Reason) -> erlang:exit(Pid, Reason).

-spec start_timer(integer(), timer_dst(), _Msg) -> reference().
start_timer(Time, Dest, Msg) ->
    Dest1 =
        case Dest of
            timer_dst.pid{Pid} -> Pid;
            timer_dst.named{Atom} -> Atom
        end,
    erlang:start_timer(Time, Dest1, Msg).

-spec start_timer(integer(), timer_dst(), _Msg, [timer_option()]) -> reference().
start_timer(Time, Dest, Options, Msg) ->
    ConvertOpt = fun (timer_option.abs{B}) -> {abs, B} end,
    Options1 = lists:map(ConvertOpt, Options),
    Dest1 =
        case Dest of
            timer_dst.pid{Pid} -> Pid;
            timer_dst.named{Atom} -> Atom
    end,
    erlang:start_timer(Time, Dest1, Msg, Options1).
