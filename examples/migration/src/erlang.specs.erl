-lang([erl2, specs]).
-module('erlang.specs').

-export_type([ext_binary/0]).
-type ext_binary() :: binary().

-spec abs(integer()) -> integer().

-deprecated([{append_element/2, "see t_erlang:append_elementN"}]).

-deprecated([{apply/2, "see t_erlang:applyN"}]).

-deprecated([{apply/3, "only in dt"}]).

-spec atom_to_list(atom()) -> string().

-spec binary_part(binary(), {integer(), integer()}) -> binary().

-spec binary_part(binary(), integer(), integer()) -> binary().

-deprecated([{binary_to_atom/2, "see t_erlang:binary_to_atom"}]).

-deprecated([{binary_to_existing_atom/2, "see t_erlang:binary_to_existing_atom"}]).

-spec binary_to_integer(binary()) -> integer().

-spec binary_to_list(binary()) -> [byte()].

-spec binary_to_term(ext_binary()) -> term().

-spec bit_size(bitstring()) -> integer().

-spec byte_size(bitstring()) -> integer().

-deprecated([{cancel_timer/1, "see t_erlang:cancel_timer/1"}]).

-deprecated([{cancel_timer/2, "see t_erlang:cancel_timer/2"}]).

-spec ceil(integer()) -> integer().

-spec check_old_code(module()) -> boolean().
-spec check_process_code(pid(), module()) -> boolean().

-spec crc32(iodata()) -> integer().
-spec crc32(integer(), iodata()) -> integer().

-spec demonitor(reference()) -> boolean().
-spec erase() -> [{term(), term()}].
%% TODO - The original is term() | undefined for the result,
%% but undefined is term().
-spec erase(term()) -> term().

%% It is the same as undefined in haskell or ???
-spec error(term()) -> _.
-spec error(term(), [term()]) -> _.

-spec exit(term()) -> _.
-spec external_size(term()) -> integer().

-spec floor(number()) -> integer().
-spec function_exported(module(), atom(), arity()) -> boolean().
-spec garbage_collect() -> boolean().
-spec garbage_collect(pid()) -> boolean().
-spec get_cookie(atom()) -> atom().

-deprecated([{get_stacktrace/0, "see erlang documentation"}]).
-spec get_stacktrace() -> [_].
-spec halt() -> _.
-spec hd([A]) -> A.
-spec integer_to_binary(integer()) -> binary().
-spec integer_to_list(integer()) -> string().
-spec integer_to_list(integer(), integer()) -> string().
-deprecated([{iolist_size/0, "see t_erlang:iolist_size and t_erlang:binary_size"}]).
-spec iolist_size(_) -> _.
-deprecated([{iolist_size/0, "see t_erlang:iolist_to_binary and t_erlang:binary_to_binary"}]).
-spec iolist_to_binary(_) -> _.

-spec is_atom(_) -> boolean().
-spec is_binary(_) -> boolean().
-spec is_bitstring(_) -> boolean().
-spec is_boolean(_) -> boolean().
-spec is_float(_) -> boolean().
-spec is_function(_) -> boolean().
-spec is_function(_, arity()) -> boolean().
-spec is_integer(_) -> boolean().
-spec is_list(_) -> boolean().
-spec is_map(_) -> boolean().
-spec is_number(_) -> boolean().
-spec is_pid(_) -> boolean().
-spec is_port(_) -> boolean().
-spec is_record(_) -> boolean().
-spec is_reference(_) -> boolean().
-spec is_tuple(_) -> boolean().

-spec is_process_alive(pid()) -> boolean().

-spec length([_]) -> integer().

-deprecated([{link/1, "see t_erlang:link_pid/1 and t_erlang:link_port/1"}]).
-spec link(_) -> boolean().

-spec list_to_atom(string()) -> atom().
-spec list_to_binary(iolist()) -> binary().
-spec list_to_existing_atom(string()) -> atom().
-spec list_to_integer(string()) -> integer().
-spec list_to_integer(string(), integer()) -> integer().
-spec list_to_pid(string()) -> pid().
-spec list_to_port(string()) -> port().
-spec list_to_ref(string()) -> reference().
-spec list_to_tuple([_]) -> _.

-spec spawn(fun(() -> _)) -> pid().

-deprecated([{start_timer/3, "see t_erlang:start_timer/3"}]).

-deprecated([{start_timer/4, "see t_erlang:start_timer/4"}]).
