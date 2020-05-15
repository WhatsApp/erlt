-lang([erl2, specs]).
-module('erlang.specs').

-export_type([ext_binary/0]).
-type ext_binary() :: binary().

-spec abs(integer()) -> integer().

-deprecated([{append_element/2, "see erlang2:append_elementN"}]).

-deprecated([{apply/2, "see erlang2:applyN"}]).

-deprecated([{apply/3, "only in dt"}]).

-spec atom_to_list(atom()) -> string().

-spec binary_part(binary(), {integer(), integer()}) -> binary().

-spec binary_part(binary(), integer(), integer()) -> binary().

-deprecated([{binary_to_atom/2, "see erlang2:binary_to_atom"}]).

-deprecated([{binary_to_existing_atom/2, "see erlang2:binary_to_existing_atom"}]).

-spec binary_to_integer(binary()) -> integer().

-spec binary_to_list(binary()) -> [byte()].

-spec binary_to_term(ext_binary()) -> term().

-spec bit_size(bitstring()) -> integer().

-spec byte_size(bitstring()) -> integer().

-deprecated([{cancel_timer/1, "see erlang2:cancel_timer/1"}]).

-deprecated([{cancel_timer/2, "see erlang2:cancel_timer/2"}]).

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

-spec spawn(fun(() -> _)) -> pid().

-deprecated([{start_timer/3, "see erlang2:start_timer/3"}]).

-deprecated([{start_timer/4, "see erlang2:start_timer/4"}]).
