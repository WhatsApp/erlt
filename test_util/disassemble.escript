#!/usr/bin/env escript

%% -*- erlang -*-

%% adapted from https://github.com/erlang/otp/blob/39e2dc5709f3df0e7b4533fa45820d7140b82d4c/scripts/diffable
%% changes:
%%     - remove unneeded stuff
%%     - add clean_fun_numbers/1 -- note that this makes the disassembly lossy

-mode(compile).

main(["--beam", BeamDir, OutDir]) ->
    Files = filelib:wildcard(BeamDir ++ "/*.beam"),
    [handle_beam(F, OutDir) || F <- Files],
    ok;
main(["--erl", ErlDir, OutDir | CompilerOpts]) ->
    Files = filelib:wildcard(ErlDir ++ "/*.erl"),
    [handle_erl(F, OutDir, CompilerOpts) || F <- Files],
    ok;
main(_) ->
    io:format(
        "regression testing utility for getting beam disassmbly from .erls and .beams~n~n"
        "usage: --beam  dir  outdir ...compiler_options~n"
        "usage: --erl   dir  outdir ...compiler_options~n~n"
    ),
    erlang:halt(1).

handle_beam(BeamFilename, OutDir) ->
    {ok, Beam} = file:read_file(BeamFilename),
    Mod = list_to_atom(filename:basename(BeamFilename, ".beam")),
    Dis = beam_to_dis(Mod, Beam),
    write_dis(Mod, Dis, OutDir).

handle_erl(File, OutDir, Opts) ->
    {ok, Mod, Asm} = compile:file(File, [to_asm, binary, report_errors | Opts]),
    AsmOpts = [from_asm, report, no_postopt, binary],
    {ok, _Mod, Beam} = compile:forms(Asm, AsmOpts),
    Dis = beam_to_dis(Mod, Beam),
    write_dis(Mod, Dis, OutDir).

beam_to_dis(Mod, Beam) ->
    Dis0 = disasm(Mod, Beam),
    Dis1 = renumber_disasm(Dis0),
    format_disasm(Dis1).

write_dis(Mod, Dis, OutDir) ->
    OutFile = filename:join(OutDir, atom_to_list(Mod) ++ ".dis"),
    ok = file:write_file(OutFile, Dis).

disasm(Mod, Beam) ->
    {module, Mod} = code:load_binary(Mod, "", Beam),
    disasm(Mod).

disasm(Mod) ->
    disasm_1(Mod:module_info(functions), Mod).

disasm_1([{Name, Arity} | Fs], Mod) ->
    MFA = {Mod, Name, Arity},
    Dis = disasm_func({MFA, <<>>, MFA}, MFA),
    [{Name, Arity, Dis} | disasm_1(Fs, Mod)];
disasm_1([], _) ->
    [].

disasm_func({Next, _, MFA}, MFA) ->
    case erts_debug:disassemble(Next) of
        {_, Line, MFA} = Cont ->
            [Line | disasm_func(Cont, MFA)];
        {_, _, _} ->
            [];
        false ->
            []
    end.

%% @doc Renumber the disassembled module to use labels instead of
%% absolute addresses. Also do other translations so that the
%% output will be the same each time (for the same BEAM file
%% runtime system).
renumber_disasm(Fs0) ->
    Fs1 = split_dis_lines(Fs0),
    renumber_disasm_fs(Fs1).

renumber_disasm_fs([{Name, Arity, Is0} | Fs]) ->
    Labels = find_labels(Is0, Name, Arity),
    Is = renumber_disasm_func(Is0, Labels),
    [{Name, Arity, Is} | renumber_disasm_fs(Fs)];
renumber_disasm_fs([]) ->
    [].

renumber_disasm_func([[A, OpCode | Ops0] | Is], Labels) ->
    Spaces = "    ",
    Left =
        case maps:find(A, Labels) of
            {ok, Lbl} ->
                case byte_size(Lbl) of
                    LblSize when LblSize < length(Spaces) ->
                        [$\n, Lbl, ":", lists:nth(LblSize, Spaces)];
                    _ ->
                        [Lbl, ":\n" | Spaces]
                end;
            error ->
                Spaces
        end,
    Ops1 = [clean_fun_numbers(Op) || Op <- Ops0],
    Ops2 = [replace_label(Op, Labels) || Op <- Ops1],
    Ops = handle_special_instrs(OpCode, Ops2),
    [[Left, OpCode | Ops] | renumber_disasm_func(Is, Labels)];
renumber_disasm_func([], _) ->
    [].

%% @doc replace the last segment of numbers from fun names with 0.
%% Example: <<"`#Fun<mod01.1.90332858>`">> becomes <<"`#Fun<mod01.1.0>`">>
%% Reason: the numbers don't seemed to be used anywhere and the last segment
%% (90332858 in this case) appears to be chaotic.
clean_fun_numbers(Op) ->
    re:replace(Op, "#Fun<(.+?)\\.(.+?)\\.(.+?)>", "#Fun<\\1.\\2.0>").

handle_special_instrs(<<"i_get_hash_cId">>, [Key, _Hash, Dst]) ->
    [Key, hash_value(), Dst];
handle_special_instrs(
    <<"i_get_map_element_", _/binary>>,
    [Fail, Src, Key, _Hash, Dst]
) ->
    [Fail, Src, Key, hash_value(), Dst];
handle_special_instrs(
    <<"i_get_map_elements_", _/binary>>,
    [Fail, Src, N, Space | List0]
) ->
    List1 = rejoin_atoms(List0),
    List = fix_hash_value(List1),
    [Fail, Src, N, Space | List];
handle_special_instrs(
    <<"i_select_val_bins_", _/binary>>,
    [Src, Fail, Num | List0]
) ->
    %% Atoms are sorted in atom-number order, which is
    %% different every time the runtime system is restarted.
    %% Resort the values in ASCII order.
    List1 = rejoin_atoms(List0),
    {Values0, Labels0} = lists:split(length(List1) div 2, List1),
    Zipped0 = lists:zip(Values0, Labels0),
    Zipped = lists:sort(Zipped0),
    {Values, Labels} = lists:unzip(Zipped),
    [Src, Fail, Num | Values ++ Labels];
handle_special_instrs(
    <<"i_select_val_lins_", _/binary>>,
    [Src, Fail, Num | List0]
) ->
    List1 = rejoin_atoms(List0),
    {Values0, Labels0} = lists:split(length(List1) div 2, List1),
    Values1 = lists:droplast(Values0),
    Labels1 = lists:droplast(Labels0),
    Vlast = lists:last(Values0),
    Llast = lists:last(Labels0),
    Zipped0 = lists:zip(Values1, Labels1),
    Zipped = lists:sort(Zipped0),
    {Values, Labels} = lists:unzip(Zipped),
    [Src, Fail, Num | Values ++ [Vlast] ++ Labels ++ [Llast]];
handle_special_instrs(_, Ops) ->
    Ops.

fix_hash_value([Val, Dst, _Hash | T]) ->
    [Val, Dst, hash_value() | fix_hash_value(T)];
fix_hash_value([]) ->
    [].

hash_value() ->
    <<"--hash-value--">>.

replace_label(<<"f(", T/binary>>, Labels) ->
    replace_label_1("f(", T, Labels);
replace_label(<<"j(", T/binary>>, Labels) ->
    replace_label_1("j(", T, Labels);
replace_label(Op, _Labels) ->
    Op.

replace_label_1(Prefix, Lbl0, Labels) ->
    Sz = byte_size(Lbl0) - 1,
    Lbl =
        case Lbl0 of
            <<"0)">> ->
                Lbl0;
            <<Lbl1:Sz/bytes, ")">> ->
                [maps:get(Lbl1, Labels), ")"];
            _ ->
                Lbl0
        end,
    iolist_to_binary([Prefix, Lbl]).

split_dis_lines(Fs) ->
    {ok, RE} = re:compile(<<"\\s*\\n$">>),
    Colon = binary:compile_pattern(<<": ">>),
    Space = binary:compile_pattern(<<" ">>),
    [split_dis_func(F, RE, Colon, Space) || F <- Fs].

split_dis_func({Name, Arity, Lines0}, RE, Colon, Space) ->
    Lines1 = [re:replace(L, RE, <<>>, [{return, binary}]) || L <- Lines0],
    Lines2 = [
        begin
            [A, I] = binary:split(L, Colon),
            Ops = binary:split(I, Space, [global]),
            [A | Ops]
        end
        || L <- Lines1
    ],
    {Name, Arity, Lines2}.

rejoin_atoms([<<"`'", Tail/binary>> = Bin0, Next | Ops]) ->
    Sz = byte_size(Tail) - 2,
    case Tail of
        <<_:Sz/bytes, "'`">> ->
            [Bin0 | rejoin_atoms([Next | Ops])];
        <<>> ->
            Bin = <<Bin0/binary, $\s, Next/binary>>,
            rejoin_atoms([Bin | Ops]);
        _ ->
            Bin = <<Bin0/binary, $\s, Next/binary>>,
            rejoin_atoms([Bin | Ops])
    end;
rejoin_atoms([Op | Ops]) ->
    [Op | rejoin_atoms(Ops)];
rejoin_atoms([]) ->
    [].

find_labels(Is, Name, Arity) ->
    [_, [Entry | _] | _] = Is,
    EntryLabel = iolist_to_binary(io_lib:format("~p/~p", [Name, Arity])),
    {ok, RE} = re:compile(<<"^[fj]\\(([0-9A-F]{8,16})\\)$">>),
    Ls0 = [find_labels_1(Ops, RE) || [_Addr, _OpCode | Ops] <- Is],
    Ls1 = lists:flatten(Ls0),
    Ls2 = lists:usort(Ls1),
    Ls3 = number(Ls2, 1),
    Ls = [{Entry, EntryLabel} | Ls3],
    maps:from_list(Ls).

find_labels_1([Op | Ops], RE) ->
    case re:run(Op, RE, [{capture, all_but_first, binary}]) of
        nomatch ->
            find_labels_1(Ops, RE);
        {match, [M]} ->
            [M | find_labels_1(Ops, RE)]
    end;
find_labels_1([], _) ->
    [].

number([H | T], N) ->
    S = iolist_to_binary(["L", integer_to_list(N)]),
    [{H, S} | number(T, N + 1)];
number([], _) ->
    [].

format_disasm([{_, _, Is} | Fs]) ->
    L = [lists:join(" ", I) || I <- Is],
    [lists:join("\n", L), "\n\n" | format_disasm(Fs)];
format_disasm([]) ->
    [].
