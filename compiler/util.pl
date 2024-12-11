
% ----------------------------------------------------------------
% printing
% ----------------------------------------------------------------

:- module(util, [print_binary/1, print_formatted_instructions/1]).

print_binary([]).
print_binary([HighByte, LowByte | Rest]) :-
    format("0x~|~`0t~16R~2+ 0x~|~`0t~16R~2+\n", [HighByte, LowByte]),
    print_binary(Rest).

format_instructions(Input, Formatted) :-
    maplist(format_instruction, Input, FormattedList),
    atomic_list_concat(FormattedList, '\n', Formatted).

format_instruction(Instr, Formatted) :-
    format(atom(Formatted), '   ~w,', [Instr]).

print_formatted_instructions(Input) :-
    format_instructions_with_hex_line_numbers(Input, Formatted),
    format("[\n~w\n]", [Formatted]).

format_instructions_with_hex_line_numbers(Instructions, Formatted) :-
    numbered_instruction_with_hex(Instructions, 0x200, NumberedInstructions),
    atomic_list_concat(NumberedInstructions, '\n', Formatted).

numbered_instruction_with_hex([], _, []).
numbered_instruction_with_hex([Instr | Rest], Addr, [Formatted | FormattedRest]) :-
    format_instruction_with_hex(Instr, FormattedInstr),
    format(atom(Formatted), '0x~|~`0t~16R~4+ ~w', [Addr, FormattedInstr]),
    NextAddr is Addr + 2,
    numbered_instruction_with_hex(Rest, NextAddr, FormattedRest).

format_instruction_with_hex(Instruction, Formatted) :-
    Instruction =.. [Functor | Args], % decompose the term
    maplist(format_argument_hex, Args, HexArgs), % format each argument
    atomic_list_concat(HexArgs, ', ', JoinedArgs), % join arguments into a single string
    format(atom(Formatted), '~w(~w)', [Functor, JoinedArgs]). % combine functor and arguments into one string

format_argument_hex(Arg, Formatted) :-
    ( integer(Arg) ->
        % format integer as hex
        format(atom(Formatted), '0x~|~`0t~16R~2+', [Arg])
    ; Arg =.. [Functor | SubArgs], % handle compound terms like v(0)
        maplist(format_argument_hex, SubArgs, HexSubArgs), % recursively format sub-arguments
        atomic_list_concat(HexSubArgs, ',', JoinedSubArgs), % join sub-arguments
        format(atom(Formatted), '~w(~w)', [Functor, JoinedSubArgs])
    ; % handle other terms as strings
        format(atom(Formatted), '~w', [Arg])
    ).
