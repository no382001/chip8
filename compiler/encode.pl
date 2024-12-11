
% ----------------------------------------------------------------
% encode
% ----------------------------------------------------------------

encode_instruction(Base, Parts, Shifts, [HighByte, LowByte]) :-
    foldl(shift_and_mask, Parts, Shifts, Base, Instruction),
    HighByte is (Instruction >> 8) /\ 0xFF,
    LowByte is Instruction /\ 0xFF.

shift_and_mask(Part, Shift, Acc, Result) :-
    Result is Acc + (Part << Shift).

% clear the display (00e0)
encode(clear_display, [0x00, 0xE0]).

% return from a subroutine (00ee)
encode(return_from_subroutine, [0x00, 0xEE]).

% jump to address nnn (1nnn)
encode(jump_to_address(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0x1000, [Address], [0], Encoded).

% call subroutine at nnn (2nnn)
encode(call_subroutine(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0x2000, [Address], [0], Encoded).

% skip next instruction if vx == nn (3xnn)
encode(skip_if_vx_eq_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x3000, [X, Value], [8, 0], Encoded).

% skip next instruction if vx != nn (4xnn)
encode(skip_if_vx_not_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x4000, [X, Value], [8, 0], Encoded).

% skip next instruction if vx == vy (5xy0)
encode(skip_if_vx_eq_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x5000, [X, Y], [8, 4], Encoded).

% set vx to nn (6xnn)
encode(set_vx_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x6000, [X, Value], [8, 0], Encoded).

% add nn to vx (7xnn)
encode(add_nn_to_vx(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0x7000, [X, Value], [8, 0], Encoded).

% set vx to vy (8xy0)
encode(set_vx_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8000, [X, Y], [8, 4], Encoded).

% vx |= vy (8xy1)
encode(vx_or_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8001, [X, Y], [8, 4], Encoded).

% vx &= vy (8xy2)
encode(vx_and_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8002, [X, Y], [8, 4], Encoded).

% vx ^= vy (8xy3)
encode(vx_xor_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8003, [X, Y], [8, 4], Encoded).

% add vy to vx, set vf on carry (8xy4)
encode(vx_add_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8004, [X, Y], [8, 4], Encoded).

% vx -= vy, set vf on borrow (8xy5)
encode(vx_sub_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8005, [X, Y], [8, 4], Encoded).

% vx >>= 1, vf = old lsb (8xy6)
encode(vx_shift_right(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0x8006, [X], [8], Encoded).

% set vx to vy - vx, vf = 0 on borrow (8xy7)
encode(vx_set_vy_minus_vx(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x8007, [X, Y], [8, 4], Encoded).

% vx <<= 1, vf = old msb (8xye)
encode(vx_shift_left(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0x800E, [X], [8], Encoded).

% skip next instruction if vx != vy (9xy0)
encode(skip_if_vx_ne_vy(v(X), v(Y)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15,
    encode_instruction(0x9000, [X, Y], [8, 4], Encoded).

% set i to nnn (annn)
encode(set_index_register(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0xA000, [Address], [0], Encoded).

% jump to nnn + v0 (bnnn)
encode(jump_nnn_plus_v0(nnn(Address)), Encoded) :-
    Address >= 0, Address =< 0xFFF,
    encode_instruction(0xB000, [Address], [0], Encoded).

% set vx to random number and nn (cxnn)
encode(vx_rand_and_nn(v(X), nn(Value)), Encoded) :-
    X >= 0, X =< 15, Value >= 0, Value =< 255,
    encode_instruction(0xC000, [X, Value], [8, 0], Encoded).

% draw sprite at vx, vy with height n (dxyn)
encode(draw_sprite(v(X), v(Y), height(N)), Encoded) :-
    X >= 0, X =< 15, Y >= 0, Y =< 15, N >= 0, N =< 15,
    encode_instruction(0xD000, [X, Y, N], [8, 4, 0], Encoded).

% skip next instruction if key in vx is pressed (ex9e)
encode(kp_skip_if_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xE09E, [X], [8], Encoded).

% skip next instruction if key in vx is not pressed (exa1)
encode(kp_skip_if_not_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xE0A1, [X], [8], Encoded).

% set vx to delay timer value (fx07)
encode(set_vx_to_delay_timer(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF007, [X], [8], Encoded).

% wait for a key press, store in vx (fx0a)
encode(wait_for_kp_save_to_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF00A, [X], [8], Encoded).

% set delay timer to vx (fx15)
encode(set_delay_timer_to_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF015, [X], [8], Encoded).

% set sound timer to vx (fx18)
encode(set_sound_timer_to_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF018, [X], [8], Encoded).

% add vx to i (fx1e)
encode(add_vx_to_i(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF01E, [X], [8], Encoded).

% set i to location of sprite for digit vx (fx29)
encode(set_i_to_sprite_location(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF029, [X], [8], Encoded).

% store bcd of vx at i, i+1, i+2 (fx33)
encode(store_bcd_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF033, [X], [8], Encoded).

% store v0 to vx in memory starting at i (fx55)
encode(save_v0_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF055, [X], [8], Encoded).

% load memory at i into v0 to vx (fx65)
encode(load_v0_vx(v(X)), Encoded) :-
    X >= 0, X =< 15,
    encode_instruction(0xF065, [X], [8], Encoded).

% ----------------------------------------------------------------
% ir
% ----------------------------------------------------------------

% ir_load(+Dest, +Value, -InstrList)
ir_load(v(X),Value,InstrList) :-
    integer(Value),
    InstrList = [
      set_vx_nn(v(X),nn(Value))  
    ].

ir_load(v(Source), v(Source), []).

ir_load(v(Dest), v(Source), InstrList) :-
    InstrList = [
        set_vx_vy(v(Dest), v(Source))
    ].


% ir_add(+Dest, +Value, -InstrList)
ir_add(v(X), Value, InstrList) :-
    integer(Value),
    InstrList = [
    add_nn_to_vx(v(X), nn(Value))
    ].

ir_add(v(X), v(Y), InstrList) :-
    InstrList = [
        vx_add_vy(v(X), v(Y))
    ].

% ir_sub(+Dest, +Source, -InstrList)
ir_sub(v(X), v(Y), InstrList) :-
    InstrList = [
        vx_sub_vy(v(X), v(Y))
    ].

ir_sub(v(X), Value, InstrList) :-
    integer(Value),
    InstrList = [
        set_vx_nn(v(15), nn(Value)), % use VF as temp
        vx_sub_vy(v(X), v(15))
    ].

ir_eq(v(X), nn(Value), InstrList) :-
    gensym('ir_eq_label_', Label),
    InstrList = [
        set_vx_nn(v(15), nn(Value)), % change this later
        skip_if_vx_eq_vy(v(X), v(15)),
        set_vx_nn(v(15), nn(0)),
        goto(label(Label)),
        set_vx_nn(v(15), nn(1)),
        declare(label(Label))
    ].

ir_eq(v(X), v(Y), InstrList) :-
    InstrList = [
        set_vx_nn(v(15), nn(0)), % change this later
        skip_if_vx_eq_vy(v(X), v(Y)),
        set_vx_nn(v(15), nn(1))
    ].


:- dynamic ir_v_register/2.

find_free_register(Id, Register) :-
    ir_v_register(Id, Register).
find_free_register(Id,Register) :-
    between(0, 15, Register),
    \+ ir_v_register(_,Register),
    assertz(ir_v_register(Id,Register)). 

ir_translate_expr(num(Value), ResultReg, InstrList) :-
    find_free_register(Value,ResultReg),
    ir_load(v(ResultReg), Value, InstrList).

% check if variable exists
ir_translate_expr(var(Name), ResultReg, []) :-
    ir_v_register(Name, ResultReg).

ir_translate_expr(declare(var(Name)), ResultReg, InstrList) :-
    find_free_register(Name,ResultReg),
    ir_load(v(ResultReg), 0, InstrList). % 0 init variable

ir_translate_expr(assign(var(Name),Rhs), _, InstrList) :-
    ir_v_register(Name, VarAddress),
    ir_translate_expr(Rhs, ResRegRhs, RhsInstr),
    ir_load(v(VarAddress),v(ResRegRhs),LoadInstr),
    % retractall(ir_v_register(_,ResRegRhs)), % how do i handle this? check if its a used var?
    InstrList = [RhsInstr, LoadInstr].

ir_translate_expr(binop(+, Lhs, Rhs), ResRegLhs, InstrList) :-
    ir_translate_expr(Lhs, ResRegLhs, LhsInstr),
    ir_translate_expr(Rhs, ResRegRhs, RhsInstr),

    ir_add(v(ResRegLhs), v(ResRegRhs), AddInstr),
    
    retractall(ir_v_register(_,ResRegRhs)),
    
    InstrList = [LhsInstr, RhsInstr, AddInstr].

ir_translate_expr(binop(==, Lhs, num(N)), ResRegLhs, InstrList) :-
    ir_translate_expr(Lhs, ResRegLhs, LhsInstr),
    ir_eq(v(ResRegLhs), N, EqInstr),
    InstrList = [LhsInstr, EqInstr].

ir_translate_expr(binop(==, Lhs, Rhs), ResRegLhs, InstrList) :-
    ir_translate_expr(Lhs, ResRegLhs, LhsInstr),
    ir_translate_expr(Rhs, ResRegRhs, RhsInstr),

    ir_eq(v(ResRegLhs), v(ResRegRhs), EqInstr), % im skipping the non var cmp
    
    retractall(ir_v_register(_,ResRegRhs)),
    
    InstrList = [LhsInstr, RhsInstr, EqInstr].


ir_translate_expr(if_then_else(Condition, Then, Else), ResRegLhs, InstrList) :-
    ir_translate_expr(Condition, ResRegCond, CondInstr),

    ir_translate_expr(Then, ResRegLhs, ThenInstr),
    ir_translate_expr(Else, ResRegRhs, ElseInstr),
    gensym('if_then_else_label1_', Label1),
    
    (Else = []
        ->  InstrList = [
            CondInstr,
            skip_if_vx_eq_vy(v(ResRegCond), v(15)),
            goto(label(Label1)),
            ThenInstr,
            label(Label1)
        ]
        ;   retractall(ir_v_register(_,ResRegRhs)),
    
            gensym('if_then_else_label2_', Label2),
            CondCheckInstr = [
                skip_if_vx_eq_vy(v(ResRegCond), v(15)),
                goto(label(Label1)),
                ThenInstr,
                goto(label(Label2)),
                label(Label1),
                ElseInstr,
                label(Label2)
            ],
            %retractall(ir_v_register(_,ResRegCond)), % uhhm?
            InstrList = [CondInstr, CondCheckInstr]
    ).

ir_translate_expr(declare(label(Name)), _, [label(Name)]).
ir_translate_expr(goto(label(Name)), _, [goto(label(Name))]).
ir_translate_expr([], _, []) :- !.

collect_labels(Instr0, Labels, InstrRes) :-
    findall((Name, AdjustedIndex), (
        nth0(Index, Instr0, label(Name)),
        count_labels_before(Instr0, Index, Count),
        % adjust it by the already processed label count
        % this should sufficiently handle the index-physical address offsets
        AdjustedIndex is Index - Count
    ), Labels),

    exclude(is_label, Instr0, InstrRes).

is_label(label(_)).

count_labels_before(InstrList, Index, Count) :-
    findall(_, (
        nth0(I, InstrList, Instr),
        I < Index,
        is_label(Instr)
    ), LabelsBefore),
    length(LabelsBefore, Count).

resolve_gotos(Instr0, Labels, ResolvedInstr) :-
    maplist(resolve_goto(Labels), Instr0, ResolvedInstr).

resolve_goto(Labels, goto(label(Name)), goto(index(Address))) :-
    member((Name, Address), Labels). % still using the list index
resolve_goto(_, Instr, Instr).  % leave other instructions unchanged

resolve_jump_addresses(InstrList, ResolvedInstrList) :-
    maplist(resolve_goto_instruction, InstrList, ResolvedInstrList).

resolve_goto_instruction(goto(index(LineNum)), jump_to_address(nnn(ResolvedAddr))) :-
    ResolvedAddr is 0x200 + LineNum * 2 , !.
resolve_goto_instruction(Instr, Instr).

ir2(ResolvedInstr) :-  % this is not a valid program btw
    retractall(ir_v_register(_,_)),
    Ins = [
        declare(label('main')),
        declare(var(first)),
        binop(+,var(first),
            binop(+,num(2),num(2))),

        declare(var(second)),
        binop(+,var(second),
            binop(+,num(1),num(1))),

        binop(+,var(first),var(second)),
        goto(label('main'))
    ], !,
    maplist(ir_translate_expr, Ins, _ , Rs),
    flatten(Rs, R0),
    collect_labels(R0, LabelMap, R1), % this fuck is misaligned
    resolve_gotos(R1, LabelMap, ResolvedInstr),

    print_formatted_instructions(ResolvedInstr).

ir3(ResolvedInstr) :- 
    retractall(ir_v_register(_,_)),
    Ins = [
        declare(var(x)),
        declare(var(y)),
        declare(label('main')),
        assign(var(x),binop(+,var(x), num(1))),
        if_then_else( % something wrong with the implementation
            binop(==, var(x), num(5)),
            assign(var(y),binop(+, var(y), num(1))),
            []),
        goto(label('main'))
    ], !,
    maplist(ir_translate_expr, Ins, _ , Rs),
    flatten(Rs, R0),
    collect_labels(R0, LabelMap, R1),
    resolve_gotos(R1, LabelMap, R2),
    resolve_jump_addresses(R2,ResolvedInstr),
    print_formatted_instructions(ResolvedInstr).

ir_translate_stmt(declaration(X, num(Value)), InstrList) :-
    % variable does not exists and there is a free register
    \+ ir_v_register(X, _), find_free_register(FreeReg),
    assertz(ir_v_register(X,FreeReg)),
    ir_load(v(FreeReg),Value,InstrList). % load the initial value

ir_translate_stmt(assign(X, num(Value)), InstrList) :-
    ir_v_register(X, Register), % variable is declared beforehand
    ir_load(v(Register),Value,InstrList). % load the new value

% ----------------------------------------------------------------
% main
% ----------------------------------------------------------------

generate_binary :-
    ir3(Program0),
    maplist(encode, Program0, EncodedList),
    flatten(EncodedList, Binary),
    export_binary("programs/test.ch8",Binary).


write_padding(_, 0).
write_padding(Stream, N) :-
    N > 0,
    put_byte(Stream, 0),
    N1 is N - 1,
    write_padding(Stream, N1).

write_all(_, []).
write_all(Stream, [Byte | Rest]) :-
    put_byte(Stream, Byte),
    write_all(Stream, Rest).

export_binary(Filename, Binary) :-
    open(Filename, write, Stream, [type(binary)]),
    %write_padding(Stream, 0x200),   % pad 512 bytes of zero
    write_all(Stream, Binary),
    close(Stream).

% ----------------------------------------------------------------
% printing
% ----------------------------------------------------------------

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
