:- module(ir, [collect_labels/3,resolve_gotos/3,resolve_jump_addresses/2,print_formatted_instructions/1,ir_translate_expr/3]).

:- use_module(encode).
:- use_module(util).


% ----------------------------------------------------------------
% ir primitives
% ----------------------------------------------------------------

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


% ir_add(-Dest, -Value, +InstrList)
ir_add(v(X), Value, InstrList) :-
    integer(Value),
    InstrList = [
        add_nn_to_vx(v(X), nn(Value))
    ].

ir_add(v(X), v(Y), InstrList) :-
    InstrList = [
        vx_add_vy(v(X), v(Y))
    ].

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
        set_vx_nn(v(15), nn(Value)),
        skip_if_vx_eq_vy(v(X), v(15)),
        goto(label(Label)),
        set_vx_nn(v(15), nn(1)),
        label(Label)
    ].

ir_eq(v(X), v(Y), InstrList) :-
    gensym('ir_eq_label_', Label),
    InstrList = [
        skip_if_vx_eq_vy(v(X), v(Y)),
        goto(label(Label)),
        set_vx_nn(v(15), nn(1)), % i could use a dedicated variable, or memory...
        label(Label)
    ].

ir_draw_sprite(v(X),v(Y),n(Height),label(Name),InstrList) :-
    InstrList = [
        set_index_register(label(Name)), % replace this later in the label pass
        draw_sprite(v(X), v(Y), n(Height))
    ].

% ----------------------------------------------------------------
% ir resolve
% ----------------------------------------------------------------

bit_accumulator(Bit, Acc, Result) :-
    Result is (Acc << 1) + Bit.

row_to_byte(Row, rawbyte(Byte)) :-
    foldl(bit_accumulator, Row, 0, Byte).

% this needs many other cases, not just v,v,num
ir_translate_expr(draw_sprite(var(X),var(Y),num(Height),label(Name)),_,InstrList) :-
    ir_v_register(X, XReg),
    ir_v_register(Y, YReg),
    ir_draw_sprite(v(XReg),v(YReg),n(Height),label(Name),InstrList).

ir_translate_expr(sprite(Name,TwoDList), _, InstrList) :-
    maplist(row_to_byte, TwoDList, RawBytes), % !! max 8 pixels per row !!
    InstrList = [
        label(Name),
        RawBytes
    ].

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

    ir_eq(v(ResRegLhs), v(ResRegRhs), EqInstr),
    
    retractall(ir_v_register(_,ResRegRhs)),
    
    InstrList = [LhsInstr, RhsInstr, EqInstr].


ir_translate_expr(if_then_else(Condition, Then, []), ResRegLhs, InstrList) :-
    Condition = binop(==, _, _),
    ir_translate_expr(Condition, _, CondInstr), % the result is in VF

    ir_translate_expr(Then, ResRegLhs, ThenInstr),
    gensym('if_then_else_label1_', Label1),
    
    find_free_register(Label1, Register), % name it anything
    InstrList = [
            CondInstr,
            set_vx_nn(v(Register),nn(1)),
            skip_if_vx_eq_vy(v(Register), v(15)),
            goto(label(Label1)),
            ThenInstr,
            label(Label1)
        ],
    retractall(ir_v_register(_,Register)).

ir_translate_expr(if_then_else(Condition, Then, Else), ResRegLhs, InstrList) :-
    Condition = binop(==, _, _),
    ir_translate_expr(Condition, _, CondInstr),

    ir_translate_expr(Then, ResRegLhs, ThenInstr),
    ir_translate_expr(Else, _, ElseInstr),
    gensym('if_then_else_label1_', Label1), % this is not a reliable way
    
    %retractall(ir_v_register(_,ResRegRhs)),
    
    gensym('if_then_else_label2_', Label2),
    find_free_register(Label1, Register), % name it anything
    InstrList = [
        CondInstr,
        set_vx_nn(v(Register),nn(1)),
        skip_if_vx_eq_vy(v(Register), v(15)),
        goto(label(Label1)),
        ThenInstr,
        goto(label(Label2)),
        label(Label1),
        ElseInstr,
        label(Label2),
        set_vx_nn(v(15), nn(0)) % dont forget to clean up
    ],
    retractall(ir_v_register(_,Register)).
    %retractall(ir_v_register(_,ResRegCond)), % uhhm?


ir_translate_expr(declare(label(Name)), _, [label(Name)]).
ir_translate_expr(goto(label(Name)), _, [goto(label(Name))]).
ir_translate_expr([], _, []) :- !.

% ----------------------------------------------------------------
% registers
% ----------------------------------------------------------------

:- dynamic ir_v_register/2.

find_free_register(Id, Register) :-
    ir_v_register(Id, Register).
find_free_register(Id,Register) :-
    between(0, 15, Register),
    \+ ir_v_register(_,Register),
    assertz(ir_v_register(Id,Register)). 

% ----------------------------------------------------------------
% labels
% ----------------------------------------------------------------

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
    member((Name, Address), Labels), !. % still using the list index
resolve_goto(Labels, set_index_register(label(Name)), set_index_register(index(Address))) :-
    member((Name, Address), Labels), !.
resolve_goto(_, Instr, Instr).  % leave other instructions unchanged

resolve_jump_addresses(InstrList, ResolvedInstrList) :-
    maplist(resolve_goto_instruction, InstrList, ResolvedInstrList).

resolve_goto_instruction(goto(index(LineNum)), jump_to_address(nnn(ResolvedAddr))) :-
    ResolvedAddr is 0x200 + LineNum * 2 , !.
resolve_goto_instruction(set_index_register(index(LineNum)), set_index_register(nnn(ResolvedAddr))) :-
    ResolvedAddr is 0x200 + LineNum * 2 , !.
resolve_goto_instruction(Instr, Instr).
