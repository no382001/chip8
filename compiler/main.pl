
:- use_module(ir).
:- use_module(encode).

% ----------------------------------------------------------------
% currently testing
% ----------------------------------------------------------------

ir3(ResolvedInstr) :- 
    retractall(ir_v_register(_,_)),
    Ins = [
        declare(var(x)),
        declare(var(y)),
        declare(label('main')),
        assign(var(x),binop(+,var(x), num(1))),
        if_then_else(
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

ir4(ResolvedInstr) :- 
    retractall(ir_v_register(_,_)),
    Ins = [
        declare(var(x)),
        declare(var(y)),
        declare(label('main')),
        assign(var(x),binop(+,var(x), num(1))),
        if_then_else(
            binop(==, var(x), num(5)),
            assign(var(y),binop(+, var(y), num(1))),
            assign(var(y),num(12))),
        goto(label('main'))
    ], !,
    maplist(ir_translate_expr, Ins, _ , Rs),
    flatten(Rs, R0),
    collect_labels(R0, LabelMap, R1),
    resolve_gotos(R1, LabelMap, R2),
    resolve_jump_addresses(R2,ResolvedInstr),
    print_formatted_instructions(ResolvedInstr).

ir5(ResolvedInstr) :- 
    retractall(ir_v_register(_,_)),
    Ins = [
        declare(var(x)),
        declare(var(y)),
        declare(label('main')),
        assign(var(x),binop(+,var(x), num(1))),
        if_then_else(
            binop(==, var(x), num(5)),
            assign(var(y),binop(+, var(y), num(1))),
            assign(var(y),num(12))),
        draw_sprite(var(x),var(y),num(4),label('rect')),
        goto(label('main')),
        sprite('rect',[[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,1,1,1]])
    ], !,
    maplist(ir_translate_expr, Ins, _ , Rs),
    flatten(Rs, R0),
    collect_labels(R0, LabelMap, R1),
    resolve_gotos(R1, LabelMap, R2),
    print_formatted_instructions(R2),
    resolve_jump_addresses(R2,ResolvedInstr),
    print_formatted_instructions(ResolvedInstr).


test_ir_complex(ResolvedInstr) :-
    retractall(ir_v_register(_, _)),  % Clear register tracking
    Ins = [
        % Declare variables
        declare(var(x)),
        declare(var(y)),
        declare(var(z)),
        declare(var(a)),
        declare(var(b)),
        declare(var(c)),
        declare(label('main')),        % Loop start

        % Increment x and y
        assign(var(x), binop(+, var(x), num(1))), % x = x + 1
        assign(var(y), binop(+, var(y), num(2))), % y = y + 2

        % Compute z = x + y
        assign(var(z), binop(+, var(x), var(y))),

        % Perform conditional operations
        if_then_else(
            binop(==, var(z), num(15)),           % if z == 15
            [assign(var(a), num(5)),              % a = 5
                assign(var(b), binop(+, var(a), num(3)))], % b = a + 3
            [assign(var(c), binop(-, var(z), num(7)))]   % else c = z - 7
        ),

        % Draw sprite if a certain condition is met
        if_then_else(
            binop(>, var(c), num(0)),             % if c > 0
            [draw_sprite(var(x), var(c), num(4), label('sprite'))], % Draw sprite
            [draw_sprite(var(x), var(y), num(4), label('sprite'))]  % Else draw at x, y
        ),

        % Loop back to the main label
        goto(label('main')),

        % Define sprite
        sprite('sprite', [[1, 1, 1, 1], [1, 0, 0, 1], [1, 0, 0, 1], [1, 1, 1, 1]])
    ],
    !,
    maplist(ir_translate_expr, Ins, _, Rs), !,
    flatten(Rs, R0),
    collect_labels(R0, LabelMap, R1),
    resolve_gotos(R1, LabelMap, R2),
    resolve_jump_addresses(R2, ResolvedInstr),
    print_formatted_instructions(ResolvedInstr).

% ----------------------------------------------------------------
% main
% ----------------------------------------------------------------

genbin :-
    test_ir_complex(Program0), !,
    maplist(encode, Program0, EncodedList),
    flatten(EncodedList, Binary),
    export_binary("programs/test.ch8",Binary),
    shell('../vm/a.out -r programs/test.ch8').

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