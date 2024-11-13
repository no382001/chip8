:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

program(Program) --> statements(Program), !.

statements([X|Xs]) --> statement(X), ws, statements(Xs).
statements([]) --> [].

ws --> whitespace, !, ws.
ws --> [].

whitespace --> [W], { char_type(W, white) ; W = \n}.

statement(assign(Var, Expr)) --> variable(Var), ws, [=], ws, expression(Expr), [;], ws.

expression(Term) --> term(Term).
expression(binop(Op, Lhs, Rhs)) --> term(Lhs), ws, operator(Op), ws, expression(Rhs).

hex_number(N) -->
    ['0', x], hex_digits(Digits),
    { Digits == error(invalid_hex)
      -> N = error(invalid_hex)
      ; atom_codes(Atom, Digits),
        atom_concat('0x', Atom, HexAtom),
        atom_number(HexAtom, N)
    }.

hex_digits(Digits) -->
    hex_digit_list(Digits), { \+ member(error(_), Digits) }, !.
hex_digits(error(invalid_hex)) --> hex_digit_list(_).

hex_digit_list([D|Ds]) --> hex_digit(D), hex_digit_list(Ds).
hex_digit_list([]) --> [].

hex_digit(D) --> [D], { is_hex_digit(D) }.
hex_digit(error(invalid_hex_digit(D))) --> [D], { \+ is_hex_digit(D) }.

is_hex_digit(D) :-
    char_type(D, digit) ; member(D, "abcdefABCDEF").


term(num(N)) --> integer(N).
term(num(N)) --> hex_number(N).
term(var(Var)) --> variable(Var).

operator(+) --> [+].
operator(-) --> [-].
operator(*) --> [*].
operator(/) --> [/].
operator(==) --> [=,=].
operator(<) --> [<].
operator(>) --> [>].
operator(error(illegal_operator(Op))) --> [Op].

variable(Var) -->
    [C], { char_type(C, alpha) },
    variable_chars(Cs),
    { atom_codes(Var, [C|Cs]) }.

variable_chars([C|Cs]) --> [C], { char_type(C, alnum) }, variable_chars(Cs).
variable_chars([]) --> [].

run_test(TestName, Input, Expected) :-
    atom_chars(Input, CharList),
    ( phrase(program(AST), CharList),
      AST = Expected -> 
        format("~w: passed~n", [TestName])
    ; 
        format("----~w: failed~n----expected: ~w~n----got: ~w~n", [TestName, Expected, AST])
    ).

repl :-
    nl,
    read_line_to_string(user_input, Input),
    ( Input = "exit" -> % `exit` to quit 
        write('exiting repl...'), nl
    ;   atom_chars(Input, CharList),
        ( phrase(program(AST), CharList) -> 
            write('parsed ast: '), write(AST), nl
        ;   write('error'), nl
        ),
        repl
    ).

main :-
    run_test("test 1: assignment",
        "r = 0x5f; p = 5;", [assign(r, num(95)),assign(p, num(5))]),
    run_test("test 2: op",
        "r = 1 + 0x5f; r = 7 & 1;", [assign(r,binop(+,num(1),num(95))),assign(r,binop(error(illegal_operator(&)),num(7),num(1)))]).


parse_file(FileName, AST) :-
    open(FileName, read, Stream),
    read_string(Stream, _, FileContent),
    close(Stream),
    string_codes(FileContent, Codes),
    phrase(program(AST), Codes).
