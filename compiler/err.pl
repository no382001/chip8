:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

program(Program) --> statements(Program), !.

statements([X|Xs]) --> statement(X), ws, statements(Xs).
statements([]) --> [].

ws --> whitespace, !, ws.
ws --> [].

whitespace --> [W], { char_type(W, white) ; W = 10}.

statement(assign(Var, Expr)) --> variable(Var), ws, "=", ws, expression(Expr), ";", ws.

expression(num(N)) --> integer(N).
expression(var(Var)) --> variable(Var).
expression(binop(Op, Lhs, Rhs)) --> term(Lhs), ws, operator(Op), ws, expression(Rhs).

hex_number(N) -->
    "0x", hex_digits(Digits),
    { atom_codes(Atom, [0'0, 0'x | Digits]),
      atom_number(Atom, N) }.

hex_digits([D|Ds]) --> hex_digit(D), hex_digits(Ds).
hex_digits([D]) --> hex_digit(D).

hex_digit(D) --> [D], { 
    (char_type(D, digit) ; member(D, `abcdefABCDEF`))
}.

term(num(N)) --> integer(N).
term(num(N)) --> hex_number(N).
term(var(Var)) --> variable(Var).

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".
operator(==) --> "==".
operator(<) --> "<".
operator(>) --> ">".

variable(Var) -->
    [C], { char_type(C, alpha) },
    variable_chars(Cs),
    { atom_codes(Var, [C|Cs]) }.

variable_chars([C|Cs]) --> [C], { char_type(C, alnum) }, variable_chars(Cs).
variable_chars([]) --> [].

run_test(TestName, Input, Expected) :-
    string_codes(Input, InputCodes),
    ( phrase(program(AST), InputCodes),
      AST = Expected -> 
        format("~w: passed~n", [TestName])
    ; 
        format("~w: failed~nexpected: ~w~ngot: ~w~n", [TestName, Expected, AST])
    ).

repl :-
    nl,
    read_line_to_string(user_input, Input),
    ( Input = "exit" -> % `exit` to quit 
        write('exiting repl...'), nl
    ;   string_codes(Input, InputCodes),
        ( phrase(program(AST), InputCodes) -> 
            write('parsed ast: '), write(AST), nl
        ;   write('error'), nl
        ),
        repl
    ).

main :-
    run_test("test 1: variable assignment", "r = 5;", [assign(r, num(5))]).


parse_file(FileName, AST) :-
    open(FileName, read, Stream),
    read_string(Stream, _, FileContent),
    close(Stream),
    string_codes(FileContent, Codes),
    phrase(program(AST), Codes).
