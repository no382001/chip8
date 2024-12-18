:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

program(Program) --> ws, statements(Program), !.

statements([X|Xs]) --> statement(X), ws, statements(Xs).
statements([]) --> ws.

ws --> whitespace, !, ws.
ws --> comment, !, ws.
ws --> [].

whitespace --> [W], { char_type(W, white) ; W = 10}.

comment --> "//", rest_of_line.

rest_of_line --> [C], { C \= 10 }, !, rest_of_line.
rest_of_line --> [10] | [].

declaration_list([assign(Var, Expr) | Rest]) -->
    variable(Var), ws, "=", ws, expression(Expr), ws, ",", ws, declaration_list(Rest).
declaration_list([assign(Var, Expr)]) -->
    variable(Var), ws, "=", ws, expression(Expr), ws.

% revisit this w/ assign_op
statement(assign(Var, Expr)) --> variable(Var), ws, "=", ws, expression(Expr), ws, ";", ws.
statement(declaration(Var, Value)) --> keyword, ws, variable(Var), ws, "=", ws, expression(Value), ";", ws.
statement(declaration_list(Declarations)) --> keyword, ws, declaration_list(Declarations), ";", ws.
statement(while(Cond, Body)) --> "while", ws, "(", ws, expression(Cond), ")", ws, block(Body).
statement(if_else(Cond, IfBody, ElseBody)) --> if_statement(Cond, IfBody), branching(ElseBody).
statement(fundecl(Func, Args, Body)) --> function_call(Func, Args), block(Body), ws.
statement(funcall(Func, Args)) --> function_call(Func, Args), ws, ";", ws.

keyword --> "auto".
keyword --> "var".
keyword --> "const".
keyword --> "memory".

if_statement(Cond, IfBody) -->
    "if", ws , "(", ws, expression(Cond), ")", ws, block(IfBody), ws.

branching(ElseBody) -->
    "else", ws, block(ElseBody).
branching(if_else(Cond,IfBody,ElseBody)) -->
    "else", ws, if_statement(Cond,IfBody), ws, branching(ElseBody).
branching([]) --> [].

block(Body) --> "{", ws, statements(Body), "}", ws.

expression(num(N)) --> integer(N).
expression(num(N)) --> hex_number(N).
expression(var(Var)) --> variable(Var).
expression(binop(Op, Lhs, Rhs)) --> term(Lhs), ws, operator(Op), ws, expression(Rhs).
expression(X) --> enumeration(X).
expression(funcall(Func,Args)) --> function_call(Func,Args).

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

enumeration(X) --> "{", ws, array(X), "}", ws.

array([X|Xs]) --> term(X), ws, ",", ws, array(Xs).
array([X]) --> term(X), ws.

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".
operator(<) --> "<".
operator(>) --> ">".
operator(==) --> "==".

assign_operator(=) --> "=".
assign_operator(-=) --> "-=".
assign_operator(+=) --> "+=".

variable(Var) -->
    [C], { char_type(C, alpha) },
    variable_chars(Cs),
    { atom_codes(Var, [C|Cs]) }.

variable_chars([C|Cs]) --> [C], { char_type(C, alnum) }, variable_chars(Cs).
variable_chars([]) --> [].

% do i need this at all? probably not, just for main maybe, i could just have labels instead
% and aliases of course
function_call(Func, Args) --> 
    variable(Func), "(", ws, arguments(Args), ")", ws.

arguments([Arg | Rest]) --> expression(Arg), ",", ws, arguments(Rest).
arguments([Arg]) --> expression(Arg).
arguments([]) --> [].

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
    run_test("test 1: variable assignment",
        "r = 5; r = 0xff ;",
        [assign(r, num(5)), assign(r, num(255))]),
    
    run_test("test 2: variable declaration",
        "auto x = 10;",
        [declaration(x, num(10))]),
    
    run_test("test 3: while loop",
        "while (x < 10) { x = x + 1; }",
        [while(binop(<, var(x), num(10)), [assign(x, binop(+, var(x), num(1)))] )]),
    
    run_test("test 4.0: if statement",
        "if (x == 5) { y = 1; }",
        [if_else(binop(==, var(x), num(5)), [assign(y, num(1))],[])]),

    run_test("test 4.1: if-else statement",
        "if (x == 5) { y = 1; } else { y = 0; }",
        [if_else(binop(==, var(x), num(5)), [assign(y, num(1))], [assign(y, num(0))])]),
    
    run_test("test 4.2: if-else-if statement",
        "if (x == 5) { y = 1; } else if (x == 4) { y = 0; }",
        [if_else(
            binop(==,var(x),num(5)),
            [assign(y,num(1))],
            if_else(
                binop(==,var(x),num(4)),
                [assign(y,num(0))],[]))]),
        

    run_test("test 5: function call",
        "draw(x, y);",
        [funcall(draw, [var(x), var(y)])]),
    
    run_test("test 6: block with multiple statements",
        "auto x = 0; auto y = 1;",
        [declaration(x, num(0)), declaration(y, num(1))]),
    
    run_test("test 7: complex assignment",
        "result = x + y * z;", 
        [assign(result, binop(+, var(x), binop(*, var(y), var(z))))]),
    
    run_test("test 8: array assignment",
        "r = { 0x80, 0x40, 0x20, 0x10 };",
        [assign(r, [num(128), num(64), num(32), num(16)])]),

    run_test("test 9.0: comment",
        "r = { 0x80, 0x40, 0x20, 0x10 }; // this is a comment",
        [assign(r, [num(128), num(64), num(32), num(16)])]),

    run_test("test 9.1: full line comment",
        " // this is a full line",
        []).


parse_file(FileName, AST) :-
    open(FileName, read, Stream),
    read_string(Stream, _, FileContent),
    close(Stream),
    string_codes(FileContent, Codes),
    phrase(program(AST), Codes).
