:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

program(Program) --> statements(Program), !.

statements([X|Xs]) --> statement(X), ws, statements(Xs).
statements([]) --> [].

ws --> [W], { char_type(W, white) }, !, ws.
ws --> [].

statement(assign(Var, Expr)) --> variable(Var), ws, "=", ws, expression(Expr), ";", ws.
statement(declaration(Var, Value)) --> "auto", ws, variable(Var), ws, "=", ws, expression(Value), ";", ws.
statement(while(Cond, Body)) --> "while(", ws, expression(Cond), ")", ws, block(Body).
statement(if_else(Cond, IfBody, ElseBody)) --> 
    "if(", ws, expression(Cond), ")", ws, block(IfBody), ws, 
    "else", ws, block(ElseBody).
statement(funcall(Func, Args)) --> function_call(Func, Args), ";", ws.

block(Body) --> "{", ws, statements(Body), "}", ws.

expression(num(N)) --> integer(N).
expression(var(Var)) --> variable(Var).
expression(binop(Op, Lhs, Rhs)) --> term(Lhs), ws, operator(Op), ws, expression(Rhs).

term(num(N)) --> integer(N).
term(var(Var)) --> variable(Var).

array([X|Xs]) --> term(X), ws, "," , array(Xs).
array([]) --> [].

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".
operator(==) --> "==".
operator(<) --> "<".
operator(>) --> ">".

variable(Var) --> string_without(" \t\n=;(){},", Cs), { atom_codes(Var, Cs) }.

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
    nl, % `exit` to quit
    read_line_to_string(user_input, Input),
    ( Input = "exit" -> 
        write('exiting repl...'), nl
    ;   string_codes(Input, InputCodes),
        ( phrase(program(AST), InputCodes) -> 
            write('parsed ast: '), write(AST), nl
        ;   write('error'), nl
        ),
        repl
    ).

main :-
    run_test("test 1: variable assignment", "r = 5;", [assign(r, num(5))]),
    
    run_test("test 2: variable declaration", "auto x = 10;", [declaration(x, num(10))]),
    
    run_test("test 3: while loop", "while(x < 10) { x = x + 1; }",
             [while(binop(<, var(x), num(10)), [assign(x, binop(+, var(x), num(1)))] )]),
    
    run_test("test 4: if-else statement", "if(x == 5) { y = 1; } else { y = 0; }",
             [if_else(binop(==, var(x), num(5)), [assign(y, num(1))], [assign(y, num(0))])]),
    
    run_test("test 5: function call", "draw(x, y);", [funcall(draw, [var(x), var(y)])]),
    
    run_test("test 6: block with multiple statements", "auto x = 0; auto y = 1;",
             [declaration(x, num(0)), declaration(y, num(1))]),
    
    run_test("test 7: complex assignment", "result = x + y * z;", 
             [assign(result, binop(+, var(x), binop(*, var(y), var(z))))]),
    
    run_test("test 8: array assignment", "r = { 0x80, 0x40, 0x20, 0x10 };",
             [assign(r, [num(128), num(64), num(32), num(16)])]),

    repl.
