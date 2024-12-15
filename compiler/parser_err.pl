:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

ws --> whitespace, !, ws.
ws --> comment, !, ws.
ws --> [].

whitespace --> [W], { char_type(W, white) ; W = 10}.

comment --> "//", !, rest_of_line.

rest_of_line --> [C], !, { C \= 10 }, rest_of_line.
rest_of_line --> [10] | [].

nonempty([T|Ts]) --> [T], anything(Ts).
anything([T|Ts]) --> [T], !, anything(Ts).
anything([]) --> [].

% ----------------------------
% bleh
% ----------------------------

variable(Var) -->
    variable_helper(Var), { Var \= '' }.
variable_helper(Var) -->
    [C], { char_type(C, alpha) },
    variable_chars(Cs),
    { atom_codes(Var, [C|Cs]) }.
variable_helper('') --> [].

variable_chars([C|Cs]) --> [C], { char_type(C, alnum) }, variable_chars(Cs).
variable_chars([]) --> [].

% ----------------------------
% 1. statements
% ----------------------------

statements(Statements) -->
    statements([],Sts), {reverse(Sts,Statements)}.

% Bs represents the semantics defined up to this point -- in reversed order,
% Stms is the actual semantics
statements(Bs,Stms) --> ws, statement(Bs,X), !, ws, statements([X|Bs],Stms).
statements(Bs,Stms) -->
    nonempty(Tokens), !, { string_codes(Ts, Tokens), Stms = [error('syntax error',Ts)|Bs] }.
statements(Bs,Bs) --> ws.


is_declared(Bs,Var) :-
    member(declare(Var), Bs) ; member(declassign(Var, _), Bs).

% ----------------------------
% 1.1 declaration and assignment
% ----------------------------

statement(Bs,Stms) -->
    % simple assignment
    variable(Var), ws, "=", ws, expression(Bs,Expr), ws, ";", ws, !,
    {
        is_declared(Bs,Var)
            ->  Stms = assign(Var, Expr)
            ;   Stms = error('assignment before declaration',[Var,=,Expr])
    }.

statement(Bs,Stms) -->
    % simple declaration
    "byte", ws, variable(Var), ws, ";", !,
    {
        is_declared(Bs,Var)
        ->  Stms = error('redeclaration of variable',Var)
        ;   Stms = declare(Var)
    }.

statement(Bs,Stms) -->
    % declaration with value
    "byte", ws, variable(Var), ws, "=", ws, expression(Bs,Expr), ws, ";", ws, !,
    {
        is_declared(Bs,Var)
        ->  Stms = error('redeclaration of variable',[Var,=,Expr])
        ;   Stms = declassign(Var, Expr)
    }.


% ----------------------------
% 1.2 if-then-else
% ----------------------------

statement(Bs, if_then_else(Cond, ThenStms, ElseStms)) -->
    "if", ws, "(", ws, expression(Bs, Cond), ws, ")", ws, "{", ws,
    block_statements(Bs, ThenStms), ws, "}", ws,
    ( "else", ws, "{", ws, block_statements(Bs, ElseStms), ws, "}" -> []
    ; { ElseStms = [] } ), !.

% this is the same as statements/4 but without nonempty/3
% also any `statement level syntax error` is ignored
block_statements(Bs, Statements) -->
    ws, statement(Bs, Stm), ws, { append(Bs, [Stm], UpdatedBs) }, block_statements(UpdatedBs, Statements).
block_statements(Bs, Bs) --> ws.

% ----------------------------
% 2. expression
% ----------------------------

expression(_,Num) --> integer(N),
    {
        between(0, 255, N)
        ->  Num = num(N)
        ;   Num = error('byte representation failed',N)
    }.
expression(Bs,Stm) --> variable(Var),
    {
        is_declared(Bs,Var)
        -> Stm = var(Var)
        ;  Stm = error('use while-in/before declaration',var(Var))
    }.

expression(_, error('invalid expression', Ts)) -->
    (nonempty(Tokens) -> { string_codes(Ts, Tokens) }
    ; { Ts = "empty expression" }).


expression(Bs,Result) -->
    expression(Bs,Lhs), !, ws, operator(Op), !, ws, expression(Bs,Rhs),
    {
        Result = binop(Op, Lhs, Rhs)
    }.

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".
operator(mod) --> "mod".
operator(<) --> "<".
operator(>) --> ">".
operator(==) --> "==".
operator(error('invalid operator',C)) --> [C]. % this is not complete

% ----------------------------
% main
% ----------------------------

parse(S,R) :-
    string_codes(S, SC),
    statements(R,SC,[]).

run_test(Name, Input, Expected) :-
    parse(Input,R),
    (   R = Expected
        ->  ansi_format([fg(green)], "pass: ", []), format("~w~n", [Name])
        ;   ansi_format([fg(red)], "failed: ",[]),
            format("~w: -> `~w`~n", [Name,Input]),
            ansi_format([fg(red)], "\texpected: ",[]),
            format("`~w`~n",[Expected]),
            ansi_format([fg(red)],"\tgot: ",[]),
            format("`~w`~n", [R])
    ).

tests :-
    format("-- section: declaration and assignment --~n", []),
    run_test("bare semicolon",
        ";",
        [error('syntax error',";")]),
    run_test("empty",
        "",
        []),
    run_test("assign non existing",
        "v = 1;",
        [error('assignment before declaration', [v, =, num(1)])]),
    run_test("simple decl",
        "byte v;",
        [declare(v)]),
    run_test("bare byte",
        "byte",
        [error('syntax error', "byte")]),
    run_test("declassign",
        "byte v = 1;",
        [declassign(v, num(1))]),
    run_test("declassign ws",
        " byte   x   =  5   ; ",
        [declassign(x, num(5))]),        
    run_test("barely 1byte",
        "byte v = 255;",
        [declassign(v, num(255))]),
    run_test("way more than 1byte",
        "byte v = 123123;",
        [declassign(v,error('byte representation failed',123123))]),
    run_test("declassign binop",
        "byte v = 1 + 1 + 1 + 1;",
        [declassign(v, binop(+, num(1), binop(+, num(1), binop(+, num(1), num(1)))))]),
    run_test("referencing itself in declaration",
        "byte v = 1 + 1 + 1 + v;",
        [declassign(v, binop(+, num(1), binop(+, num(1), binop(+, num(1), error('use while-in/before declaration', var(v))))))]),
    run_test("redecl",
        "byte x; byte x = 5;",
        [declare(x), error('redeclaration of variable', [x, =, num(5)])]),
    run_test("no delc and ref to non existant",
        "x = 1 + y;",
        [error('assignment before declaration', [x, =, binop(+, num(1), error('use while-in/before declaration', var(y)))])]),
    run_test("simple op",
        "byte x = 10; x = x + 5;",
        [declassign(x, num(10)), assign(x, binop(+, var(x), num(5)))]),
    run_test("just byte",
        "byte;",
        [error('syntax error',"byte;")]),
    run_test("not finished",
        "x =",
        [error('syntax error',"x =")]),
    run_test("almost finished",
        "byte x = ;",
        [error('syntax error', "byte x = ;")]),
    run_test("invalid op",
        "byte x = 1 @ 2;",
        [declassign(x, binop(error('invalid operator', 64), num(1), num(2)))]),

    format("-- section: if-then-else --~n", []),
    run_test("if-then empty",
        "if(1){}",
        [if_then_else(num(1), [], [])]),
    run_test("if-then simple",
        "if(1){ byte x = 10; }",
        [if_then_else(num(1), [declassign(x, num(10))], [])]),
    run_test("if-then multiple statements",
        "if(1){ byte x = 10; x = x + 1; }",
        [if_then_else(num(1), [declassign(x, num(10)), assign(x, binop(+, var(x), num(1)))], [])]),
    run_test("if-then-else",
        "if(1){ byte x = 10; } else { byte y = 20; }",
        [if_then_else(num(1), [declassign(x, num(10))], [declassign(y, num(20))])]),
    run_test("if-then syntax error",
        "if(1){ byte x = ; }",
        [if_then_else(num(1), [error('syntax error', "byte x = ;")], [])]).

%!!.	Repeat last query
%!nr.	Repeat query numbered <nr>
%!str.	Repeat last query starting with <str>
%h.	    Show history of commands
%!h.	Show this list

% List = [+, 1, 1],
% Term =.. List,
% R is Term.
