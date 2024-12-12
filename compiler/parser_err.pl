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
    [C], { char_type(C, alpha) },
    variable_chars(Cs),
    { atom_codes(Var, [C|Cs]) }.

variable_chars([C|Cs]) --> [C], { char_type(C, alnum) }, variable_chars(Cs).
variable_chars([]) --> [].

% ----------------------------
% statements
% ----------------------------

statements(Statements) -->
    statements([],Sts), {reverse(Sts,Statements)}.

% Bs represents the semantics defined up to this point -- in reversed order,
% Stms is the actual semantics
statements(Bs,Stms) --> statement(Bs,X), !, ws, statements([X|Bs],Stms).
statements(Bs,Stms) -->
    nonempty(Tokens), !, { string_codes(Ts, Tokens), Stms = [error('syntax error',Ts)|Bs] }.
statements(Bs,Bs) --> ws.


is_declared(Bs,Var) :-
    member(declare(Var), Bs) ; member(declassign(Var, _), Bs).


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
        ;   Expr = error(_,_)
            -> Stms = Expr % pass the error
            ;   Stms = declassign(Var, Expr) % flatten this later
    }.

% ----------------------------
% expression
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
        ; Stm = error('use while-in/before declaration',var(Var))
    }.
expression(Bs,Result) -->
    expression(Bs,Lhs), ws, operator(Op), ws, expression(Bs,Rhs),
    {
        (   Lhs = error(_, _)
        ->  Result = Lhs
        ;   Rhs = error(_, _)
        ->  Result = Rhs
        ;   Result = binop(Op, Lhs, Rhs)
        )
    }.

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".
operator(mod) --> "mod".
operator(<) --> "<".
operator(>) --> ">".
operator(==) --> "==".
operator(invalid) --> { throw(error('invalid_operator','invalid_operator')) }.

parse(S,R) :-
    string_codes(S, SC),
    statements(R,SC,[]).

run_test(Input, Expected) :-
    string_codes(Input, InputCodes),
    ( phrase(statements(Ast), InputCodes),
        Ast = Expected -> 
        format("~w: passed~n", [Input])
    ; 
        format("~w: failed~nexpected: ~w~ngot: ~w~n", [Input, Ast])
    ).

tests :-
    run_test("v = 1;",
        [error('assignment before declaration', [v, =, num(1)])]),
    run_test("byte v;",
        [declare(v)]),
    run_test("byte",
        [error('syntax error', "byte")]),
    run_test("byte v = 1;",
        [declassign(v, num(1))]),
    run_test("byte v = 255;",
        [declassign(v, num(255))]),
    run_test("byte v = 123123;",
        [error('byte representation failed', 123123)]),
    run_test("byte v = 1 + 1 + 1 + 1;",
        [declassign(v, binop(+, num(1), binop(+, num(1), binop(+, num(1), num(1)))))]).

% parse("byte v = 1 + 1 + 1 + v;",T).

% throw and catch things?

%!!.	Repeat last query
%!nr.	Repeat query numbered <nr>
%!str.	Repeat last query starting with <str>
%h.	    Show history of commands
%!h.	Show this list

% parse("byte v = 0x1;",T).