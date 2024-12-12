:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

ws --> whitespace, !, ws.
ws --> comment, !, ws.
ws --> [].

whitespace --> [W], { char_type(W, white) ; W = 10}.

comment --> "//", rest_of_line.

rest_of_line --> [C], { C \= 10 }, !, rest_of_line.
rest_of_line --> [10] | [].

nonempty([T|Ts]) --> [T], anything(Ts).
anything([T|Ts]) --> [T], !, anything(Ts).
anything([]) --> [].

% ----------------------------
% bleh
% ----------------------------

variable(_,Var) -->
    [C], { char_type(C, alpha) },
    variable_chars(Cs),
    { atom_codes(Var, [C|Cs]) }.

variable_chars([C|Cs]) --> [C], { char_type(C, alnum) }, variable_chars(Cs).
variable_chars([]) --> [].

hex_number(Bs,N) -->
    "0x", !, hex_digits(Bs,Digits),
    { atom_codes(Atom, [0'0, 0'x | Digits]),
      atom_number(Atom, N) }.

hex_digits(Bs,[D|Ds]) --> hex_digit(Bs,D), !, hex_digits(Bs,Ds).
hex_digits(Bs,[D]) --> hex_digit(Bs,D).

hex_digit(_,Dig) --> [D], { 
    (char_type(D, digit) ; member(D, `abcdefABCDEF`)
        ->  Dig = D
        ;   Dig = error('not a hex-digit',D))
}.

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

statement(Bs,Stms) -->
    % simple assignment
    variable(Bs,Var), ws, "=", ws, expression(Bs,Expr), ws, ";", ws, !,
    {
        member(declare(Var), Bs)
            ->  Stms = assign(Var, Expr)
            ;   Stms = error('assignment before declaration',[Var,=,Expr])
    }.

statement(Bs,Stms) -->
    % simple declaration
    "byte", ws, variable(Bs,Var), ws, ";", !,
    {
        member(declare(Var), Bs)
        ->  Stms = error('redeclaration of variable',Var)
        ;   Stms = declare(Var)
    }.

statement(Bs,Stms) -->
    % declaration with value
    "byte", ws, variable(Bs,Var), ws, "=", ws, expression(Bs,Expr), ws, ";", ws, !,
    {
        member(declare(Var), Bs)
        ->  Stms = error('redeclaration of variable',[Var,=,Expr])
        ;   Expr = error(_,_)
            -> Stms = Expr % pass the error
            ;   Stms = [declare(Var),assign(Var, Expr)] % flatten this later
    }.

% ----------------------------
% expression
% ----------------------------

expression(_,Num) --> integer(N), !,
    {
        between(0, 255, N)
        ->  Num = num(N)
        ;   Num = error('byte representation failed',N)
    }.
expression(Bs,num(N)) --> hex_number(Bs,N).

parse(S,R) :-
    string_codes(S, SC),
    statements(R,SC,[]).

tests :-
    parse("v = 1;",
        [error('assignment before declaration', [v, =, num(1)])]),
    parse("byte v;",[declare(v)]),
    parse("byte",[error('syntax error', "byte")]),
    parse("byte v = 1;",[[declare(v), assign(v, num(1))]]),
    parse("byte v = 255;",[[declare(v), assign(v, num(255))]]),
    parse("byte v = 123123;",[error('byte representation failed', 123123)]).

%!!.	Repeat last query
%!nr.	Repeat query numbered <nr>
%!str.	Repeat last query starting with <str>
%h.	    Show history of commands
%!h.	Show this list
