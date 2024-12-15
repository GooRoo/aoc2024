:- use_module(library(clpfd)).

read_input(Filename, Machines) :-
    open(Filename, read, Stream),
    read_machines(Stream, [], Machines),
    close(Stream).

read_machines(Stream, Acc, Machines) :-
    (at_end_of_stream(Stream) ->
        Machines = Acc
    ;
        read_machine(Stream, Machine),
        append(Acc, [Machine], NewAcc),
        read_machines(Stream, NewAcc, Machines)
    ).

read_machine(Stream, machine(Ax, Ay, Bx, By, Px, Py)) :-
    read_line_to_string(Stream, ButtonA),
    read_line_to_string(Stream, ButtonB),
    read_line_to_string(Stream, Prize),

    % skip empty line
    (at_end_of_stream(Stream) -> true
    ; read_line_to_string(Stream, _)),

    parse_button_a(ButtonA, Ax, Ay),
    parse_button_b(ButtonB, Bx, By),
    parse_prize(Prize, Px, Py).

parse_num(Var, String, Number) :-
    atom_chars(String, [Var, Sign|Rest]),
    atom_chars(NumStr, [Sign|Rest]),
    atom_number(NumStr, Number).

parse_button_a(Line, X, Y) :-
    split_string(Line, ",:", " ", ["Button A", Xs, Ys]),
	parse_num('X', Xs, X),
	parse_num('Y', Ys, Y).

parse_button_b(Line, X, Y) :-
    split_string(Line, ",:", " ", ["Button B", Xs, Ys]),
	parse_num('X', Xs, X),
	parse_num('Y', Ys, Y).

parse_prize(Line, X, Y) :-
    split_string(Line, ":=,", " ", ["Prize", "X", Xs, "Y", Ys]),
    number_string(X, Xs),
    number_string(Y, Ys).

check_possible(Solver, Ax, Ay, Bx, By, Px, Py) :-
    catch(
        (call(Solver, Ax, Ay, Bx, By, Px, Py, _, _) -> true),
        _,
        false
    ).

solve_machine(Ax, Ay, Bx, By, Px, Py, A, B) :-
    A in 0..100,
    B in 0..100,

    A * Ax + B * Bx #= Px,
    A * Ay + B * By #= Py,

    Tokens #= A * 3 + B,

    labeling([min(Tokens)], [A,B]).

solve_machine2(Ax, Ay, Bx, By, Px, Py, A, B) :-
    A in 0..10000000000000,
    B in 0..10000000000000,

    A * Ax + B * Bx #= (Px + 10000000000000),
    A * Ay + B * By #= (Py + 10000000000000),

    Tokens #= A * 3 + B,

    labeling([min(Tokens)], [A,B]).

solve_machines_generic([], _, Total, Total).
solve_machines_generic([Machine|Rest], SolveMachine, Acc, Total) :-
    machine(Ax, Ay, Bx, By, Px, Py) = Machine,
    (check_possible(SolveMachine, Ax, Ay, Bx, By, Px, Py) ->
        call(SolveMachine, Ax, Ay, Bx, By, Px, Py, A, B),
        Tokens is A * 3 + B,
        % format('Machine ~w solved: A = ~w, B = ~w, Tokens = ~w~n', [Machine, A, B, Tokens]),
        NewAcc is Acc + Tokens
    ;
        % format('No solution for ~w~n', [Machine]),
        NewAcc is Acc
    ),
    solve_machines_generic(Rest, SolveMachine, NewAcc, Total).

solve_machines(Machines, Acc, Total) :-
	solve_machines_generic(Machines, solve_machine, Acc, Total).

solve_machines2(Machines, Acc, Total) :-
	solve_machines_generic(Machines, solve_machine2, Acc, Total).

solve_first(Filename, Tokens) :-
	read_input(Filename, Machines),
	solve_machines(Machines, 0, Tokens).

solve_second(Filename, Tokens) :-
	read_input(Filename, Machines),
	solve_machines2(Machines, 0, Tokens).

main :-
	solve_first("data/task.data", Tokens1),
	solve_second("data/task.data", Tokens2),
	format('Results:~n    first: ~w~n    second: ~w~n', [Tokens1, Tokens2]).

% :- initialization(main).
