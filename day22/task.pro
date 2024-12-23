:- use_module(library(prolog_profile)).

read_numbers(FileName, Numbers) :-
    setup_call_cleanup(
        open(FileName, read, Stream),
        read_lines(Stream, Numbers),
        close(Stream)).

read_lines(Stream, Numbers) :-
    read_line_to_string(Stream, Line),
    (   Line \= end_of_file
    ->  number_string(Num, Line),
        Numbers = [Num|Rest],
        read_lines(Stream, Rest)
    ;   Numbers = []
    ).

step(Secret, Num, NewSecret) :-
    NewSecret is (Secret << Num) xor Secret /\ 16'ffffff.

new_secret(Secret, NewSecret) :-
	step(Secret, 6, A),
	step(A, -5, B),
	step(B, 11, NewSecret).

evolve_secret(Secret, 0, Secret).
evolve_secret(Secret, Times, NextSecret) :-
	NewTimes is Times - 1,
	new_secret(Secret, NewSecret),
	evolve_secret(NewSecret, NewTimes, NextSecret).

evolve2000(Secret, Result) :-
	evolve_secret(Secret, 2000, Result).

solve_first(Secrets, Result) :-
	concurrent_maplist(evolve2000, Secrets, Results),
	foldl(plus, Results, 0, Result).

solve_first_from_file(FileName, Result) :-
	read_numbers(FileName, Secrets),
	solve_first(Secrets, Result).

solve() :-
	solve_first_from_file('data/task.data', Result),
	format('Result: ~w~n', [Result]).
