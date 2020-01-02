:- ensure_loaded('puzzleSolver.pl').



test1(Res):-
    solvePuzzle([[a, -3, A], [b, -1, B], [c, 2, [[ca, -2, CA],[cb, -1, CB],[cc, 1, CC]]]], Res).