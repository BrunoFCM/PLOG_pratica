
getCurrentPlayer([ _, 0], 1).
getCurrentPlayer([ 0, _], 2).

updateTurnState([ _, 0], 1, [ 0, 2]).
updateTurnState([ 0, _], 1, [ 2, 0]).
updateTurnState([ 2, 0], 0, [ 1, 0]).
updateTurnState([ 0, 2], 0, [ 0, 1]).
updateTurnState([ 1, 0], 0, [ 0, 1]).
updateTurnState([ 0, 1], 0, [ 1, 0]).

getInput(0, _, 1, Xcoord, Ycoord):-
    getPlayerInput(1, Xcoord, Ycoord).

getInput(_, 0, 2, Xcoord, Ycoord):-
    getPlayerInput(2, Xcoord, Ycoord).

/*TODO: make getInput for bots*/

doTurn(Board, Player1, Player2, CurrPlayer, NewBoard, Cut):-
    repeat.
    getInput(Player1, Player2, CurrPlayer, Xcoord, Ycoord),
    executePlay(Board, CurrPlayer, Xcoord, Ycoord, NewBoard, Cut).

playLoop(Board, Player1, Player2, Turns):-
    getCurrentPlayer(Turns, CurrPlayer),
    doTurn(Board, Player1, Player2, CurrPlayer, NewBoard, Cut),
    /*TODO: display */
    updateTurnState(Turns, Cut, NewTurns),
    \+ checkGameEnd(NewBoard, CurrPlayer),
    !, playLoop(NewBoard, Player1, Player2, NewTurns).

/* game end */
playLoop(_,_,_,_).
