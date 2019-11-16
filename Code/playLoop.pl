
/* getCurrentPlayer(+Turns, -CurrentPlayer) */
/* Reads the turn list and determines who plays next. */
getCurrentPlayer([ _, 0], 1).
getCurrentPlayer([ 0, _], 2).

/* updateTurnState(+Turns, +Cut, -NewTurns) */
/* Determines the amount of remaining moves each player has, depending on whether there was a cut. */
updateTurnState([ _, 0], 1, [ 0, 2]).
updateTurnState([ 0, _], 1, [ 2, 0]).
updateTurnState([ 2, 0], 0, [ 1, 0]).
updateTurnState([ 0, 2], 0, [ 0, 1]).
updateTurnState([ 1, 0], 0, [ 0, 1]).
updateTurnState([ 0, 1], 0, [ 1, 0]).

/* getInput(+Board, +P1, +P2, +CurrPlayer, +Turns, -Xcoord, -Ycoord) */
/* Gets the coordinates for the next move from the current player. */
getInput( _, 0, _, 1, _, Xcoord, Ycoord):-
    getPlayerInput(1, Xcoord, Ycoord).

getInput( _, _, 0, 2, _, Xcoord, Ycoord):-
    getPlayerInput(2, Xcoord, Ycoord).

/*TODO: make getInput for bots*/


/* doTurn(+Board, +P1, +P2, +CurrPlayer, Turns, -NewBoard, -Cut) */
/* Prompts the current player to make his/her move and modifies the board accordingly. */
doTurn(Board, Player1, Player2, CurrPlayer, Turns, NewBoard, Cut):-
    repeat,
    getInput(Board, Player1, Player2, CurrPlayer, Turns, Xcoord, Ycoord),
    executePlay(Board, CurrPlayer, Xcoord, Ycoord, NewBoard, Cut).

/* playLoop(+Board, +P1, +P2, +Turns) */
/* Main recursive loop of the game. */
playLoop(Board, Player1, Player2, Turns):-
    getCurrentPlayer(Turns, CurrPlayer),
    doTurn(Board, Player1, Player2, CurrPlayer, Turns, NewBoard, Cut),
    /*TODO: display */
    display_game(NewBoard, CurrPlayer),
    updateTurnState(Turns, Cut, NewTurns),
    \+ checkGameEnd(NewBoard, CurrPlayer), !,
    playLoop(NewBoard, Player1, Player2, NewTurns).

/* game end */
playLoop(_,_,_,_).
