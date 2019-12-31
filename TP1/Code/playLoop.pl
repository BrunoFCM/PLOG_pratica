:-ensure_loaded('getPlayerInput.pl').
:-ensure_loaded('gameLogic.pl').
:-ensure_loaded('executePlay.pl').
:-ensure_loaded('gameAI.pl').

/*------------------------------------------------------------------------------------------------------*/
/* getCurrentPlayer(+Turns, -CurrentPlayer) */
/* Reads the turn list and determines who plays next. */
getCurrentPlayer([ _, 0], 1).
getCurrentPlayer([ 0, _], 2).

/*------------------------------------------------------------------------------------------------------*/
/* opponent(+Player, -Opponent) */
/* Determines the identifier for a given Player's Opponent */
opponent(1,2).
opponent(2,1).

/*------------------------------------------------------------------------------------------------------*/
/* updateTurnState(+Turns, +Cut, -NewTurns) */
/* Determines the amount of remaining moves each player has, depending on whether there was a cut. */
updateTurnState([ _, 0], 1, [ 0, 2]).
updateTurnState([ 0, _], 1, [ 2, 0]).
updateTurnState([ 2, 0], 0, [ 1, 0]).
updateTurnState([ 0, 2], 0, [ 0, 1]).
updateTurnState([ 1, 0], 0, [ 0, 1]).
updateTurnState([ 0, 1], 0, [ 1, 0]).

/* getLevel(+Player, +Levels, -PlayerLevel) */
/* Determines the AI level for a given Player (a human player has a level of 0) */
getLevel(1, [Player1, _], Player1).
getLevel(2, [_, Player2], Player2).

/*------------------------------------------------------------------------------------------------------*/
/* getInput(+Board, +P1, +P2, +CurrPlayer, +Turns, -Xcoord, -Ycoord) */
/* Gets the coordinates for the next move from the current player. */
getInput( _, 0, _, 1, _, Xcoord, Ycoord):-
    getPlayerInput(1, Xcoord, Ycoord).

getInput( _, _, 0, 2, _, Xcoord, Ycoord):-
    getPlayerInput(2, Xcoord, Ycoord).

getInput(BoardState, Player1, Player2, CurrPlayer, Turns, Xcoord, Ycoord):-
    getLevel(CurrPlayer, [Player1,Player2], Depth),
    getAIinput(Depth, Turns, BoardState, [Xcoord, Ycoord]).

/*------------------------------------------------------------------------------------------------------*/
/* doTurn(+Board, +P1, +P2, +CurrPlayer, Turns, -NewBoard, -Cut) */
/* Prompts the current player to make his/her move and modifies the board accordingly. */
doTurn(Board, Player1, Player2, CurrPlayer, Turns, NewBoard, Cut):-
    repeat,
    getInput(Board, Player1, Player2, CurrPlayer, Turns, Xcoord, Ycoord),
    executePlay(Board, CurrPlayer, Xcoord, Ycoord, NewBoard, Cut).

/*------------------------------------------------------------------------------------------------------*/
/* playLoop(+Board, +P1, +P2, +Turns) */
/* Main recursive loop of the game. */

/* game end */
playLoop(Board, _, _, Turns):-
    getCurrentPlayer(Turns, CurrPlayer),
    opponent(CurrPlayer,Opponent),
    checkGameEnd(Board, Opponent),
    displayResult(Opponent).

/* Main predicate */
playLoop(Board, Player1, Player2, Turns):-
    getCurrentPlayer(Turns, CurrPlayer),
    doTurn(Board, Player1, Player2, CurrPlayer, Turns, NewBoard, Cut),
    display_game(NewBoard, CurrPlayer),
    updateTurnState(Turns, Cut, NewTurns),
    playLoop(NewBoard, Player1, Player2, NewTurns).
