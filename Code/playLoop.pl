:-ensure_loaded('getPlayerInput.pl').
:-ensure_loaded('gameLogic.pl').
:-ensure_loaded('executePlay.pl').
:-ensure_loaded('gameAI.pl').

getCurrentPlayer([ _, 0], 1).
getCurrentPlayer([ 0, _], 2).

opponent(1,2).
opponent(2,1).

updateTurnState([ _, 0], 1, [ 0, 2]).
updateTurnState([ 0, _], 1, [ 2, 0]).
updateTurnState([ 2, 0], 0, [ 1, 0]).
updateTurnState([ 0, 2], 0, [ 0, 1]).
updateTurnState([ 1, 0], 0, [ 0, 1]).
updateTurnState([ 0, 1], 0, [ 1, 0]).

getLevel(1, [Player1, _], Player1).
getLevel(2, [_, Player2], Player2).

/* getInput(+Board, +P1, +P2, +CurrPlayer, +Turns, -Xcoord, -Ycoord) */
getInput( _, 0, _, 1, _, Xcoord, Ycoord):-
    getPlayerInput(1, Xcoord, Ycoord).

getInput( _, _, 0, 2, _, Xcoord, Ycoord):-
    getPlayerInput(2, Xcoord, Ycoord).

getInput(BoardState, Player1, Player2, CurrPlayer, Turns, Xcoord, Ycoord):-
    getLevel(CurrPlayer, [Player1,Player2], Depth),
    getAIinput(Depth, Turns, BoardState, [Xcoord, Ycoord]).

doTurn(Board, Player1, Player2, CurrPlayer, Turns, NewBoard, Cut):-
    repeat,
    getInput(Board, Player1, Player2, CurrPlayer, Turns, Xcoord, Ycoord),
    executePlay(Board, CurrPlayer, Xcoord, Ycoord, NewBoard, Cut).

/* game end */
playLoop(Board, _, _, Turns):-
    getCurrentPlayer(Turns, CurrPlayer),
    opponent(CurrPlayer,Opponent),
    checkGameEnd(Board, Opponent),
    displayResult(Opponent).

playLoop(Board, Player1, Player2, Turns):-
    getCurrentPlayer(Turns, CurrPlayer),
    doTurn(Board, Player1, Player2, CurrPlayer, Turns, NewBoard, Cut),
    display_game(NewBoard, CurrPlayer),
    updateTurnState(Turns, Cut, NewTurns),
    playLoop(NewBoard, Player1, Player2, NewTurns).
