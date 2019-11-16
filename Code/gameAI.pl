:-use_module(library(random)).
:-dynamic(distance/2).

/*------------------------------------------------------------------------------------------------------*/

findAllEmpty(BoardPieces, Points):-
    findAllEmpty(BoardPieces, 8, 8, Points).

findAllEmpty(_, _, 0, []).

findAllEmpty([Row|BoardPieces], X, Y, Points):-
    findAllEmpty(Row, X, XValues),
    makePointList(XValues, Y, RowPoints),
    Yn is Y - 1,
    findAllEmpty(BoardPieces, X, Yn, ExtraPoints),
    append(RowPoints, ExtraPoints, Points).

findAllEmpty(_, 0, []).

findAllEmpty([0|Row], X, XValues):-

/*------------------------------------------------------------------------------------------------------*/

subtractRandomIndex(List, Value, NewList):-
    length(List, Length),
    random_between(0, Length, Index),
    subtractIndex(List, Index, Value, NewList).

subtractIndex([Value|List], 1, Value, List).

subtractIndex([Element|List], Index, Value, [Element|NewList]):-
    IndexN is Index - 1,
    subtractIndex(List, IndexN, Value, NewList).

/*------------------------------------------------------------------------------------------------------*/

getIdealPath(BoardState, Player, Path)

/*------------------------------------------------------------------------------------------------------*/

getStartingTurn(1, [1,0]).
getStartingTurn(2, [0,1]).

opponent(1,2).opponent(2,1).
    
simulatePaths(_, Turns, BoardState, _, []):-
    getCurrentPlayer(_, Player),
    checkGameEnd(BoardState, Player).

simulatePaths(Depth, Turns, BoardState, Play, []):-
    opponent(Player, Opponent),
/*TODO*/
    getIdealPath(BoardState, Player, PlayerPath),
    getIdealPath(BoardState, Opponent, OpponentPath),

    handlePaths(PlayerPath, OpponentPath, Path),

    subtractRandomIndex(Path, Play, AlternativePlays),

    doTurn(Board, Player1, Player2, CurrPlayer, NewBoard, Cut),

    updateTurnState(Turns, Cut, NewTurns),

    DepthN is Depth - 1,
    /+simulatePaths(DepthN, OpponentPath, BoardState, AlternativePlays).

simulatePaths(Depth, Turns, BoardState, Play, [Play|AlternateList]):-
    doTurn(Board, Player1, Player2, CurrPlayer, NewBoard, Cut),

    updateTurnState(Turns, Cut, NewTurns),

    DepthN is Depth - 1,
    /+simulatePaths(DepthN, OpponentPath, BoardState, AlternativePlays).

simulatePaths(Depth, Turns, BoardState, Play, [_|AlternateList]):-
    simulatePaths(Depth, Turns, BoardState, Play, AlternateList).

/*------------------------------------------------------------------------------------------------------*/

getAIinput(Depth, Player, BoardState, Input):-
    simulatePaths(Depth, Player, BoardState, Input, []).

getAIinput(_, Player, [BoardPieces,_], Input):-
    findAllEmpty(BoardPieces, PossiblePlays),
    subtractRandomIndex(PossiblePlays, Input, _).

