:-use_module(library(random)).
:-include('gameLogic.pl').

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

findAllEmpty([0|Row], X, [X|XValues]):-
    Xn is X - 1,
    findAllEmpty(Row, Xn, Xvalues).

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

dijkstra([]).

dijkstra([Node|NodeList]):-
    visit([Node], 1),
    dijkstra(NodeList).

% Stops when the node list is empty
visit([], _).

visit([[X,Y] | Nodes], Distance):-
    \+visited(X,Y),

    distance([X,Y], OlderDistance),
    Distance < OlderDistance,

    % From this point on, this predicate does not fail, assuring the retraction after the assertion
    asserta(visited(X,Y)),
    
    findall(Adjacent, isAdjacent([X,Y], Adjacent), Adjacents),
    findall(Diagonal, isDiagonal([X,Y], _, Diagonal), Diagonals),
    append(Adjacents, Diagonals, NewNodes),

    NextDistance is Distance + 1,
    visit(NewNodes, NextDistance),

    retract(visited(X,Y)).

% Visit very Node in the Node list
visit([_|Nodes], Distance):-
    visit(Nodes, Distance).

/*------------------------------------------------------------------------------------------------------*/

% Player piece found
initDistancesRow([Player|Row], X, Y, Player):-
    distance([X,Y], 1),
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

% Empty starting Postion for player 1
initDistancesRow([0|Row], 1, Y, 1):-
    distance([1,Y], 1),
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

% Empty starting Postion for player 2
initDistancesRow([0|Row], X, 1, 2):-
    distance([X,1], 1),
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

% Empty position found
initDistancesRow([0|Row], X, Y, Player):-
    distance([X,Y], 99),
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

% Opposing player piece found
initDistancesRow([_|Row], X, Y, Player):-
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

initDistances([Row|BoardState], Y, Player):-
    initDistancesRow(Row, 1, Y, Player),
    Yn is Y + 1,
    initDistances(BoardState, Yn, Player).

/*------------------------------------------------------------------------------------------------------*/

getClosestNode([Node], Node).

getClosestNode([Node|List], ClosestNode):-
    getClosestNode(List, ClosestNode),
    distance(Node, Distance1),
    distance(ClosestNode, Distance2),
    Distance1 > Distance2.

getClosestNode([Node|List], Node).

/*------------------------------------------------------------------------------------------------------*/

makePath(StartingPoint, []):-
    distance(StartingPoint, 1).

makePath(StartingPoint, [StartingPoint|Path]):-
    findall(Adjacent, isAdjacent(StartingPoint, Adjacent), Adjacents),
    findall(Diagonal, isDiagonal(StartingPoint, _, Diagonal), Diagonals),
    append(Adjacents, Diagonals, NextPoints),
    getClosestNode(NextPoints, NextNode),
    makePath(NextNode, Path).

/*------------------------------------------------------------------------------------------------------*/

getIdealPath(BoardState, Player, Path):-
    initDistances(BoardState, 1, Player),
    findAll(Node, distance(Node, 1), StartingNodes),
    dijkstra(StartingNodes),
    subtractRandomIndex(StartingNodes, StartingPoint),
    makePath(StartingPoint, Path).

/*------------------------------------------------------------------------------------------------------*/

isEmptyList([]).

handlePaths(PlayerPath, OpponentPath, Intersected):-
    intersect(PlayerPath, OpponentPath, Intersected),
    \+isEmptyList(Intersected).

handlePaths(PlayerPath, OpponentPath, PlayerPath):-
    length(PlayerPath, PlayerLength),
    length(OpponentPath, OpponentLength),
    PlayerLength < OpponentLength.

handlePaths(_, OpponentPath, OpponentPath).

/*------------------------------------------------------------------------------------------------------*/

opponent(1,2).
opponent(2,1).

simulatePaths(_, Turns, BoardState, _, []):-
    getCurrentPlayer(Turns, Player),
    checkGameEnd(BoardState, Player).

simulatePaths(Depth, Turns, BoardState, Play, []):-
    getCurrentPlayer(Turns, Player),
    opponent(Player, Opponent),
    getIdealPath(BoardState, Player, PlayerPath),
    getIdealPath(BoardState, Opponent, OpponentPath),
    handlePaths(PlayerPath, OpponentPath, Path),
    subtractRandomIndex(Path, Play, AlternativePlays),
    simulateFollowingPaths(Depth,).

/* These two predicates have the purpose of trying every alternative play*/
simulatePaths(Depth, Turns, BoardState, _, [[Xcoord,Ycoord]|AlternateList]):-
    getCurrentPlayer(Turns, Player),
    executePlay(BoardState, Player, Xcoord, Ycoord, NewBoard, Cut).
    updateTurnState(Turns, Cut, NewTurns),
    DepthN is Depth - 1,
    /+simulatePaths(DepthN, OpponentPath, BoardState, _, []).

simulatePaths(Depth, Turns, BoardState, _, [_|AlternateList]):-
    simulatePaths(Depth, Turns, BoardState, _, AlternateList).

/*------------------------------------------------------------------------------------------------------*/
    
/*Considering two turn plays*/ 
simulateFollowingPaths(Depth, [_,2], BoardState, AlternativePlays):-
    Depth > 0,
    executePlay(Board, [0,2], Xcoord, Ycoord, NewBoard, Cut),
    updateTurnState(Turns, Cut, NewTurns),
    DepthN is Depth - 1,
    /*Simulating the next move as the same player*/
    simulatePaths(DepthN, NewTurns, BoardState, AlternativePlays).

simulateFollowingPaths(Depth, [2,_], BoardState, AlternativePlays):-
    Depth > 0,
    executePlay(Board, [2,0], Xcoord, Ycoord, NewBoard, Cut),
    updateTurnState(Turns, Cut, NewTurns),
    DepthN is Depth - 1,
    simulatePaths(DepthN, NewTurns, BoardState, AlternativePlays).

% Considering one turn plays
simulateFollowingPaths(Depth, Turns, BoardState, AlternativePlays):-
    Depth > 0,
    executePlay(Board, Turns, Xcoord, Ycoord, NewBoard, Cut),
    updateTurnState(Turns, Cut, NewTurns),
    DepthN is Depth - 1,
    /*Simulating the next move as the opposing player*/
    \+simulatePaths(DepthN, NewTurns, BoardState, AlternativePlays).

/*------------------------------------------------------------------------------------------------------*/

getAIinput(Depth, Turns, BoardState, Input):-
    simulatePaths(Depth, Turns, BoardState, Input, []).

getAIinput(_, _, [BoardPieces,_], Input):-
    findAllEmpty(BoardPieces, PossiblePlays),
    subtractRandomIndex(PossiblePlays, Input, _).

/*------------------------------------------------------------------------------------------------------*/

getLevel(1, [Player1, _], Player1).
getLevel(2, [_, Player2], Player2).

getInput(BoardState, Turns, Player1, Player2, CurrPlayer, Xcoord, Ycoord):-
    getLevel(CurrPlayer, [Player1,Player2], Depth),
    getAIinput(Depth, Turns, BoardState, [Xcoord, Ycoord]).
