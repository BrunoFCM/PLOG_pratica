:-ensure_loaded('gameLogicLists.pl').
:-ensure_loaded('gameLogic.pl').
:-ensure_loaded('boardNodes.pl').

:-dynamic(distance/2).

/*------------------------------------------------------------------------------------------------------*/

isValidDiagonalNode([Xa,Ya], [Xb,Yb]):-
    \+distance([Xa,Yb], _),
    \+distance([Xb,Ya], _).

isValidDiagonalNode([Xa,Ya], [Xb,Yb]):-
    (distance([Xa,Yb], 0) ; distance([Xb,Ya], 0)).

isDiagonalNode(Node, Diagonal):-
    isDiagonal(Node, _, Diagonal),
    isValidDiagonalNode(Node, Diagonal),
    distance(Diagonal, _).

isAdjacentNode(Node, Adjacent):-
    isAdjacent(Node, Adjacent),
    distance(Adjacent, _).

/*------------------------------------------------------------------------------------------------------*/

display():-
    findall(Node, distance(Node, _), Nodes),
    display(Nodes).

display([]).

display([[X,Y]|Nodes]):-
    write(X),
    write(','),
    write(Y),
    write('='),
    distance([X,Y], Distance),
    write(Distance),
    nl,
    display(Nodes).

dijkstra([]).

dijkstra(Nodelist):-
    visit(Nodelist, 1).

% Stops when the node list is empty
visit([], _):-
    display,
    fail.

visit([[X,Y] | _], Distance):-
    \+visited(X,Y),
    
    distance([X,Y], 0),

    % From this point on, this predicate does not fail, assuring the retraction after the assertion
    asserta(visited(X,Y)),
    
    findall(Adjacent, isAdjacentNode([X,Y], Adjacent), Adjacents),
    findall(Diagonal, isDiagonalNode([X,Y], Diagonal), Diagonals),
    append(Adjacents, Diagonals, NewNodes),

    visit(NewNodes, Distance),

    retract(visited(X,Y)).

visit([[X,Y] | Nodes], Distance):-
    Distance < 31,

    \+visited(X,Y),
    
    distance([X,Y], OlderDistance),
    (Distance < OlderDistance ; Distance = 1),

    % From this point on, this predicate does not fail, assuring the retraction after the assertion
    asserta(visited(X,Y)),
    
    retractall(distance([X,Y], _)),
    asserta(distance([X,Y], Distance)),
    
    findall(Adjacent, isAdjacentNode([X,Y], Adjacent), Adjacents),
    findall(Diagonal, isDiagonalNode([X,Y], Diagonal), Diagonals),
    append(Adjacents, Diagonals, NewNodes),

    NextDistance is Distance + 1,
    visit(NewNodes, NextDistance),
    visit(Nodes, Distance),

    retract(visited(X,Y)).

% Visit very Node in the Node list
visit([_|Nodes], Distance):-
    visit(Nodes, Distance).

/*------------------------------------------------------------------------------------------------------*/

initDistancesRow([], _, _, _).

% Player piece found
initDistancesRow([Player|Row], X, Y, Player):-
    asserta(distance([X,Y], 0)),
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

% Empty starting Postion for player 1
initDistancesRow([0|Row], 1, Y, 1):-
    asserta(distance([1,Y], 1)),
    initDistancesRow(Row, 2, Y, 1).

% Empty starting Postion for player 2
initDistancesRow([0|Row], X, 1, 2):-
    asserta(distance([X,1], 1)),
    Xn is X + 1,
    initDistancesRow(Row, Xn, 1, 2).

% Empty position found
initDistancesRow([0|Row], X, Y, Player):-
    asserta(distance([X,Y], 99)),
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

% Opposing player piece found
initDistancesRow([_|Row], X, Y, Player):-
    Xn is X + 1,
    initDistancesRow(Row, Xn, Y, Player).

initDistances([], _, _).

initDistances([Row|BoardPieces], Y, Player):-
    initDistancesRow(Row, 1, Y, Player),
    Yn is Y + 1,
    initDistances(BoardPieces, Yn, Player).

/*------------------------------------------------------------------------------------------------------*/

getNodeOfValidDistance([Node|_], _, Node):-
    distance(Node, 0).

getNodeOfValidDistance([Node|_], Distance, Node):-
    distance(Node, Distance).

getNodeOfValidDistance([_|NodeList], Distance, Node):-
    getNodeOfValidDistance(NodeList, Distance, Node).

/*------------------------------------------------------------------------------------------------------*/

makePath([8,Y], [[8,Y]], 1).

makePath([X,8], [[X,8]], 2).

makePath(StartingPoint, [StartingPoint|Path], Player):-
    distance(StartingPoint, Distance),
    findall(Adjacent, isAdjacentNode(StartingPoint, Adjacent), Adjacents),
    findall(Diagonal, isDiagonalNode(StartingPoint, Diagonal), Diagonals),
    append(Adjacents, Diagonals, NextPoints),
    NextDistance is Distance + 1,
    getNodeOfValidDistance(NextPoints, NextDistance, NextNode),
    makePath(NextNode, Path, Player).

/*------------------------------------------------------------------------------------------------------*/

getIdealPath([BoardPieces|_], Player, Path):-
    initDistances(BoardPieces, 1, Player),
    findall(Node, distance(Node, 1), StartingNodes),
    dijkstra(StartingNodes),
    subtractRandomIndex(StartingNodes, StartingPoint, _),
    makePath(StartingPoint, Path, Player),
    retractall(distance(_,_)).
