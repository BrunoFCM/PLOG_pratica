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
    pointListLength(Nodelist, Size),
    listOfSize(1, Size, DistanceList),
    visit(Nodelist, DistanceList),
    display.

% Stops when the node list is empty
visit([], []).

visit([[X,Y] | _], [Distance|DistanceList]):-    
    distance([X,Y], 0),

    % From this point on, this predicate does not fail, assuring the retraction after the assertion
    
    findall(Adjacent, isAdjacentNode([X,Y], Adjacent), Adjacents),
    findall(Diagonal, isDiagonalNode([X,Y], Diagonal), Diagonals),
    append(Adjacents, Diagonals, ExtraNodes),
    append(Nodes, ExtraNodes, NewNodesList),

    pointListLength(ExtraNodes, Size),
    listOfSize(Distance, Size, ExtraDistanceList),
    append(DistanceList, ExtraDistanceList, NewDistanceList),

    visit(NewNodesList, NewDistanceList).

visit([[X,Y] | Nodes], [Distance|DistanceList]):-
    Distance < 31,
    
    distance([X,Y], OlderDistance),
    (Distance < OlderDistance ; Distance = 1),

    % From this point on, this predicate does not fail, assuring the retraction after the assertion
    
    retractall(distance([X,Y], _)),
    asserta(distance([X,Y], Distance)),
    
    findall(Adjacent, isAdjacentNode([X,Y], Adjacent), Adjacents),
    findall(Diagonal, isDiagonalNode([X,Y], Diagonal), Diagonals),
    append(Adjacents, Diagonals, ExtraNodes),
    append(Nodes, ExtraNodes, NewNodesList),

    NextDistance is Distance + 1,
    pointListLength(ExtraNodes, Size),
    listOfSize(NextDistance, Size, ExtraDistanceList),
    append(DistanceList, ExtraDistanceList, NewDistanceList),

    visit(NewNodesList, NewDistanceList).

% Visit very Node in the Node list
visit([_|Nodes], [_|DistanceList]):-
    visit(Nodes, DistanceList).

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

pivot([],Pivot,[],[Pivot],[]).

pivot([Element|List], Pivot, [Element|Left], Equals, Right):-
    distance(Element, Distance1),
    distance(Pivot, Distance2),
    Distance1 < Distance2,
    pivot(List,Pivot,Left,Equals,Right).

pivot([Element|List], Pivot, Left, [Element|Equals], Right):-
    distance(Element, Distance),
    distance(Pivot, Distance),
    pivot(List,Pivot,Left,Equals,Right).

pivot([Element|List], Pivot, Left, Equals, [Element|Right]):-
    pivot(List,Pivot,Left,Equals,Right).

quickSort([],[]).

quickSort([Element],[Element]).

quickSort(List, Sorted):-
    length(List, Length),
    Index is div(Length,2) + 1,
    subtractIndex(List, Index, Pivot, NewList),
    pivot(NewList, Pivot, Left, Equals, Right),
    quickSort(Left, SortedLeft),
    quickSort(Right, SortedRight),
    append(SortedLeft, Equals, SortedHalf),
    append(SortedHalf, Right, Sorted).

/*------------------------------------------------------------------------------------------------------*/

makePath([1,Y], [[1,Y]], 1).

makePath([X,1], [[X,1]], 2).

makePath(StartingPoint, [StartingPoint|Path], Player):-
    distance(StartingPoint, Distance),
    findall(Adjacent, isAdjacentNode(StartingPoint, Adjacent), Adjacents),
    findall(Diagonal, isDiagonalNode(StartingPoint, Diagonal), Diagonals),
    append(Adjacents, Diagonals, NextPoints),
    quickSort(NextPoints, [NextNode|SortedPoints]),

    makePath(NextNode, Path, Player).

/*------------------------------------------------------------------------------------------------------*/

getEndpoints(1, Endpoints):-
    findall(Y, distance([8,Y], _), YList),
    makePointList(8,YList, Endpoints).

getEndpoints(2, Endpoints):-
    findall(X, distance([X,8], _), XList),
    makePointList(XList,8, Endpoints).

/*------------------------------------------------------------------------------------------------------*/

getIdealPath([BoardPieces|_], Player, Path):-
    retractall(distance(_,_)),
    initDistances(BoardPieces, 1, Player),
    findall(Node, distance(Node, 1), StartingNodes),
    dijkstra(StartingNodes),
    getEndpoints(Player, Endpoints),
    quickSort(Endpoints, ClosestEndpoints),
    subtractRandomIndex(ClosestEndpoints, EndPoint, _),
    !,
    makePath(EndPoint,Path, Player),
    retractall(distance(_,_)).
