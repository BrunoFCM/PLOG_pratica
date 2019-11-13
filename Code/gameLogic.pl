:- dynamic(visited/2);

/*------------------------------------------------------------------------------------------------------*/

find([Element|_], 1, Element).

find([_|Row], X, Value):-
    X > 1,
    Xn is X - 1,
    find(Row, Xn, Value).

find([Row|_], X, 1, Value):-
    find(Row, X, Value).

find([_|GameState], X, Y, Value):-
    Y > 1,
    Yn is Y - 1,
    find(GameState, X, Yn, Value).

/*------------------------------------------------------------------------------------------------------*/

listPoint(X, Y, [X,Y]).

/*------------------------------------------------------------------------------------------------------*/

makePointList([], _, []).
makePointList(_, [], []).

makePointList([X|XList], Y, List):-
    makePointList(XList, Y, Points),
    append([[X,Y]], Points, List).

makePointList(X, [Y|YList], List):-
    makePointList(X, YList, Points),
    append([[X,Y]], Points, List).

/*------------------------------------------------------------------------------------------------------*/

getStartingHorizontal([],1).

getStartingHorizontal([2|_], 1).

getStartingHorizontal([_|Row], X):-
    getStartingHorizontal(Row, Xn),
    X is Xn + 1.

getStartingVertical([],1).

getStartingVertical([[1|_]|_], 1).

getStartingVertical([_|BoardPieces], Y):-
    getStartingVertical(BoardPieces, Yn),
    Y is Yn + 1.

getStartingPoints([[Row|_], _], Points, 2):-
    findall(X, getStartingHorizontal(Row, X), XValues),
    makePointList(XValues, 1, Points).


getStartingPoints([BoardPieces, _], Points, 1):-
    findall(Y, getStartingVertical(BoardPieces,Y), YValues),
    makePointList(1, YValues, Points).

/*------------------------------------------------------------------------------------------------------*/

isAdjacent([X,Y], [X,Ya]):-
    Y = Ya + 1.

isAdjacent([X,Y], [X,Ya]):-
    Y = Ya - 1.

isAdjacent([X,Y], [Xa,Y]):-
    X = Xa + 1.

isAdjacent([X,Y], [Xa,Y]):-
    X = Xa - 1.

isValidAdjacent(GameState, Player, Point, [Xadjacent,Yadjacent]):-
    isAdjacent(Point, [Xadjacent,Yadjacent]),
    find(GameState, Xadjacent, Yadjacent, Player).

getAdjacents(GameState, Player, Point, Adjacents):-
    findall(Adjacent, isValidAdjacent(GameState, Player, Point, Adjacent), Adjacents).

/*------------------------------------------------------------------------------------------------------*/

isDiagonal([X,Y], [X,Y]).

isDiagonal([X,Y], [X,Yc]):-
    Yc = Y + 1.

isDiagonal([X,Y], [Xc,Yc]):-
    Yc = Y + 1,
    Xc = X + 1.

isDiagonal([X,Y], [Xc,Y]):-
    Xc = X + 1.

/*----------------------------------------------------------------------------*/

checkForEmpty([0|_], 2).

checkForEmpty([0|_], 1).

checkForEmpty([_|Row], X):-
    X > 0,
    Xn is X - 1,
    checkForEmpty(Row, Xn).

checkForEmpty([Row|_], X, 2):-
    checkForEmpty(Row,X).

checkForEmpty([Row|_], X, 1):-
    checkForEmpty(Row,X).

checkForEmpty([_|BoardPieces], X, Y):-
    Y > 0,
    Yn is Y - 1,
    checkForEmpty(BoardPieces, X, Yn).

/*----------------------------------------------------------------------------*/

isValidDiagonal(BoardConnections, Player, Point, [Xdiag, Ydiag]):-
    isDiagonal(Point, [Xdiag, Ydiag]),
    find(BoardConnections, Xdiag, Ydiag, Player).

isUnbreakableDiagonal([BoardPieces, BoardConnections], Player, Point, [Xdiag,Ydiag]):-
    isValidDiagonal(BoardConnections, Player, Point, [Xdiag,Ydiag]),
    \+(checkForEmpty(BoardPieces, Xdiag,Ydiag)).

getDiagonals(GameState, Player, Point, Diagonals):-
    findall(Diagonal, isUnbreakableDiagonal(GameState, Player, Point, Diagonal), Diagonals).

/*------------------------------------------------------------------------------------------------------*/

visitPoint(_, 1, [8,_], _).

visitPoint(_, 2, [_,8], _).

visitPoint(GameState, Player, [Xpoint,Ypoint], ExtraPoints):-
    \+(visited(Xpoint,Ypoint)),
    asserta(visited(Xpoint,Ypoint)),
    getAdjacents(GameState, Player, [Xpoint,Ypoint], Adjacents),
    getDiagonals(GameState, Player, [Xpoint,Ypoint], Diagonals),
    append(Adjacents,Diagonals, ExtraPoints).

/*------------------------------------------------------------------------------------------------------*/

visitPoints(GameState, Player, [Point|Points]):-
    visitPoint(GameState, Player, Point, ExtraPoints),
    append(Points, ExtraPoints, NewPoints),
    visitPoints(GameState, Player, NewPoints).

/*------------------------------------------------------------------------------------------------------*/

checkGameEnd(GameState, Player):-
    getStartingPoints(GameState, Points, Player),
    !,    
    retractall(visited/2),
    visitPoints(GameState, Player, Points),
    retractall(visited/2).