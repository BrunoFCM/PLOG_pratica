getNumericalCoord('1', 1).
getNumericalCoord('2', 2).
getNumericalCoord('3', 3).
getNumericalCoord('4', 4).
getNumericalCoord('5', 5).
getNumericalCoord('6', 6).
getNumericalCoord('7', 7).
getNumericalCoord('8', 8).

/* Used to place a tile in an empty space */
placePieceY([0 | RemColumn], Player, 1, 0, [Player | RemColumn]).

placePieceY([ _ | RemColumn], Player, 1, 1, [Player | RemColumn]).

placePieceY(_, _, 1, _, _):-
    !, fail.

placePieceY([CurrentSpace | RemColumn], Player, Ynum, IsDiag, [CurrentSpace | RetColumn]):-
    Ynum2 is Ynum - 1,
    placePieceY(RemColumn, Player, Ynum2, IsDiag, RetColumn).


placePieceX([CurrentColumn | RemBoard], Player, 1, Ynum, IsDiag, [RetColumn | RemBoard]):-
    !,
    placePieceY(CurrentColumn, Player, Ynum, IsDiag, RetColumn).

placePieceX([CurrentColumn | RemBoard], Player, Xnum, Ynum, IsDiag, [CurrentColumn | RetBoard]):-
    Xnum2 is Xnum - 1,
    placePieceX(RemBoard, Player, Xnum2, Ynum, IsDiag, RetBoard).

/* Used to check for matching tiles in diagonal spaces*/
checkPieceY([Player | _ ], Player, 1).
checkPieceY(_, _, 1):-
    !, fail.
checkPieceY([ _ | RemColumn], Player, Ynum):-
    Ynum2 is Ynum - 1,
    checkPieceY(RemColumn, Player, Ynum2).

checkPieceX([Column | _ ], Player, 1, Ynum):-
    !, checkPieceY(Column, Player, Ynum).
checkPieceX([ _ | RemBoard], Player, Xnum, Ynum):-
    Xnum2 is Xnum - 1,
    checkPieceX(RemBoard, Player, Xnum2, Ynum).


connectDiag(Board, Diags, Player, Xtile, Ytile, Xbridge, Ybridge, NewDiags):-
    checkPieceX(Board, Player, Xtile, Ytile),
    !, placePieceX(Diags, Player, Xbridge, Ybridge, 1, NewDiags).

connectDiag(_, Diags, _, _, _, _, _, Diags).




connectDiags(Board, Diags, Player, 1, 1, NewDiags):-
    connectDiag(Board, Diags, Player, 2, 2, 1, 1, NewDiags).
connectDiags(Board, Diags, Player, 1, 8, NewDiags):-
    connectDiag(Board, Diags, Player, 2, 7, 1, 7, NewDiags).
connectDiags(Board, Diags, Player, 8, 1, NewDiags):-
    connectDiag(Board, Diags, Player, 7, 2, 7, 1, NewDiags).
connectDiags(Board, Diags, Player, 8, 8, NewDiags):-
    connectDiag(Board, Diags, Player, 7, 7, 7, 7, NewDiags).

connectDiags(Board, Diags, Player, Xnum, 1, NewDiags):-
    XnumDec is Xnum - 1,
    XnumInc is Xnum + 1,
    connectDiag(Board, Diags, Player, XnumDec, 2, XnumDec, 1, NewD1),
    connectDiag(Board, NewD1, Player, XnumInc, 2, Xnum, 1, NewDiags).
connectDiags(Board, Diags, Player, Xnum, 8, NewDiags):-
    XnumDec is Xnum - 1,
    XnumInc is Xnum + 1,
    connectDiag(Board, Diags, Player, XnumDec, 7, XnumDec, 7, NewD1),
    connectDiag(Board, NewD1, Player, XnumInc, 7, Xnum, 7, NewDiags).
connectDiags(Board, Diags, Player, 1, Ynum, NewDiags):-
    YnumDec is Ynum - 1,
    YnumInc is Ynum + 1,
    connectDiag(Board, Diags, Player, 2, YnumDec, 1, YnumDec, NewD1),
    connectDiag(Board, NewD1, Player, 2, YnumInc, 1, Ynum, NewDiags).
connectDiags(Board, Diags, Player, 8, Ynum, NewDiags):-
    YnumDec is Ynum - 1,
    YnumInc is Ynum + 1,
    connectDiag(Board, Diags, Player, 7, YnumDec, 7, YnumDec, NewD1),
    connectDiag(Board, NewD1, Player, 7, YnumInc, 7, Ynum, NewDiags).
    
    
connectDiags(Board, Diags, Player, Xnum, Ynum, NewDiags):-
    XnumDec is Xnum - 1,
    XnumInc is Xnum + 1,
    YnumDec is Ynum - 1,
    YnumInc is Ynum + 1,
    connectDiag(Board, Diags, Player, XnumDec, YnumDec, XnumDec, YnumDec, NewD1),
    connectDiag(Board, NewD1, Player, XnumDec, YnumInc, XnumDec, Ynum, NewD2),
    connectDiag(Board, NewD2, Player, XnumInc, YnumDec, Xnum, YnumDec, NewD3),
    connectDiag(Board, NewD3, Player, XnumInc, YnumInc, Xnum, Ynum, NewDiags).

checkCutsY([Gottem | _], [Gottem | _], _, _, 1).
checkCutsY(_, _, 1, 1, 0).
checkCutsY( _, _, _, 1, _):-
    !, fail.
checkCutsY([ _ | OldCol], [ _ | NewCol], Xnum, Ynum, Cut):-
    Ynum2 is Ynum - 1,
    checkCutsY(OldCol, NewCol, Xnum, Ynum2, Cut).

checkCutsX([OldCol | _], [NewCol | _], Xnum, Ynum, Cut):-
    checkCutsY(OldCol, NewCol, Xnum, Ynum, Cut).
checkCutsX([_ | OldDiags], [_ | NewDiags], Xnum, Ynum, Cut):-
    Xnum2 is Xnum - 1,
    checkCutsX(OldDiags, NewDiags, Xnum2, Ynum, Cut).



/* A small issue of X and Y being the wrong way around arised, and due to a lack of time, all Xs and Ys above this line are swapped */

executePlay([Board, Diags], Player, Xcoord, Ycoord, [RetBoard, RetDiags], Cut):-
    getNumericalCoord(Xcoord, Xnum), getNumericalCoord(Ycoord, Ynum),
    placePieceX(Board, Player, Ynum, Xnum, 0, RetBoard),
    connectDiags(Board, Diags, Player, Ynum, Xnum, RetDiags),
    checkCutsX(Diags, RetDiags, 7, 7, Cut).

