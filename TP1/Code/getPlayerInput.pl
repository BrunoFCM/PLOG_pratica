/* getNumericalCoord(+CharCoordinate, -NumericalCoordinate) */
/* Simultaneously checks if the input coordinate is valid (whithin the board range) and converts it to a numerical value. */
getNumericalCoord('1', 1).
getNumericalCoord('2', 2).
getNumericalCoord('3', 3).
getNumericalCoord('4', 4).
getNumericalCoord('5', 5).
getNumericalCoord('6', 6).
getNumericalCoord('7', 7).
getNumericalCoord('8', 8).

/* getPlayerInput(+Player, -Xcoord, -Ycoord) */
/* as get_char leaves the end of line character in the buffer, skip_line is used */
getPlayerInput(1, Xcoord, Ycoord):-
    write('Red Player, please insert valid coordinates for your next piece (X,Y)'),
    get_char(Xchar), get_char(','), get_char(Ychar), skip_line,
    getNumericalCoord(Xchar,Xcoord),
    getNumericalCoord(Ychar, Ycoord).


getPlayerInput(2, Xcoord, Ycoord):-
    write('Blue Player, please insert valid coordinates for your next piece (X,Y)'),
    get_char(Xchar), get_char(','), get_char(Ychar), skip_line,
    getNumericalCoord(Xchar,Xcoord),
    getNumericalCoord(Ychar, Ycoord).

/* cleans buffer in case of bad inputs not passing the ',' */
getPlayerInput(_, 0, 0):-
    skip_line.
    
    
    