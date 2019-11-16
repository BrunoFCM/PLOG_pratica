print_upper_frame(N):-
    N > 0,
    N1 is N - 1,
    write('B'),
    print_frame_divisor(N),
    print_upper_frame(N1).

print_upper_frame(0):-
    put_code(187),
    nl(user_output).

print_lower_frame(N):-
    N > 0,
    N1 is N - 1,
    write('B'),
    print_frame_divisor(N),
    print_lower_frame(N1).

print_lower_frame(0):-
    put_code(188),
    nl(user_output).

print_frame_divisor(1).

print_frame_divisor(N):-
    N > 1,
    put_code(205).

print_left_frame(a):-
    write('R').

print_right_frame(a):-
    write('R'),
    nl(user_output).

print_piece(0):-
    write('E').

print_piece(1):-
    write('R').

print_piece(2):-
    write('B').

print_piece_divisor([]).

print_piece_divisor(ROW):-
    \+(ROW == []),
    put_code(186).

print_connect(0):-
    write('e').

print_connect(1):-
    write('r').

print_connect(2):-
    write('b').

print_piece_row([PIECE]):-
    print_piece(PIECE),
    print_piece_row([]).

print_piece_row([PIECE|ROW]):-
    print_piece(PIECE),
    print_piece_divisor(ROW),
    print_piece_row(ROW).

print_piece_row([]):-
    print_right_frame(a).

print_connect_row([CONNECT|ROW]):-
    put_code(205),
    print_connect(CONNECT),
    print_connect_row(ROW).

print_connect_row([]):-
    put_code(205),
    put_code(186),
    nl(user_output).

print_row([PIECE|PROW], [CONNECT|CROW]):-
    print_left_frame(a),
    print_piece_row([PIECE|PROW]),
    put_code(186),
    print_connect_row([CONNECT|CROW]).

print_row([],[]).

print_rows([PROW|PBOARD],[CROW|CBOARD]):-
    print_row(PROW,CROW),
    print_rows(PBOARD,CBOARD).

print_rows([PROW|PBOARD],[]):-
    print_left_frame(a),
    print_piece_row(PROW),
    print_rows(PBOARD,[]).

print_rows([],[]).

print_board([PBOARD,CBOARD]):-
    nl(user_output),
    put_code(201),
    print_upper_frame(8),
    print_rows(PBOARD,CBOARD),
    put_code(200),
    print_lower_frame(8).

print_player(1):-
    write('Blue player turn'),
    nl(user_output).

print_player(2):-
    write('Red player turn'),
    nl(user_output).

display_game(BOARD, PLAYER):-
    print_board(BOARD),
    print_player(PLAYER).