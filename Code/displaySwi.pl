:- encoding(utf8).

print_upper_frame(N):-
    N > 0,
    N1 is N - 1,
    ansi_format([fg(blue)], '╦', []),
    print_frame_divisor(N),
    print_upper_frame(N1).

print_upper_frame(0):-
    ansi_format([], '╗\n', []).

print_lower_frame(N):-
    N > 0,
    N1 is N - 1,
    ansi_format([fg(blue)], '╩', []),
    print_frame_divisor(N),
    print_lower_frame(N1).

print_lower_frame(0):-
    ansi_format([], '╝\n', []).

print_frame_divisor(1).

print_frame_divisor(N):-
    N > 1,
    ansi_format([fg(black)], '═', []).

print_left_frame(a):-
    ansi_format([fg(red)], '╠', []).

print_right_frame(a):-
    ansi_format([fg(red)], '╣\n', []).


print_piece(0):-
    ansi_format([fg(black)], ' ', []).

print_piece(1):-
    ansi_format([fg(red)], '⯀', []).

print_piece(2):-
    ansi_format([fg(blue)], '⯀', []).

print_piece_divisor([]).

print_piece_divisor(ROW):-
    not(ROW == []),
    ansi_format([fg(black)], '║', []).

print_connect(0):-
    ansi_format([], '◆', []).

print_connect(1):-
    ansi_format([fg(red)], '◆', []).

print_connect(2):-
    ansi_format([fg(blue)], '◆', []).

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
    ansi_format([fg(black)], '═', []),
    print_connect(CONNECT),
    print_connect_row(ROW).

print_connect_row([]):-
    ansi_format([fg(black)], '═║\n', []).

print_row([PIECE|PROW], [CONNECT|CROW]):-
    print_left_frame(a),
    print_piece_row([PIECE|PROW]),
    ansi_format([fg(black)], '║', []),
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
    ansi_format([fg(black)], '\n╔', []),
    print_upper_frame(8),
    print_rows(PBOARD,CBOARD),
    ansi_format([fg(black)], '╚', []),
    print_lower_frame(8).

print_player(0):-
    ansi_format([fg(blue)], 'Blue player turn', []).

print_player(1):-
    ansi_format([fg(red)], 'Red player turn', []).

display_game(BOARD, PLAYER):-
    print_board(BOARD),
    print_player(PLAYER).