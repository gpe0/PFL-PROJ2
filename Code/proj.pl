:- use_module(library(lists)).
:- use_module(boards).

drawHeader :- 
    write('      A       B       C       D       E       F       G       H       I       J     '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

drawFooter :-
    write('      A       B       C       D       E       F       G       H       I       J     '), nl.

/*
Piece:
    0 - No Piece
    1 - Elephant (Player 1)
    2 - Mouse (Player 1)
    3 - Lion (Player 1)
    4 - Elephant (Player 2)
    5 - Mouse (Player 2)
    6 - Lion (Player 2)
    7 - Target

Offset:
    1 - Top
    2 - Mid
    2 - Bot

Colors:
    0 - White
    1 - Black (dots)
*/

%drawPlace(Piece, Offset, Color).

% Empty
drawPlace(0, _, 0) :- write('       |').
drawPlace(0, _, 1) :- write(' . . . |').

% Target
drawPlace(7, 2, _) :- write(' --|-- |').
drawPlace(7, _, _) :- write('   |   |').

% Elephant
drawPlace(1, 1, _) :- write('()o o()|').
drawPlace(1, _, _) :- write('  ( )  |').
drawPlace(4, 1, _) :- write('()o o()|').
drawPlace(4, _, _) :- write('  ( )  |').

% Mouse
drawPlace(2, 1, _) :- write('| o o ||').
drawPlace(2, 2, _) :- write(' \\   / |').
drawPlace(2, 3, _) :- write(' -\\_/- |').

drawPlace(5, 1, _) :- write('| o o ||').
drawPlace(5, 2, _) :- write(' \\   / |').
drawPlace(5, 3, _) :- write(' -\\_/- |').

% Lion
drawPlace(3, 1, _) :- write(' @@@@@ |').
drawPlace(3, 2, _) :- write('@o\\ /o@|').
drawPlace(3, 3, _) :- write(' @@@@@ |').

drawPlace(6, 1, _) :- write(' @@@@@ |').
drawPlace(6, 2, _) :- write('@o\\ /o@|').
drawPlace(6, 3, _) :- write(' @@@@@ |').

% Possible Move
drawPlace(8, _, _) :- write('XXXXXXX|').

% drawLineLoop(Index, Line, Offset)
drawLineLoop(_, [], _).
drawLineLoop(Index, [H|T], Offset) :-
    Color is Index mod 2,
    I1 is Index + 1,
    drawPlace(H, Offset, Color),
    drawLineLoop(I1, T, Offset).

% drawLine(LineIndex, Line, Offset)
drawLine(10, Line, 2) :-
    write('10|'),
    drawLineLoop(0, Line, 2),
    write('10'), nl.

drawLine(N, Line, 2) :-
    format(' ~d|', [N]),
    N1 is N mod 2,
    drawLineLoop(N1, Line, 2),
    write(N), nl.

drawLine(N, Line, Offset) :-
    write('  |'),
    N1 is N mod 2,
    drawLineLoop(N1, Line, Offset), nl.

% drawRow(Index, Row)
drawRow(Index, Row) :-
    drawLine(Index, Row, 1),
    drawLine(Index, Row, 2),
    drawLine(Index, Row, 3).

% drawRowLoop(Index, Row)
drawRowLoop(_, []).
drawRowLoop(Index, [H|T]) :-
    drawRow(Index, H),
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    I1 is Index - 1,
    drawRowLoop(I1, T).

% drawBoard(Board)
drawBoard(Board) :- 
    drawHeader,
    drawRowLoop(10, Board),
    drawFooter.

% For now, empty hard-coded board.
drawBoard :-
    get_initial_board(Board),
    drawBoard(Board).

getPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P).

setPiece(X, Y, Board, P, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestLine),
    nth1(X, ModifiedLine, P, RestLine),
    nth1(Y, NewBoard, ModifiedLine, RestBoard).

movePiece(X1, Y1, X2, Y2, Board, NewBoard) :-
    (Y1 = 4 ; Y1 = 7),
    (X1 = 4 ; X1 = 7),
    getPiece(X1, Y1, Board, Piece),
    setPiece(X1, Y1, Board, 7, TempBoard), % clear the previous space
    setPiece(X2, Y2, TempBoard, Piece, NewBoard).

movePiece(X1, Y1, X2, Y2, Board, NewBoard) :-
    getPiece(X1, Y1, Board, Piece),
    setPiece(X1, Y1, Board, 0, TempBoard), % clear the previous space
    setPiece(X2, Y2, TempBoard, Piece, NewBoard).    

% getMoves(X, Y, Piece, Board, Moves)
getMoves(X, Y, P, Board, Moves) :-
    (P = 2; P = 5),
    expand(X, Y, -1, 0, Board, Left),
    expand(X, Y, 1, 0, Board, Right),
    expand(X, Y, 0, 1, Board, Top),
    expand(X, Y, 0, -1, Board, Down),
    % TODO: implement flatten
    append(Left, Right, M1),
    append(M1, Top, M2),
    append(M2, Down, Moves).
getMoves(X, Y, P, Board, Moves) :-
    (P = 3; P = 6),
    expand(X, Y, -1, -1, Board, DL),
    expand(X, Y, 1, 1, Board, TR),
    expand(X, Y, -1, 1, Board, TL),
    expand(X, Y, 1, -1, Board, DR),
    % TODO: implement flatten
    append(DL, DR, D),
    append(TL, TR, T),
    append(D, T, Moves).
getMoves(X, Y, P, Board, Moves) :-
    (P = 1; P = 4),
    getMoves(X,Y,2,Board,Moves1),
    getMoves(X,Y,3,Board,Moves2),
    append(Moves1,Moves2,Moves).

expand(X, Y, StepX, StepY, Board, Moves) :-
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, [], Moves).

% expand_acc(X, Y, StepX, StepY, Acc, Result)
expand_acc(0, _, _, _, _, Acc, Acc).
expand_acc(11, _, _, _, _, Acc, Acc).
expand_acc(_, 0, _, _, _, Acc, Acc).
expand_acc(_, 11, _, _, _, Acc, Acc).
expand_acc(X, Y, StepX, StepY, Board, Acc, Result) :-
    getPiece(X, Y, Board, V),
    (V =:= 0 ; V =:= 7),
    A1 = [X-Y|Acc],
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, A1, Result).
expand_acc(_, _, _, _, _, Acc, Acc).
    
visualize_moves(X, Y, Piece, Board) :-
    getMoves(X, Y, Piece, Board, Moves),
    drawMoves(Board, Moves, NewBoard),
    drawBoard(NewBoard).

drawMoves(Board, [], Board).
drawMoves(Board, [X-Y|T], NewBoard) :-
    setPiece(X,Y,Board,8,B1),
    drawMoves(B1,T,NewBoard).

test_vis(X, Y, Piece) :-
    get_initial_board(B),
    visualize_moves(X, Y, Piece, B).