drawHeader :- 
    write('      A       B       C       D       E       F       G       H       I       J     '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

drawFooter :-
    write('      A       B       C       D       E       F       G       H       I       J     '), nl.

/*
Piece:
    0 - No Piece
    1 - Elephant
    2 - Mouse
    3 - Lion
    4 - Target

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
drawPlace(4, 2, _) :- write(' --|-- |').
drawPlace(4, _, _) :- write('   |   |').

% Elephant
drawPlace(1, 1, _) :- write('()o o()|').
drawPlace(1, _, _) :- write('  ( )  |').

% Mouse
drawPlace(2, 1, _) :- write('| o o ||').
drawPlace(2, 2, _) :- write(' \\   / |').
drawPlace(2, 3, _) :- write(' -\\_/- |').

% Lion
drawPlace(3, 1, _) :- write(' @@@@@ |').
drawPlace(3, 2, _) :- write('@o\\ /o@|').
drawPlace(3, 3, _) :- write(' @@@@@ |').

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
    test_board1(Board),
    drawBoard(Board).

test_board1(P) :-
    P = [
        [0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 3, 2, 2, 3, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 4, 0, 0, 4, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 4, 0, 0, 4, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 3, 2, 2, 3, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
    ].

test_board2(P) :-
    P = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ].

test_board3(P) :-
    P = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ].

getPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P).

setPiece(Line, Column, Board, Piece, BoardRes) :-
    use_module(library(lists)),
    nth1(Line, Board, BoardLine, RestBoard),
    nth1(Column, BoardLine, _, RestLine),
    nth1(Column, ModifiedLine, Piece, RestLine),
    nth1(Line, BoardRes, ModifiedLine, RestBoard).

% getMoves(X, Y, Piece, Board, Moves)
getMoves(X, Y, 2, Board, Moves) :-
    expand(X, Y, -1, 0, Board, Left),
    expand(X, Y, 1, 0, Board, Right),
    expand(X, Y, 0, 1, Board, Top),
    expand(X, Y, 0, -1, Board, Down),
    % TODO: implement flatten
    append(Left, Right, M1),
    append(M1, Top, M2),
    append(M2, Down, Moves).
getMoves(X, Y, 3, Board, Moves) :-
    expand(X, Y, -1, -1, Board, DL),
    expand(X, Y, 1, 1, Board, TR),
    expand(X, Y, -1, 1, Board, TL),
    expand(X, Y, 1, -1, Board, DR),
    % TODO: implement flatten
    append(DL, DR, D),
    append(TL, TR, T),
    append(D, T, Moves).
getMoves(X, Y, 1, Board, Moves) :-
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
    (V =:= 0 ; V =:= 4),
    A1 = [X-Y|Acc],
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, A1, Result).
expand_acc(_, _, _, _, _, Acc, Acc).
    


visualize_moves(X, Y, Piece, Board) :-
    getMoves(X, Y, Piece, Board, Moves),
    setMoves(Board, Moves, NewBoard),
    drawBoard(NewBoard).

setMoves(Board, [], Board).
setMoves(Board, [X-Y|T], NewBoard) :-
    setPiece(Y,X,Board,8,B1),
    setMoves(B1,T,NewBoard).

test_vis(X, Y, Piece) :-
    test_board1(B),
    visualize_moves(X, Y, Piece, B).


test_getPiece(X, Y) :-
    use_module(library(lists)),
    test_board1(B),
    getPiece(X, Y, B, P),
    write(P).

test_expand_acc(X, Y, StepX, StepY, Res) :-
    use_module(library(lists)),
    test_board2(B),
    expand_acc(X, Y, StepX, StepY, B, [], Res).

test_expand_1(Res) :-
    test_board2(B),
    expand_acc(1,1,-1,0,B,[],Res).

test_expand_2(Res) :-
    test_board2(B),
    expand_acc(1,1,1,0,B,[],Res).

test_expand_3(Res) :-
    test_board2(B),
    expand_acc(1,1,0,-1,B,[],Res).

test_expand_4(Res) :-
    test_board2(B),
    expand_acc(1,1,0,1,B,[],Res).

test_expand_5(Res) :-
    test_board2(B),
    expand_acc(4,1,0,1,B,[],Res).

test_expand_hor1(Res) :-
    test_board2(B),
    expandHorizontally(5,5,B,Res).

test_expand_hor2(Res) :-
    test_board3(B),
    expandHorizontally(5,5,B,Res).

test_rat_moves(Res) :-
    test_board1(B),
    getMoves(5, 5, 2, B, Res).

test_elp_moves(Res) :-
    test_board1(B),
    getMoves(5, 5, 1, B, Res).