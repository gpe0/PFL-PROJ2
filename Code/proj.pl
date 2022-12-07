:- use_module(library(lists)).
:- use_module(library(random)).
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

% drawYLoop(Index, Y, Offset)
drawYLoop(_, [], _).
drawYLoop(Index, [H|T], Offset) :-
    Color is Index mod 2,
    I1 is Index + 1,
    drawPlace(H, Offset, Color),
    drawYLoop(I1, T, Offset).

% drawY(YIndex, Y, Offset)
drawY(10, Y, 2) :-
    write('10|'),
    drawYLoop(0, Y, 2),
    write('10'), nl.

drawY(N, Y, 2) :-
    format(' ~d|', [N]),
    N1 is N mod 2,
    drawYLoop(N1, Y, 2),
    write(N), nl.

drawY(N, Y, Offset) :-
    write('  |'),
    N1 is N mod 2,
    drawYLoop(N1, Y, Offset), nl.

% drawRow(Index, Row)
drawRow(Index, Row) :-
    drawY(Index, Row, 1),
    drawY(Index, Row, 2),
    drawY(Index, Row, 3).

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

getPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P).
 
setPiece(X, Y, Board, P, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestY),
    nth1(X, ModifiedY, P, RestY),
    nth1(Y, NewBoard, ModifiedY, RestBoard).

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

main :-
    get_initial_board(Board),
    %random_between(0, 1, RandomPlayer),
    gameLoop(0, Board).


clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

read_number(X):-
    read_number_aux(X,0).

read_number_aux(X,Acc):- 
    get_code(C),
    C >= 48,
    C =< 57,
    !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

getY(YCode, Y) :-
    YCode < 97,
    Y is YCode - 64.

getY(YCode, Y) :-
    YCode > 96,
    Y is YCode - 96.

getInput(0, X, Y) :-
    write('[Player 1] Choose the piece to move:'),
    get_code(XCode),
    read_number(YInput),
    getY(XCode, X),
    Y is 11 - YInput,
    nl.

getInput(1, X, Y) :-
    write('[Player 2] Choose the piece to move:'),
    get_code(XCode),
    read_number(YInput),
    getY(XCode, X),
    Y is 11 - YInput,
    nl.

inputHandler(Board, 0, X, Y, Piece) :-
    getInput(0, X, Y),
    getPiece(X, Y, Board, Piece),
    (Piece = 1 ; Piece = 2 ; Piece = 3; inputHandler(Board, 0, X, Y)).


inputHandler(Board, 1, X, Y, Piece) :-
    getInput(1, X, Y),
    getPiece(X, Y, Board, Piece),
    (Piece = 4 ; Piece = 5 ; Piece = 6; inputHandler(Board, 1, X, Y, Piece)).

inputHandler(Board, 2, X, Y, Moves) :-
    getInput(0, X, Y),
    getPiece(X, Y, Board, Piece),
    (member(X-Y, Moves); inputHandler(Board, 2, X, Y, Moves)).

inputHandler(Board, 3, X, Y, Moves) :-
    getInput(0, X, Y),
    getPiece(X, Y, Board, Piece),
    (member(X-Y, Moves); inputHandler(Board, 3, X, Y, Moves)).


gameLoop(0, Board) :-
    drawBoard(Board),
    inputHandler(Board, 0, X, Y, Piece),
    visualize_moves(X, Y, Piece, Board),
    getMoves(X, Y, Piece, Board, Moves),
    inputHandler(Board, 2, ToX, ToY, Moves),
    movePiece(X, Y, ToX, ToY, Board, NewBoard),
    gameLoop(1, NewBoard).

gameLoop(1, Board) :-
    drawBoard(Board),
    inputHandler(Board, 1, X, Y, Piece),
    visualize_moves(X, Y, Piece, Board),
    getMoves(X, Y, Piece, Board, Moves),
    inputHandler(Board, 3, ToX, ToY, Moves),
    movePiece(X, Y, ToX, ToY, Board, NewBoard),
    gameLoop(0, NewBoard).  


    
