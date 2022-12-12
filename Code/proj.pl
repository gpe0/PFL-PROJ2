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
drawPlace(1, 2, _) :- write('  (1)  |').
drawPlace(1, _, _) :- write('  ( )  |').
drawPlace(4, 1, _) :- write('()o o()|').
drawPlace(4, 2, _) :- write('  (2)  |').
drawPlace(4, _, _) :- write('  ( )  |').

% Mouse
drawPlace(2, 1, _) :- write('| o o ||').
drawPlace(2, 2, _) :- write(' \\ 1 / |').
drawPlace(2, 3, _) :- write(' -\\_/- |').

drawPlace(5, 1, _) :- write('| o o ||').
drawPlace(5, 2, _) :- write(' \\ 2 / |').
drawPlace(5, 3, _) :- write(' -\\_/- |').

% Lion
drawPlace(3, 1, _) :- write(' @@@@@ |').
drawPlace(3, 2, _) :- write('@o\\1/o@|').
drawPlace(3, 3, _) :- write(' @@@@@ |').

drawPlace(6, 1, _) :- write(' @@@@@ |').
drawPlace(6, 2, _) :- write('@o\\2/o@|').
drawPlace(6, 3, _) :- write(' @@@@@ |').

% Possible Move
drawPlace(8, _, _) :- write('XXXXXXX|').

% drawLineLoop(Index, Y, Offset)
drawLineLoop(_, [], _).
drawLineLoop(Index, [H|T], Offset) :-
    Color is Index mod 2,
    I1 is Index + 1,
    drawPlace(H, Offset, Color),
    drawLineLoop(I1, T, Offset).

% drawLine(YIndex, Y, Offset)
drawLine(10, Y, 2) :-
    write('10|'),
    drawLineLoop(0, Y, 2),
    write('10'), nl.

drawLine(N, Y, 2) :-
    format(' ~d|', [N]),
    N1 is N mod 2,
    drawLineLoop(N1, Y, 2),
    write(N), nl.

drawLine(N, Y, Offset) :-
    write('  |'),
    N1 is N mod 2,
    drawLineLoop(N1, Y, Offset), nl.

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

getPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P).
getPiece(_, _, _, 99). % Fail prevention (AI)
 
setPiece(X, Y, Board, P, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestY),
    nth1(X, ModifiedY, P, RestY),
    nth1(Y, NewBoard, ModifiedY, RestBoard).

% =========================================================================
% MOVEMENT
% =========================================================================

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

% getMoves(X, Y, Piece, Board, Player, Moves)
getMoves(X, Y, P, Board, Player, Moves) :-
    (P = 2; P = 5),
    expand(X, Y, -1, 0, Board, Player, P, Left),
    expand(X, Y, 1, 0, Board, Player, P, Right),
    expand(X, Y, 0, 1, Board, Player, P, Top),
    expand(X, Y, 0, -1, Board, Player, P, Down),
    append(Left, Right, M1),
    append(M1, Top, M2),
    append(M2, Down, Moves).
getMoves(X, Y, P, Board, Player, Moves) :-
    (P = 3; P = 6),
    expand(X, Y, -1, -1, Board, Player, P, DL),
    expand(X, Y, 1, 1, Board, Player, P, TR),
    expand(X, Y, -1, 1, Board, Player, P, TL),
    expand(X, Y, 1, -1, Board, Player, P, DR),
    append(DL, DR, D),
    append(TL, TR, T),
    append(D, T, Moves).
getMoves(X, Y, P, Board, Player, Moves) :-
    (P = 1; P = 4),
    getMoves(X, Y, 2, Board, Player, Moves1),
    getMoves(X, Y, 3, Board, Player, Moves2),
    append(Moves1, Moves2, Moves).

expand(X, Y, StepX, StepY, Board, Player, Piece, Moves) :-
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, [], Moves).

% expand_acc(X, Y, StepX, StepY, Acc, Result)
expand_acc(0, _, _, _, _, _, _, Acc, Acc).
expand_acc(11, _, _, _, _, _, _, Acc, Acc).
expand_acc(_, 0, _, _, _, _, _, Acc, Acc).
expand_acc(_, 11, _, _, _, _, _, Acc, Acc).
expand_acc(X, Y, StepX, StepY, Board, Player, Piece, Acc, Result) :-
    isScared(X, Y, Board, Player, Piece),
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, Acc, Result).
expand_acc(X, Y, StepX, StepY, Board, Player, Piece, Acc, Result) :-
    getPiece(X, Y, Board, V),
    (V =:= 0 ; V =:= 7),
    A1 = [X-Y|Acc],
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, A1, Result).
expand_acc(_, _, _, _, _, _, _, Acc, Acc).
    
visualize_moves(X, Y, Piece, Board, Player, Moves) :-
    getMoves(X, Y, Piece, Board, Player, Moves),
    drawMoves(Board, Moves, NewBoard),
    drawBoard(NewBoard).

drawMoves(Board, [], Board).
drawMoves(Board, [X-Y|T], NewBoard) :-
    setPiece(X,Y,Board,8,B1),
    drawMoves(B1,T,NewBoard).

test_vis(X, Y, Piece) :-
    get_initial_board(B),
    visualize_moves(X, Y, Piece, B, 0, _).

% =========================================================================
% INPUT
% =========================================================================

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

getX(XCode, X) :-
    XCode < 97,
    X is XCode - 64.

getX(XCode, X) :-
    XCode > 96,
    X is XCode - 96.

getInput(0, X, Y) :-
    write('[Player 1] Choose the piece to move:'),
    get_code(XCode),
    read_number(YInput),
    getX(XCode, X),
    Y is 11 - YInput,
    nl.

getInput(1, X, Y) :-
    write('[Player 2] Choose the piece to move:'),
    get_code(XCode),
    read_number(YInput),
    getX(XCode, X),
    Y is 11 - YInput,
    nl.

%inputHandler(+Board, +manageMode, ?X, ?Y, ?Piece)
inputHandler(Board, 0, X, Y, Piece) :-
    getInput(0, TempX, TempY),
    getPiece(TempX, TempY, Board, TempPiece),
    (((TempPiece = 1 ; TempPiece = 2 ; TempPiece = 3), X = TempX, Y = TempY, Piece = TempPiece); inputHandler(Board, 0, X, Y, Piece)).


inputHandler(Board, 1, X, Y, Piece) :-
    getInput(1, TempX, TempY),
    getPiece(TempX, TempY, Board, TempPiece),
    (((TempPiece = 4 ; TempPiece = 5 ; TempPiece = 6), X = TempX, Y = TempY, Piece = TempPiece); inputHandler(Board, 1, X, Y, Piece)).


%inputHandler(+Board, +manageMode, +Moves, ?X, ?Y, ?Piece)
inputHandler(Board, 2, Moves, X, Y, Piece) :-
    getInput(0, TempX, TempY),
    getPiece(TempX, TempY, Board, TempPiece),
    ((member(X-Y, Moves), X = TempX, Y = TempY, Piece = TempPiece); inputHandler(Board, 2, Moves, X, Y, Piece)).

inputHandler(Board, 3, Moves, X, Y, Piece) :-
    getInput(1, TempX, TempY),
    getPiece(TempX, TempY, Board, TempPiece),
    ((member(X-Y, Moves), X = TempX, Y = TempY, Piece = TempPiece); inputHandler(Board, 3, Moves, X, Y, Piece)).


gameLoop(0, Board) :-
    drawBoard(Board),
    inputHandler(Board, 0, X, Y, Piece),
    visualize_moves(X, Y, Piece, Board, 0, Moves),
    inputHandler(Board, 2, Moves, ToX, ToY, _),
    movePiece(X, Y, ToX, ToY, Board, NewBoard),
    gameLoop(1, NewBoard).

gameLoop(1, Board) :-
    drawBoard(Board),
    inputHandler(Board, 1, X, Y, Piece),
    visualize_moves(X, Y, Piece, Board, 1, Moves),
    inputHandler(Board, 3, Moves, ToX, ToY, _),
    movePiece(X, Y, ToX, ToY, Board, NewBoard),
    gameLoop(0, NewBoard).  

% =========================================================================
% AI RANDOM BOT
% =========================================================================

% isEmptyPiece
isEmptyPiece(0).
isEmptyPiece(7).

valid_pieces(Board, Player, Pieces) :-
    findall(X-Y-P, valid_pieces_aux(X, Y, Board, Player, P), Pieces).

valid_pieces_aux(X, Y, Board, Player, P) :-
    getPiece(X, Y, Board, P),
    \+isEmptyPiece(P),
    playerPiece(P, Player).

getRandomPiece(Board, Player, X-Y-Piece) :-
    valid_pieces(Board, Player, ValidPieces),
    random_member(X-Y-Piece, ValidPieces).

getRandomMove(Board, Player, X-Y-Piece, XF-YF) :-
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves).

test :-
    get_initial_board(Board),
    getRandomPiece(Board, 1, X-Y-Piece),
    getRandomMove(Board, 1, X-Y-Piece, XF-YF),
    write('Piece: '), write(Piece), nl,
    write('Current Position: '), write(X-Y), nl,
    write('Move Position: '), write(XF-YF).

% =========================================================================
% AI BIG BRAIN BOT
% =========================================================================

generateBoards(Board, Player, [X-Y-Piece|RestPieces], Acc, Boards) :-
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    generateBoardsAux(Board, X-Y-Piece, ValidMoves, [], NewBoards),
    append(Acc, NewBoards, Aux),
    generateBoards(Board, Player, RestPieces, Aux, Boards).
generateBoards(_, _, _, Acc, Acc).

generateBoardsAux(Board, X-Y-Piece, [XF-YF|RestMoves], Acc, Boards) :-
    movePiece(X, Y, XF, YF, Board, NewBoard),
    generateBoardsAux(Board, X-Y-Piece, RestMoves, [NewBoard | Acc], Boards).
generateBoardsAux(_,_,_,Acc,Acc).

evaluate(Board, Player, Value) :-
    getPiece(4, 4, Board, P1),
    getPiece(7, 4, Board, P2),
    getPiece(4, 7, Board, P3),
    getPiece(7, 7, Board, P4),
    getPlayerPoints(P1, P2, P3, P4, Player, Points),
    findScaredPieces(Board, Player, ScaredPlayer),
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    length(ScaredPlayer, NumScaredPlayer),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    Value is Points * -100 + NumScaredPlayer - NumScaredOtherPlayer.

evaluateBoards([], _, []).
evaluateBoards([B|RestBoards], Player, [V-B|RT]) :-
    evaluate(B, Player, V),
    evaluateBoards(RestBoards, Player, RT).

getBestBoards([V-B|RestBoards], V, [B|T]) :-
    getBestBoards(RestBoards, V, T).
getBestBoards(_, _, []).

test_all :-
    Player = 1,
    get_initial_board(Board),
    valid_pieces(Board, Player, ValidPieces),
    generateBoards(Board, Player, ValidPieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    random_member(MoveChosen, BestBoards),
    drawBoard(MoveChosen).

drawBoards([]).
drawBoards([H|T]) :-
    drawBoard(H),
    drawBoards(T).

bots_main :-
    get_initial_board(Board),
    drawBoard(Board),
    bots_loop(0, Board).

bots_loop(_, Board) :-
    game_over(Board, Winner),
    Winner < 3,
    format('WINNER IS ~d', [Winner]).
bots_loop(Player, Board) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    write('Has scared pieces.'), nl, write(ScaredPieces), nl,
    bots_loop_aux(Player, Board, ScaredPieces).
bots_loop(Player, Board) :-
    valid_pieces(Board, Player, Pieces),
    write('Free game.'), nl,
    bots_loop_aux(Player, Board, Pieces).

bots_loop_aux(Player, Board, Pieces) :-
    generateBoards(Board, Player, Pieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    random_member(MoveChosen, BestBoards),

    Other is 1 - Player,
    drawBoard(MoveChosen),
    bots_loop(Other, MoveChosen).

% =========================================================================
% FEAR MECHANIC
% =========================================================================

scared(2,6).
scared(5,3).
scared(3,4).
scared(6,1).
scared(1,5).
scared(4,2).

findScaredPieces(Board, Player, Pieces) :-
    valid_pieces(Board, Player, ValidPieces),
    findScaredPiecesAux(Board, Player, ValidPieces, [], Pieces).

findScaredPiecesAux(_, _, [], Acc, Acc).
findScaredPiecesAux(Board, Player, [X-Y-P|T], Acc, ScaredPieces) :-
    isScared(X, Y, Board, Player, P),
    findScaredPiecesAux(Board, Player, T, [X-Y-P|Acc], ScaredPieces).
findScaredPiecesAux(Board, Player, [_|T], Acc, ScaredPieces) :-
    findScaredPiecesAux(Board, Player, T, Acc, ScaredPieces).

% UGLY CODE ALERT %
isScared(X, Y, Board, Player, P) :-
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    getPiece(X, Y1, Board, P1),
    getPiece(X, Y2, Board, P2),
    getPiece(X1, Y, Board, P3),
    getPiece(X1, Y1, Board, P4),
    getPiece(X1, Y2, Board, P5),
    getPiece(X2, Y, Board, P6),
    getPiece(X2, Y1, Board, P7),
    getPiece(X2, Y2, Board, P8),
    OtherPlayer is 1 - Player,
    ((playerPiece(P1, OtherPlayer), scared(P, P1)); 
    (playerPiece(P2, OtherPlayer), scared(P, P2));
    (playerPiece(P3, OtherPlayer), scared(P, P3));
    (playerPiece(P4, OtherPlayer), scared(P, P4));
    (playerPiece(P5, OtherPlayer), scared(P, P5));
    (playerPiece(P6, OtherPlayer), scared(P, P6));
    (playerPiece(P7, OtherPlayer), scared(P, P7));
    (playerPiece(P8, OtherPlayer), scared(P, P8))).

test_scare :-
    board_scared_1(B),
    findScaredPieces(B, 0, Pieces),
    write(Pieces).

test_scare_mov :-
    board_scared_2(B),
    visualize_moves(5, 5, 1, B, 0, _).

test_fix_bug :-
    board_bug_scared(B),
    bots_loop(1, B).

% =========================================================================
% GAME OVER
% =========================================================================

% playerPiece(Piece, Player)
playerPiece(1, 0).
playerPiece(2, 0).
playerPiece(3, 0).
playerPiece(4, 1).
playerPiece(5, 1).
playerPiece(6, 1).

% playerPiece(Piece, Player, Value)
% Value = 1 on success
% Value = 0 on fail
% This function will help game over be more efficient
playerPieceValue(1, 0, 1).
playerPieceValue(2, 0, 1).
playerPieceValue(3, 0, 1).
playerPieceValue(4, 1, 1).
playerPieceValue(5, 1, 1).
playerPieceValue(6, 1, 1).
playerPieceValue(_, _, 0).

getPlayerPoints(P1, P2, P3, P4, Player, Points) :-
    playerPieceValue(P1, Player, V1),
    playerPieceValue(P2, Player, V2),
    playerPieceValue(P3, Player, V3),
    playerPieceValue(P4, Player, V4),
    Points is V1 + V2 + V3 + V4.

% game_over(+GameState, -Winner)
game_over(Board, Winner) :-
    getPiece(4, 4, Board, P1),
    getPiece(7, 4, Board, P2),
    getPiece(4, 7, Board, P3),
    getPiece(7, 7, Board, P4),
    game_over_winner(P1, P2, P3, P4, Winner).

% game_over_winner(P1, P2, P3, P4, Winner)
game_over_winner(P1, P2, P3, P4, 1) :-
    getPlayerPoints(P1, P2, P3, P4, 0, Points),
    Points > 2.
game_over_winner(P1, P2, P3, P4, 2) :-
    getPlayerPoints(P1, P2, P3, P4, 1, Points),
    Points > 2.
game_over_winner(_,_,_,_,3).

test_game_over :-
    board_game_is_over1(B1),
    board_game_is_over2(B2),
    board_game_is_over3(B3),
    board_game_is_over4(B4),
    board_game_not_over1(B5),
    board_game_not_over2(B6),
    game_over(B1, 0),
    game_over(B2, 1),
    game_over(B3, 1),
    game_over(B4, 1),
    game_over(B5, 2),
    game_over(B6, 2).
