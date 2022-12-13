:- use_module(library(lists)).
:- use_module(library(random)).

:- ensure_loaded('boards.pl').
:- ensure_loaded('view.pl').

% =========================================================================
% BOARD
% =========================================================================

elephant(1).
elephant(4).
lion(3).
lion(6).
mouse(2).
mouse(5).
target(7).
empty(0).
empty(7).

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
isXorY4or7(4, 4).
isXorY4or7(4, 7).
isXorY4or7(7, 4).
isXorY4or7(7, 7).

movePiece(X1, Y1, X2, Y2, Board, NewBoard) :-
    isXorY4or7(X1, Y1),
    getPiece(X1, Y1, Board, Piece),
    setPiece(X1, Y1, Board, 7, TempBoard), % clear the previous space
    setPiece(X2, Y2, TempBoard, Piece, NewBoard).

movePiece(X1, Y1, X2, Y2, Board, NewBoard) :-
    getPiece(X1, Y1, Board, Piece),
    setPiece(X1, Y1, Board, 0, TempBoard), % clear the previous space
    setPiece(X2, Y2, TempBoard, Piece, NewBoard).    

% getMoves(X, Y, Piece, Board, Player, Moves)
getMoves(X, Y, P, Board, Player, Moves) :-
    mouse(P),
    expand_up_down(X, Y, Board, Player, P, Moves).
getMoves(X, Y, P, Board, Player, Moves) :-
    lion(P),
    expand_diagonal(X, Y, Board, Player, P, Moves).
getMoves(X, Y, P, Board, Player, Moves) :-
    elephant(P),
    expand_up_down(X, Y, Board, Player, P, Moves1),
    expand_diagonal(X, Y, Board, Player, P, Moves2),
    append(Moves1, Moves2, Moves).

expand_up_down(X, Y, Board, Player, P, Moves) :-
    expand(X, Y, -1, 0, Board, Player, P, Left),
    expand(X, Y, 1, 0, Board, Player, P, Right),
    expand(X, Y, 0, 1, Board, Player, P, Top),
    expand(X, Y, 0, -1, Board, Player, P, Down),
    append(Left, Right, M1),
    append(M1, Top, M2),
    append(M2, Down, Moves).

expand_diagonal(X, Y, Board, Player, P, Moves) :-
    expand(X, Y, -1, -1, Board, Player, P, DL),
    expand(X, Y, 1, 1, Board, Player, P, TR),
    expand(X, Y, -1, 1, Board, Player, P, TL),
    expand(X, Y, 1, -1, Board, Player, P, DR),
    append(DL, DR, D),
    append(TL, TR, T),
    append(D, T, Moves).

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
    empty(V),
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
    random(0, 2, RandomPlayer),
    gameLoop(RandomPlayer, Board).


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

gameLoop(_, Board) :-
    game_over(Board, Winner),
    Winner < 3,
    !,
    format('WINNER IS PLAYER~d', [Winner]).

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

scared(P1, P2) :- lion(P1), elephant(P2).
scared(P1, P2) :- elephant(P1), mouse(P2).
scared(P1, P2) :- mouse(P1), lion(P2).

findScaredPieces(Board, Player, Pieces) :-
    valid_pieces(Board, Player, ValidPieces),
    findScaredPiecesAux(Board, Player, ValidPieces, [], Pieces).

findScaredPiecesAux(_, _, [], Acc, Acc).
findScaredPiecesAux(Board, Player, [X-Y-P|T], Acc, ScaredPieces) :-
    isScared(X, Y, Board, Player, P),
    findScaredPiecesAux(Board, Player, T, [X-Y-P|Acc], ScaredPieces).
findScaredPiecesAux(Board, Player, [_|T], Acc, ScaredPieces) :-
    findScaredPiecesAux(Board, Player, T, Acc, ScaredPieces).

adjacent(X, Y, X1, Y) :- X1 is X + 1.
adjacent(X, Y, X1, Y) :- X1 is X - 1.
adjacent(X, Y, X1, Y1) :- X1 is X + 1, Y1 is Y + 1.
adjacent(X, Y, X1, Y1) :- X1 is X + 1, Y1 is Y - 1.
adjacent(X, Y, X1, Y1) :- X1 is X - 1, Y1 is Y - 1.
adjacent(X, Y, X1, Y1) :- X1 is X - 1, Y1 is Y + 1.
adjacent(X, Y, X, Y1) :- Y1 is Y + 1.
adjacent(X, Y, X, Y1) :- Y1 is Y - 1.

isScared(X, Y, Board, Player, P) :-
    OtherPlayer is 1 - Player,
    adjacent(X, Y, X1, Y1),
    getPiece(X1, Y1, Board, NeighPiece),
    playerPiece(NeighPiece, OtherPlayer),
    scared(P, NeighPiece).

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

boolToInt(Pred, 1) :- call(Pred).
boolToInt(_, 0).

getPlayerPoints(P1, P2, P3, P4, Player, Points) :-
    boolToInt(playerPiece(P1, Player), V1),
    boolToInt(playerPiece(P2, Player), V2),
    boolToInt(playerPiece(P3, Player), V3),
    boolToInt(playerPiece(P4, Player), V4),
    Points is V1 + V2 + V3 + V4.

% game_over(+GameState, -Winner)
game_over(Board, Winner) :-
    getPiece(4, 4, Board, P1),
    getPiece(7, 4, Board, P2),
    getPiece(4, 7, Board, P3),
    getPiece(7, 7, Board, P4),
    game_over_winner(P1, P2, P3, P4, Winner).

gameOver(Winner) :-
    board(Board),
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
    game_over(B1, 1),
    game_over(B2, 2),
    game_over(B3, 2),
    game_over(B4, 2),
    game_over(B5, 3),
    game_over(B6, 3).

% =========================================================================
% INPUT
% =========================================================================

getInput([]) :- peek_code(10), get_code(10), !.
getInput([H|T]) :-
    get_code(H),
    getInput(T).

readNumber(L, Res) :- readNumberAux(L, 0, Res).

readNumberAux([], Acc, Acc).
readNumberAux([C|T], Acc, Res):- 
    C >= 48,
    C =< 57,
    !,
    Acc1 is 10*Acc + (C - 48),
    readNumberAux(T, Acc1, Res).
readNumberAux(_, Acc, Acc).

parseOption(L, Option) :-
    readNumber(L, Option),
    Option > 0,
    Option < 11.
parseOption(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% =========================================================================
% PLAY
% =========================================================================

:- dynamic playerType/2.
:- dynamic board/1.
:- dynamic playerTurn/1.

/**
 * Player Type
 * - 0 => Human
 * - 1 => Random Bot
 * - 2 => Greedy Bot
 */

displayWinnerMessage(Winner) :-
    board(FinalBoard),
    drawBoard(FinalBoard),
    % To change %
    format('WINNER IS ~d', [Winner]).

turn(Board, _) :-
    game_over(Board, Winner),
    Winner < 3,
    displayWinnerMessage(Winner),
    !,
    fail.
turn(Board, Player) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    !,
    turn_action(Board, Player, ScaredPieces).
turn(Board, Player) :-
    valid_pieces(Board, Player, Pieces),
    turn_action(Board, Player, Pieces).

turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 0),
    turn_human(Board, Player, PiecesToMove).

turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 1),
    turn_random(Board, Player, PiecesToMove).

turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 2),
    turn_greedy(Board, Player, PiecesToMove).

turn_human(Board, Player, PiecesToMove) :-
    inputHandler(Board, Player, X, Y, Piece),
    visualize_moves(X, Y, Piece, Board, 0, Moves),
    Aux is Player + 2,
    inputHandler(Board, Aux, Moves, ToX, ToY, _),
    movePiece(X, Y, ToX, ToY, Board, NewBoard),
    !,
    setBoard(NewBoard).

turn_random(Board, Player, PiecesToMove) :-
    random_member(X-Y-Piece, PiecesToMove),
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves),
    movePiece(X, Y, XF, YF, Board, NewBoard),
    !,
    setBoard(NewBoard).

turn_greedy(Board, Player, Pieces) :-
    generateBoards(Board, Player, Pieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    !,
    random_member(MoveChosen, BestBoards),
    setBoard(MoveChosen).

displayInitalMessage :-
    write('===== Barca Board Game ====='), nl,
    write('          PFL 22/23         '), nl,
    write('============================'), nl, nl.

displayGamemodes :-
    write('         GAME MODES         '), nl,
    write('1. Human      vs Human     '), nl,
    write('2. Human      vs Random Bot'), nl,
    write('3. Human      vs Greedy Bot'), nl,
    write('4. Random Bot vs Human     '), nl,
    write('5. Random Bot vs Random Bot'), nl,
    write('6. Random Bot vs Greedy Bot'), nl,
    write('7. Greedy Bot vs Human     '), nl,
    write('8. Greedy Bot vs Random Bot'), nl,
    write('9. Greedy Bot vs Greedy Bot'), nl.

getGamemode(Option) :-
    getInput(Input),
    parseOption(Input, Option),
    retractall(playerType(_,_)).

setGamemode(1) :- asserta(playerType(0, 0)), asserta(playerType(1, 0)).
setGamemode(2) :- asserta(playerType(0, 0)), asserta(playerType(1, 1)).
setGamemode(3) :- asserta(playerType(0, 0)), asserta(playerType(1, 2)).
setGamemode(4) :- asserta(playerType(0, 1)), asserta(playerType(1, 0)).
setGamemode(5) :- asserta(playerType(0, 1)), asserta(playerType(1, 1)).
setGamemode(6) :- asserta(playerType(0, 1)), asserta(playerType(1, 2)).
setGamemode(7) :- asserta(playerType(0, 2)), asserta(playerType(1, 0)).
setGamemode(8) :- asserta(playerType(0, 2)), asserta(playerType(1, 1)).
setGamemode(9) :- asserta(playerType(0, 2)), asserta(playerType(1, 2)).

readPlayerMode :-
    repeat,
    displayGamemodes,
    getGamemode(Option),
    setGamemode(Option).

menu :-
    displayInitalMessage,
    readPlayerMode.

switchPlayer :-
    playerTurn(0),
    retract(playerTurn(0)),
    asserta(playerTurn(1)),
    !. 
switchPlayer :-
    playerTurn(1),
    retract(playerTurn(1)),
    asserta(playerTurn(0)). 

setBoard(Board) :-
    retractall(board(_)),
    asserta(board(Board)).

startGame :-
    get_initial_board(InitialBoard),
    retractall(playerTurn(_)),
    asserta(playerTurn(0)),
    setBoard(InitialBoard),
    !,
    repeat,
    board(B),
    drawBoard(B),
    playerTurn(Player),
    turn(B, Player),
    switchPlayer,
    gameOver(Winner),
    Winner < 3,
    displayWinnerMessage(Winner).

play :-
    menu,
    startGame.