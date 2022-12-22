:- use_module(library(lists)).
:- use_module(library(random)).

:- ensure_loaded('boards.pl').
:- ensure_loaded('view.pl').
:- ensure_loaded('tests.pl').
:- ensure_loaded('stats.pl').

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
    expand_up_down(X, Y, Board, Player, P, Moves),
    !.
getMoves(X, Y, P, Board, Player, Moves) :-
    lion(P),
    expand_diagonal(X, Y, Board, Player, P, Moves),
    !.
getMoves(X, Y, P, Board, Player, Moves) :-
    elephant(P),
    expand_up_down(X, Y, Board, Player, P, Moves1),
    expand_diagonal(X, Y, Board, Player, P, Moves2),
    append(Moves1, Moves2, Moves),
    !.

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

% =========================================================================
% INPUT
% =========================================================================

main :-
    get_initial_board(Board),
    random(0, 2, RandomPlayer),
    gameLoop(RandomPlayer, Board).

letterToIndex(Letter, X) :-
    Letter > 64,
    Letter < 75,
    X is Letter - 64.

letterToIndex(Letter, X) :-
    Letter > 96,
    Letter < 107,
    X is Letter - 96.

writeStatus(0) :- write('[Player 1] Choose the piece to move:').
writeStatus(1) :- write('[Player 2] Choose the piece to move:').
writeStatus(2) :- write('[Player 1] Choose the piece destination:').
writeStatus(3) :- write('[Player 2] Choose the piece destination:').

getInput(Mode, X, Y) :-
    writeStatus(Mode),
    getInput([Letter|Number]),
    readNumber(Number, YInput),
    letterToIndex(Letter, X),
    Y is 11 - YInput.
getInput(Mode, X, Y) :-
    write('error: Invalid input, try again!'), nl, nl,
    getInput(Mode, X, Y).

readPositon(X, Y, Piece, Board, Player) :-
    getInput(Player, X, Y),
    getPiece(X, Y, Board, Piece),
    playerPiece(Piece, Player).

readDestination(X, Y, Moves, Player) :-
    Mode is Player + 2,
    getInput(Mode, X, Y),
    member(X-Y, Moves).

% TO DELETE
gameLoop(_, Board) :-
    gameOver(Board, Winner),
    Winner < 3,
    !,
    format('WINNER IS PLAYER~d', [Winner]).

% TO DELETE
gameLoop(Player, Board) :-
    drawBoard(Board),
    readPositon(X, Y, Piece, Board, Player),
    visualize_moves(X, Y, Piece, Board, Player, Moves),
    readDestination(XF, YF, Moves, Player),
    movePiece(X, Y, XF, YF, Board, NewBoard),
    OtherPlayer is 1 - Player,
    gameLoop(OtherPlayer, NewBoard).

% =========================================================================
% AI RANDOM BOT
% =========================================================================

validPieces(Board, Player, Pieces) :-
    findall(X-Y-P, validPiecesAux(X, Y, Board, Player, P), Pieces).

validPiecesAux(X, Y, Board, Player, P) :-
    getPiece(X, Y, Board, P),
    playerPiece(P, Player),
    getMoves(X, Y, P, Board, Player, [_|_]).

getRandomPiece(Board, Player, X-Y-Piece) :-
    validPieces(Board, Player, ValidPieces),
    random_member(X-Y-Piece, ValidPieces).

getRandomMove(Board, Player, X-Y-Piece, XF-YF) :-
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves).

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
    % Evaluate objectives
    getTargetPieces(Board, TargetPieces),
    getPlayerPoints(TargetPieces, Player, 0, Points),
    % Evaluate Pieces
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    findScaredPieces(Board, Player, ScaredPlayer),
    length(ScaredPlayer, NumScaredPlayer),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is -100 * Points + NumScaredPlayer - NumScaredOtherPlayer.

evaluateBoards([], _, []).
evaluateBoards([B|RestBoards], Player, [V-B|RT]) :-
    evaluate(B, Player, V),
    evaluateBoards(RestBoards, Player, RT).

getBestBoards([V-B|RestBoards], V, [B|T]) :-
    getBestBoards(RestBoards, V, T).
getBestBoards(_, _, []).

% =========================================================================
% FEAR MECHANIC
% =========================================================================

scared(P1, P2) :- lion(P1), elephant(P2).
scared(P1, P2) :- elephant(P1), mouse(P2).
scared(P1, P2) :- mouse(P1), lion(P2).

findScaredPieces(Board, Player, Pieces) :-
    findall(X-Y-P, findScaredPiecesAux(X, Y, Board, Player, P), Pieces).

findScaredPiecesAux(X, Y, Board, Player, P) :-
    validPiecesAux(X, Y, Board, Player, P),
    isScared(X, Y, Board, Player, P).

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

getPlayerPoints([], _, Acc, Acc).
getPlayerPoints([H|T], Player, Acc, Points) :-
    boolToInt(playerPiece(H, Player), Point),
    Acc1 is Acc + Point,
    getPlayerPoints(T, Player, Acc1, Points).

getTargetPieces(Board, [P1,P2,P3,P4]) :- 
    getPiece(4, 7,Board, P1),
    getPiece(7, 7,Board, P2),
    getPiece(4, 4,Board, P3),
    getPiece(7, 4,Board, P4).

% gameOver(+GameState, -Winner)
% TEMPORARY FUNCTION JUST USED ON TESTS
gameOver(Board, Winner) :-
    getTargetPieces(Board, Pieces),
    gameOverWinner(Pieces, Winner).

gameOver(Winner) :-
    board(Board),
    getTargetPieces(Board, Pieces),
    gameOverWinner(Pieces, Winner).

% gameOverWinner(P1, P2, P3, P4, Winner)
gameOverWinner(Pieces, 1) :-
    getPlayerPoints(Pieces, 0, 0, Points),
    Points > 2,
    !.
gameOverWinner(Pieces, 2) :-
    getPlayerPoints(Pieces, 1, 0, Points),
    Points > 2,
    !.
gameOverWinner(_,3).

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
    % To change
    write('============================'), nl,
    format('=        PLAYER ~d          =', [Winner]), nl,
    write('=          WINS!           ='), nl,
    write('============================'), nl.

% Test if player has scared pieces
% If yes, he needs to play them
turn(Board, Player) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    turn_action(Board, Player, ScaredPieces),
    !.
% Otherwise, play a normal turn
turn(Board, Player) :-
    validPieces(Board, Player, Pieces),
    turn_action(Board, Player, Pieces).

% If Player is Human
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 0),
    turn_human(Board, Player, PiecesToMove).

% If Player is Random
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 1),
    turn_random(Board, Player, PiecesToMove).

% If Player is Greedy
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 2),
    turn_greedy(Board, Player, PiecesToMove).

% If Player is Greedy (MinMax)
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 3),
    turn_greedy_minmax(Board, Player, PiecesToMove).

% Handle Human Turn
turn_human(Board, Player, PiecesToMove) :-
    % TODO FORCE THE PIECE TO BE IN PIECESTOMOVE
    readPositon(X, Y, Piece, Board, Player),
    visualize_moves(X, Y, Piece, Board, Player, Moves),
    readDestination(XF, YF, Moves, Player),
    movePiece(X, Y, XF, YF, Board, NewBoard),
    !,
    setBoard(NewBoard).

% Handle Random Turn
turn_random(Board, Player, PiecesToMove) :-
    random_member(X-Y-Piece, PiecesToMove),
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves),
    movePiece(X, Y, XF, YF, Board, NewBoard),
    !,
    setBoard(NewBoard).

% Handle Greedy Turn
turn_greedy(Board, Player, Pieces) :-
    generateBoards(Board, Player, Pieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    !,
    random_member(MoveChosen, BestBoards),
    setBoard(MoveChosen).

% MinMax BIG BRAIN Bot

turn_greedy_minmax(Board, Player, Pieces) :-
    nextLevelBoards([Board], Player, [], Lv1, [], _),
    nextLevelBoards(Lv1, Player, [], Lv2, [], Lv2Parent),
    evaluateBoards(Lv2, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    SortedBoards = [V-_|_],
    getBestBoards(SortedBoards, V, BestBoards),
    findPreviousBestBoards(BestBoards, Lv2, Lv2Parent, PBestBoards),
    !,
    random_member(MoveChosen, PBestBoards),
    setBoard(MoveChosen).

nextLevelBoards([], Player, Acc1, Acc1, Acc2, Acc2).
nextLevelBoards([CurrentBoard | Rest], Player, Acc1, Next, Acc2, Before) :-
    validPieces(CurrentBoard, Player, Pieces),
    generateBoards(CurrentBoard, Player, Pieces, [], NextBoards),
    !,
    length(NextBoards, N),
    appendToListNTimes(CurrentBoard, SameLevel, N),
    append(Acc1, NextBoards, Temp1),
    append(Acc2, SameLevel, Temp2),
    nextLevelBoards(Rest, Player, Temp1, Next, Temp2, Before).


appendToListNTimes(X, L, 0).
appendToListNTimes(X, L, N):-
    N1 is N - 1,
    appendToListNTimes(X, Temp, N1),
    append(Temp, [X], L).

findPreviousBestBoards([], _, _, []).
findPreviousBestBoards([B|T], LvlC, LvlP, [PB|PBestBoards]) :-
    findPreviousBestBoards(T, LvlC, LvlP, PBestBoards),
    nth1(Index, LvlC, B),
    nth1(Index, LvlP, PB).



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
    write('9. Greedy Bot vs Greedy Bot'), nl,
    write('10. Greedy (Hard) Bot vs Greedy Bot'), nl.

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
setGamemode(10) :- asserta(playerType(0, 3)), asserta(playerType(1, 2)).

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
    retractall(playerTurn(_)),
    asserta(playerTurn(1)),
    !. 
switchPlayer :-
    playerTurn(1),
    retractall(playerTurn(_)),
    asserta(playerTurn(0)),
    !.
switchPlayer :-
    asserta(playerTurn(0)).

setBoard(Board) :-
    retractall(board(_)),
    asserta(board(Board)).

startGame :-
    get_initial_board(InitialBoard),
    setBoard(InitialBoard),
    switchPlayer, % Will set player 0 turn
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