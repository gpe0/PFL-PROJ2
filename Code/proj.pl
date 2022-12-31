:- use_module(library(lists)).
:- use_module(library(random)).

:- ensure_loaded('boards.pl').
:- ensure_loaded('view.pl').
:- ensure_loaded('tests.pl').
:- ensure_loaded('stats.pl').
:- ensure_loaded('AI.pl').

% =========================================================================
% BASE FACTS
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
empty(9).

% playerPiece(Piece, Player)
playerPiece(1, 0).
playerPiece(2, 0).
playerPiece(3, 0).
playerPiece(4, 1).
playerPiece(5, 1).
playerPiece(6, 1).

% targetPosition(X, Y)
targetPosition(4, 4).
targetPosition(4, 7).
targetPosition(7, 4).
targetPosition(7, 7).

% =========================================================================
% BOARD
% =========================================================================

backtrackGetPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P).

getPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P), !.
getPiece(_, _, _, 99). % Fail prevention (AI)
 
setPiece(X, Y, Board, P, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestY),
    nth1(X, ModifiedY, P, RestY),
    nth1(Y, NewBoard, ModifiedY, RestBoard).

% =========================================================================
% MOVEMENT
% =========================================================================

/*
flatten([], []).
flatten([XI-YI-Moves|T2], Flat) :-
    flattenAux(XI-YI, Moves, Flat2),
    flatten(T2, Flat3),
    append(Flat2, Flat3, Flat).

flattenAux(_, [], []).
flattenAux(XI-YI, [XF-YF|T], [XI-YI-XF-YF|T2]) :-
    flattenAux(XI-YI, T, T2).

valid_moves(Board, Player, Moves) :-
    findall(X-Y-M, (backtrackGetPiece(X, Y, Board, Piece), playerPiece(Piece, Player), getMoves(X, Y, Piece, Board, Player, M)), MovesAux),
    flatten(MovesAux, Moves).

Test predicate on SICStus:

|: get_initial_board(B), valid_moves(B, 0, Moves).
|: get_initial_board(B), valid_moves(B, 0, Moves), nth1(1, Moves, M), move(B, M, NB), display_game(NB).
*/

move(Board, X-Y-XF-YF, NewBoard) :-
    getPiece(X, Y, Board, Piece),
    setPiece(X, Y, Board, 9, TempBoard),
    setPiece(XF, YF, TempBoard, Piece, NewBoard).    

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
    append(Moves1, T, Moves), T = Moves2,
    !.

expand_up_down(X, Y, Board, Player, P, Moves) :-
    expand(X, Y, -1, 0, Board, Player, P, Left),
    expand(X, Y, 1, 0, Board, Player, P, Right),
    expand(X, Y, 0, 1, Board, Player, P, Top),
    expand(X, Y, 0, -1, Board, Player, P, Down),
    append(Left, Right, Aux1),
    append(Aux1, Top, Aux2),
    append(Aux2, Down, Moves).

expand_diagonal(X, Y, Board, Player, P, Moves) :-
    expand(X, Y, -1, -1, Board, Player, P, DL),
    expand(X, Y, 1, 1, Board, Player, P, TR),
    expand(X, Y, -1, 1, Board, Player, P, TL),
    expand(X, Y, 1, -1, Board, Player, P, DR),
    append(DL, TR, Aux1),
    append(Aux1, TL, Aux2),
    append(Aux2, DR, Moves).

expand(X, Y, StepX, StepY, Board, Player, Piece, Moves) :-
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, [], Moves).

% expand_acc(X, Y, StepX, StepY, Acc, Result)
expand_acc(0, _, _, _, _, _, _, Acc, Acc).
expand_acc(11, _, _, _, _, _, _, Acc, Acc).
expand_acc(_, 0, _, _, _, _, _, Acc, Acc).
expand_acc(_, 11, _, _, _, _, _, Acc, Acc).
expand_acc(X, Y, _, _, Board, _, _, Acc, Acc) :-
    getPiece(X, Y, Board, Piece),
    \+empty(Piece).
expand_acc(X, Y, StepX, StepY, Board, Player, Piece, Acc, Result) :-
    isScared(X, Y, Board, Player, Piece),
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, Acc, Result).
expand_acc(X, Y, StepX, StepY, Board, Player, Piece, Acc, Result) :-
    A1 = [X-Y|Acc],
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, A1, Result).
    
visualize_moves(X, Y, Piece, Board, Player, Moves) :-
    getMoves(X, Y, Piece, Board, Player, Moves),
    drawMoves(Board, Moves, NewBoard),
    display_game(NewBoard).

drawMoves(Board, [], Board).
drawMoves(Board, [X-Y|T], NewBoard) :-
    setPiece(X,Y,Board,8,B1),
    drawMoves(B1,T,NewBoard).

% =========================================================================
% AI RANDOM BOT
% =========================================================================

validPieces(Board, Player, Pieces) :-
    findall(X-Y-P, validPiecesAux(X, Y, Board, Player, P), Pieces).

validPiecesAux(X, Y, Board, Player, P) :-
    backtrackGetPiece(X, Y, Board, P),
    playerPiece(P, Player),
    getMoves(X, Y, P, Board, Player, [_|_]).

% =========================================================================
% AI BIG BRAIN BOT
% =========================================================================

generateBoards(_, _, [], Acc, Acc).
generateBoards(Board, Player, [X-Y-Piece|RestPieces], Acc, Boards) :-
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    generateBoardsAux(Board, X-Y-Piece, ValidMoves, [], NewBoards),
    append(Acc, T1, Aux), T1 = NewBoards,
    generateBoards(Board, Player, RestPieces, Aux, Boards).

generateBoardsAux(_, _, [], Acc, Acc).
generateBoardsAux(Board, X-Y-Piece, [XF-YF|RestMoves], Acc, Boards) :-
    move(Board, X-Y-XF-YF, NewBoard),
    generateBoardsAux(Board, X-Y-Piece, RestMoves, [NewBoard | Acc], Boards).

value_simple(Board, Player, Value) :-
    % Evaluate objectives
    getTargetPieces(Board, TargetPieces),
    getPlayerPoints(TargetPieces, Player, 0, Points),
    % Evaluate Pieces
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    numScaredOnTarget(ScaredOtherPlayer, 0, ScaredOnTarget),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is -100 * Points - 50 * ScaredOnTarget - NumScaredOtherPlayer.

evaluateBoards([], _, []).
evaluateBoards([B|RestBoards], Player, [V-B|RT]) :-
    evaluationType(Player, 0),
    value_simple(B, Player, V),
    evaluateBoards(RestBoards, Player, RT).
evaluateBoards([B|RestBoards], Player, [V-B|RT]) :-
    evaluationType(Player, 1),
    value(B, Player, V),
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

boolToInt(Pred, 1) :- call(Pred).
boolToInt(_, 0).

getPlayerPoints([], _, Acc, Acc).
getPlayerPoints([H|T], Player, Acc, Points) :-
    boolToInt(playerPiece(H, Player), Point),
    Acc1 is Acc + Point,
    getPlayerPoints(T, Player, Acc1, Points).

getTargetPieces(Board, [P1,P2,P3,P4]) :- 
    getPiece(4, 4, Board, P1),
    getPiece(7, 4, Board, P2),
    getPiece(4, 7, Board, P3),
    getPiece(7, 7, Board, P4).

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

getBuffer([]) :- peek_code(10), get_code(10), !.
getBuffer([H|T]) :-
    get_code(H),
    getBuffer(T).

readNumber(L, Res) :- readNumberAux(L, 0, Res).

readNumberAux([], Acc, Acc).
readNumberAux([C|T], Acc, Res):- 
    C >= 48,
    C =< 57,
    !,
    Acc1 is 10*Acc + (C - 48),
    readNumberAux(T, Acc1, Res).
readNumberAux(_, Acc, Acc).

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
    getBuffer([Letter|Number]),
    readNumber(Number, YInput),
    letterToIndex(Letter, X),
    Y is 11 - YInput.
getInput(Mode, X, Y) :-
    write('error: Invalid input, try again!'), nl, nl,
    getInput(Mode, X, Y).

readPosition(X, Y, Piece, Board, Player, PossiblePieces) :-
    getInput(Player, X, Y),
    getPiece(X, Y, Board, Piece),
    playerPiece(Piece, Player),
    member(X-Y-Piece, PossiblePieces).

readDestination(X, Y, Moves, Player) :-
    Mode is Player + 2,
    getInput(Mode, X, Y),
    member(X-Y, Moves).

parsePlayerType(Input, Option) :-
    readNumber(Input, Option),
    Option > -1,
    Option < 4.
parsePlayerType(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

parseEvaluationType(Input, Option) :-
    readNumber(Input, Option),
    Option > -1,
    Option < 2.
parseEvaluationType(_, _) :-
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
 * - 3 => MinMax Bot
 */

displayWinnerMessage(Winner) :-
    board(FinalBoard),
    display_game(FinalBoard),
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
    choose_move(Board, Player, 1, PiecesToMove).

% If Player is Greedy
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 2),
    choose_move(Board, Player, 2, PiecesToMove).

% If Player is Greedy (MinMax)
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 3),
    choose_move(Board, Player, 3, PiecesToMove).

% Handle Human Turn
turn_human(Board, Player, PiecesToMove) :-
    readPosition(X, Y, Piece, Board, Player, PiecesToMove),
    visualize_moves(X, Y, Piece, Board, Player, Moves),
    readDestination(XF, YF, Moves, Player),
    move(Board, X-Y-XF-YF, NewBoard),
    !,
    setBoard(NewBoard).

% Handle Random Turn
choose_move(Board, Player, 1, PiecesToMove) :-
    random_member(X-Y-Piece, PiecesToMove),
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves),
    move(Board, X-Y-XF-YF, NewBoard),
    !,
    setBoard(NewBoard).

% Handle Greedy Turn
choose_move(Board, Player, 2, PiecesToMove) :-
    generateBoards(Board, Player, PiecesToMove, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    !,
    random_member(MoveChosen, BestBoards),
    setBoard(MoveChosen).

% Handle MinMax Turn
choose_move(Board, Player, 3, PiecesToMove) :-
    retractall(moveBoard(_, _)),
    asserta(moveBoard(-100000, [])),
    maxValue(Board, Player, 0, -100000, 100000, PiecesToMove, _),
    moveBoard(_, New),
    setBoard(New),
    retractall(moveBoard(_, _)), !.

displayInitalMessage :-
    write('===== Barca Board Game ====='), nl,
    write('          PFL 22/23         '), nl,
    write('============================'), nl, nl.

displayPlayerTypes(Player) :-
    Aux is Player + 1,
    format('       PLAYER ~d TYPE        ', [Aux]), nl,
    write('0. Human'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. MinMax'), nl.

displayEvaluationTypes(Player) :-
    Aux is Player + 1,
    format('       PLAYER ~d EVALUATION TYPE        ', [Aux]), nl,
    write('0. Simple'), nl,
    write('1. Complex'), nl.

getPlayerType(Player) :-
    getBuffer(Input),
    parsePlayerType(Input, Option),
    retractall(playerType(Player, _)),
    asserta(playerType(Player, Option)).

readPlayerType(Player) :-
    repeat,
    displayPlayerTypes(Player),
    getPlayerType(Player).

getEvaluationType(Player) :-
    getBuffer(Input),
    parseEvaluationType(Input, Option),
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, Option)).

readEvaluationType(Player) :-
    playerType(Player, Type),
    Type \= 0,
    repeat,
    displayEvaluationTypes(Player),
    getEvaluationType(Player).

readEvaluationType(_).

menu :-
    displayInitalMessage,
    readPlayerType(0),
    readEvaluationType(0),
    readPlayerType(1),
    readEvaluationType(1).

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

removeLastPosition(Board, Res) :-
    backtrackGetPiece(X, Y, Board, 9),
    !,
    resetPosition(X, Y, Board, Res).
removeLastPosition(Board, Board).

resetPosition(X, Y, Board, Res) :-
    targetPosition(X, Y),
    setPiece(X, Y, Board, 7, Res),
    !.
resetPosition(X, Y, Board, Res) :-
    setPiece(X, Y, Board, 0, Res).

startGame :-
    initial_state(InitialBoard),
    setBoard(InitialBoard),
    switchPlayer, % Will set player 0 turn
    !,
    repeat,
    board(B),
    display_game(B),
    playerTurn(Player),
    removeLastPosition(B, Board),
    turn(Board, Player),
    switchPlayer,
    gameOver(Winner),
    Winner < 3,
    displayWinnerMessage(Winner).

play :-
    menu,
    startGame.