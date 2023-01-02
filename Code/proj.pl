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
expand_acc(Width1, _, _, _, _, _, _, Acc, Acc):- boardWidth(Width), Width1 is Width + 1.
expand_acc(_, 0, _, _, _, _, _, Acc, Acc).
expand_acc(_, Height1, _, _, _, _, _, Acc, Acc):- boardHeight(Height), Height1 is Height + 1.
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
    % Evaluate Other Player Pieces
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
    boardWidth(Width),
    boardHeight(Height),
    X1 is div(Width, 2) - 1,
    Y1 is div(Height, 2) - 1,
    X2 is X1 + 3,
    Y2 is Y1 + 3,
    getPiece(X1, Y1, Board, P1),
    getPiece(X1, Y2, Board, P2),
    getPiece(X2, Y1, Board, P3),
    getPiece(X2, Y2, Board, P4).

gameOver(99) :-
    retract(num_turn(100)), !.

gameOver(Winner) :-
    retract(num_turn(N)),
    N1 is N + 1,
    asserta(num_turn(N1)),
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
    boardHeight(Height),
    Y is Height + 1 - YInput.

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
    Option > 0,
    Option < 5.
parsePlayerType(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

parseEvaluationType(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 3.
parseEvaluationType(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% =========================================================================
% PLAY
% =========================================================================

:- dynamic playerType/2.
:- dynamic board/1.
:- dynamic playerTurn/1.
:- dynamic evaluationType/2.
:- dynamic num_turn/1.
:- dynamic targetPosition/2.

/**
 * Player Type
 * - 0 => Human
 * - 1 => Random Bot
 * - 2 => Greedy Bot
 * - 3 => MinMax Bot
 */

displayWinnerMessage(99) :-
    board(FinalBoard),
    display_game(FinalBoard),
    write('============================'), nl,
    write('=          DRAW!           ='), nl,
    write('============================'), nl.

displayWinnerMessage(Winner) :-
    board(FinalBoard),
    display_game(FinalBoard),
    write('============================'), nl,
    format('=        PLAYER ~d          =', [Winner]), nl,
    write('=          WINS!           ='), nl,
    write('============================'), nl.

piece_name(P, 'Elephant') :- elephant(P).
piece_name(P, 'Lion') :- lion(P).
piece_name(P, 'Mouse') :- mouse(P).

log_pieces([]).
log_pieces([X-Y-P|T]) :-
    piece_name(P, Name),
    boardHeight(Height),
    Y1 is Height + 1 - Y,
    Col is 64 + X,
    write('Position '),
    put_code(Col),
    format('~d : ~w', [Y1, Name]), nl,
    log_pieces_to_move(T).

warn_player(Player, Pieces) :-
    playerType(Player, 0), % Only if is human
    write('Warning: you have the following pieces scared'), nl,
    log_pieces(Pieces).
warn_player(_, _).


% Test if player has scared pieces
% If yes, he needs to play them
turn(Board, Player) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    warn_player(Player, ScaredPieces),
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
    write('1. Human'), nl,
    write('2. Random'), nl,
    write('3. Greedy'), nl,
    write('4. MinMax'), nl.

displayEvaluationTypes(Player) :-
    Aux is Player + 1,
    format('       PLAYER ~d EVALUATION TYPE        ', [Aux]), nl,
    write('1. Simple'), nl,
    write('2. Complex'), nl.

getPlayerType(Player) :-
    getBuffer(Input),
    parsePlayerType(Input, Option),
    Option1 is Option - 1,
    retractall(playerType(Player, _)),
    asserta(playerType(Player, Option1)).

readPlayerType(Player) :-
    repeat,
    displayPlayerTypes(Player),
    getPlayerType(Player).

getEvaluationType(Player) :-
    boardHeight(10),
    boardWidth(10),
    getBuffer(Input),
    parseEvaluationType(Input, Option),
    Option1 is Option - 1,
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, Option1)).

readEvaluationType(Player) :-
    boardPreference(2),
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, 0)).

readEvaluationType(Player) :-
    playerType(Player, Type),
    Type > 1,
    repeat,
    displayEvaluationTypes(Player),
    getEvaluationType(Player).

readEvaluationType(_).

even(N):- mod(N,2) =:= 0.

%BoardSetup
setupBoard :-
    readBoardPreference,
    setupCustomBoard,
    readTargetPreference,
    setupTargetPositions.

readTargetPreference :-
    boardPreference(1),
    retractall(targetDistance(_)),
    asserta(targetDistance(1)).

readTargetPreference :-
    boardPreference(2),
    repeat,
    displayTargetPreferences,
    getTargetPreference.

displayTargetPreferences :-
    write('       TARGET POSITION PREFERENCE        '), nl,
    write('1. Default'), nl,
    write('2. Custom'), nl.

getTargetPreference :-
    getBuffer(Input),
    parseTargetPreference(Input, Option),
    retractall(targetDistance(_)),
    asserta(targetDistance(Option)).

parseTargetPreference(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 3.
parseTargetPreference(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

setupTargetPositions :-
    boardPreference(1),
    fillBoard.

setupTargetPositions :-
    targetDistance(1),
    fillBoard.

setupTargetPositions :-
    getTargetDistanceFromDimensions(Distance),
    readTargetDistance(Distance),
    fillBoard.

fillBoard :-
    board(Board),
    boardWidth(Width),
    boardHeight(Height),
    targetDistance(Distance),    
    setTargetPositionLoop(Distance, Height, Width, Board, NewBoard),
    setDefaultPieces(NewBoard, NewerBoard),
    setBoard(NewerBoard).

setTargetPositionLoop(Distance, Height, Width, Board, NewBoard) :-
    X is div(Width, 2) - Distance,
    Y is div(Height, 2) - Distance,
    retractall(targetPosition(_,_)),
    setTargetPosition(X,Y, Board, TempBoard1),
    X1 is X + 1 + 2 * Distance,
    setTargetPosition(X1,Y, TempBoard1, TempBoard2),
    Y1 is Y + 1 + 2 * Distance,
    setTargetPosition(X,Y1, TempBoard2, TempBoard3),
    setTargetPosition(X1,Y1, TempBoard3, NewBoard).

setTargetPosition(X, Y, Board, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestRow),
    nth1(X, ModifiedRow, 7, RestRow),
    nth1(Y, NewBoard, ModifiedRow, RestBoard),
    asserta(targetPosition(X, Y)).

getTargetDistanceFromDimensions(Distance) :-
    boardHeight(Height),
    boardWidth(Width),
    Width < Height,
    Distance is div(Width, 2) - 3.

getTargetDistanceFromDimensions(Distance) :-
    boardHeight(Height),
    Distance is div(Height, 2) - 3.

readTargetDistance(Distance) :-
    repeat,
    displayTargetDistances(Distance),
    getTargetDistance(Distance).

displayTargetDistances(Distance) :-
    write('       TARGET POSITIONS        '), nl,
    write('Please choose the distance from the target positions to the center (diagonally)'), nl,
    write('Note that the distance must be between 1 and '),
    write(Distance), nl.

getTargetDistance(Distance) :-
    getBuffer(Input),
    parseTargetDistance(Input, Option, Distance),
    retractall(targetDistance(_)),
    asserta(targetDistance(Option)).

parseTargetDistance(Input, Option, Distance) :-
    readNumber(Input, Option),
    Option > 0,
    Option < Distance + 1.

parseTargetDistance(_, _, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

%Dimensions
readBoardPreference :-
    repeat,
    displayBoardPreferences,
    getBoardPreference.

displayBoardPreferences :-
    write('       BOARD PREFERENCE        '), nl,
    write('1. Default'), nl,
    write('2. Custom'), nl,
    write('Note that selecting this option will prevent you from using complex evaluation'), nl.

getBoardPreference :-
    getBuffer(Input),
    parseBoardPreference(Input, Option),
    retractall(boardPreference(_)),
    asserta(boardPreference(Option)).

parseBoardPreference(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 3.
parseBoardPreference(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

setupCustomBoard :-
    boardPreference(1),
    retractall(boardHeight(_)),
    asserta(boardHeight(10)),
    retractall(boardWidth(_)),
    asserta(boardWidth(10)),
    setupEmptyBoard.

setupCustomBoard :-
    boardPreference(2),
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, 0)),
    readBoardDimensions,
    setupEmptyBoard.

setupEmptyBoard :-
    boardWidth(Width),
    boardHeight(Height),
    length(Row, Width),
    maplist(=(0), Row),
    length(Board, Height),
    maplist(=(Row), Board),
    setBoard(Board).

setDefaultPieces(Board, NewBoard) :-
    boardHeight(Height),
    boardWidth(Width),
    X2 is div(Width, 2),
    X1 is X2 - 1,
    X3 is X2 + 1,
    X4 is X3 + 1,
    Y1 is Height - 1,
    setPiece(X2, Height, Board, 4, TempBoard1),
    setPiece(X3, Height, TempBoard1, 4, TempBoard2),
    setPiece(X1, Y1, TempBoard2, 6, TempBoard3),
    setPiece(X2, Y1, TempBoard3, 5, TempBoard4),
    setPiece(X3, Y1, TempBoard4, 5, TempBoard5),
    setPiece(X4, Y1, TempBoard5, 6, TempBoard6),
    setPiece(X2, 1, TempBoard6, 1, TempBoard7),
    setPiece(X3, 1, TempBoard7, 1, TempBoard8),
    setPiece(X1, 2, TempBoard8, 3, TempBoard9),
    setPiece(X2, 2, TempBoard9, 2, TempBoard10),
    setPiece(X3, 2, TempBoard10, 2, TempBoard11),
    setPiece(X4, 2, TempBoard11, 3, NewBoard).

readBoardDimensions :-
    readBoardWidth,
    readBoardHeight.

readBoardWidth :-
    repeat,
    displayBoardWidth,
    getBoardWidth.

readBoardHeight :-
    repeat,
    displayBoardHeight,
    getBoardHeight.

displayBoardWidth :-
    write('       BOARD DIMENSIONS        '), nl,
    write('Please choose a width for your board'), nl,
    write('Note that the width must be even between 8 and 26'), nl.

displayBoardHeight :-
    write('Please choose a height for your board'), nl,
    write('Note that the height must be even between 8 and 26'), nl.

getBoardWidth :-
    getBuffer(Input),
    parseBoardDimensions(Input, Option),
    retractall(boardWidth(_)),
    asserta(boardWidth(Option)).

getBoardHeight :-
    getBuffer(Input),
    parseBoardDimensions(Input, Option),
    retractall(boardHeight(_)),
    asserta(boardHeight(Option)).

parseBoardDimensions(Input, Option) :-
    readNumber(Input, Option),
    even(Option),
    Option > 7,
    Option < 27.
parseBoardDimensions(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

menu :-
    displayInitalMessage,
    setupBoard,
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
    switchPlayer, % Will set player 0 turn
    repeat,
    board(B),
    display_game(B),
    playerTurn(Player),
    removeLastPosition(B, Board),
    turn(Board, Player),
    switchPlayer,
    gameOver(Winner),
    isGameOver(Winner),
    displayWinnerMessage(Winner).

isGameOver(1).
isGameOver(2).
isGameOver(99).

play :-
    menu,
    retractall(playerTurn(_)),
    retractall(num_turn(_)),
    asserta(num_turn(0)),
    startGame.