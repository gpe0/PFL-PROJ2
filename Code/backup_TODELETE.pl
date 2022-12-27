% evaluatePiece(+Piece, +X, +Y, -Points)

% Targets
evaluatePiece(Piece, X, Y, _, 100) :- mouse(Piece), targetPosition(X, Y).
evaluatePiece(Piece, X, Y, _, 100) :- lion(Piece), targetPosition(X, Y).
evaluatePiece(Piece, X, Y, _, 100) :- elephant(Piece), targetPosition(X, Y).

evaluatePiece(Piece, X, Y, Goals, Value) :-
    mouse(Piece),
    evaluateMouse(X, Y, Goals, Value).
evaluatePiece(Piece, X, Y, Goals, Value) :-
    lion(Piece),
    evaluateLion(X, Y, Goals, Value).
evaluatePiece(Piece, X, Y, Goals, Value) :-
    elephant(Piece),
    evaluateElephant(X, Y, Goals, Value).

evaluatePiece(_, _, _, _, 0).

% Elephants

evaluateElephant(X, Y, [7, _, _, _], 7) :-
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateElephant(X, Y, [_, _, _, _], 4) :-
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateElephant(X, Y, [_, 7, _, _], 7) :-
    X > 5, X < 9,
    Y > 2, Y < 6.

evaluateElephant(X, Y, [_, _, _, _], 4) :-
    X > 5, X < 9,
    Y > 2, Y < 6.

evaluateElephant(X, Y, [_, _, 7, _], 7) :-
    X > 2, X < 6,
    Y > 5, Y < 9.

evaluateElephant(X, Y, [_, _, _, _], 4) :-
    X > 2, X < 6,
    Y > 5, Y < 9.

evaluateElephant(X, Y, [_, _, _, 7], 7) :-
    X > 5, X < 9,
    Y > 5, Y < 9.

evaluateElephant(X, Y, [_, _, _, _], 4) :-
    X > 5, X < 9,
    Y > 5, Y < 9.

evaluateElephant(_, _, _, 1).

% Mice

evaluateMouse(4, Y, [7, _, _, _], 7) :-
    Y > 2, Y < 6.

evaluateMouse(X, 4, [7, _, _, _], 7) :-
    X > 2, X < 6.

evaluateMouse(4, Y, [_, _, _, _], 4) :-
    Y > 2, Y < 6.

evaluateMouse(X, 4, [_, _, _, _], 4) :-
    X > 2, X < 6.

evaluateMouse(7, Y, [_, 7, _, _], 7) :-
    Y > 2, Y < 6.

evaluateMouse(X, 4, [_, 7, _, _], 7) :- 
    X > 5, X < 9.

evaluateMouse(7, Y, [_, _, _, _], 4) :-
    Y > 2, Y < 6.

evaluateMouse(X, 4, [_, _, _, _], 4) :- 
    X > 5, X < 9.

evaluateMouse(4, Y, [_, _, 7, _], 7) :-
    Y > 5, Y < 9.

evaluateMouse(X, 7, [_, _, 7, _], 7) :-
    X > 2, X < 6.

evaluateMouse(4, Y, [_, _, _, _], 4) :-
    Y > 5, Y < 9.

evaluateMouse(X, 7, [_, _, _, _], 4) :-
    X > 2, X < 6.

evaluateMouse(7, Y, [_, _, _, 7], 7) :-
    Y > 5, Y < 9.

evaluateMouse(X, 7, [_, _, _, 7], 7) :-
    X > 5, X < 9.

evaluateMouse(7, Y, [_, _, _, _], 4) :-
    Y > 5, Y < 9.

evaluateMouse(X, 7, [_, _, _, _], 4) :-
    X > 5, X < 9.


evaluateMouse(_, _, _, 1).

% Lions (NOT FINISHED)

evaluateLion(X, Y, [7, _, _, _], 7) :-
    Y = X + 4,
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateLion(X, Y, [7, _, _, _], 7) :-
    Y = 11 - X,
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateLion(X, Y, [_, _, _, _], 4) :-
    Y = X + 4,
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateLion(X, Y, [_, _, _, _], 4) :-
    Y = 11 - X,
    X > 2, X < 6,
    Y > 2, Y < 6.


evaluateLion(X, X, [_, 7, _, _], 7) :-
    X > 5, X < 9.

evaluateLion(X, Y, [_, 7, _, _], 7) :-
    Y = 14 - X,
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateLion(X, X, [_, _, _, _], 4) :-
    X > 5, X < 9.

evaluateLion(X, Y, [_, _, _, _], 4) :-
    Y = 14 - X,
    X > 2, X < 6,
    Y > 2, Y < 6.

evaluateLion(X, X, [_, _, 7, _], 7) :-
    X > 2, X < 6.
    
evaluateLion(X, Y, [_, _, 7, _], 7) :-
    Y = 8 - X,
    X > 2, X < 6,
    Y > 5, Y < 9.

evaluateLion(X, X, [_, _, _, _], 4) :-
    X > 2, X < 6.
    
evaluateLion(X, Y, [_, _, _, _], 4) :-
    Y = 8 - X,
    X > 2, X < 6,
    Y > 5, Y < 9.

evaluateLion(X, Y, [_, _, _, 7], 7) :-
    Y = 11 - X,
    X > 5, X < 9,
    Y > 5, Y < 9.

evaluateLion(X, Y, [_, _, _, 7], 7) :-
    Y = X - 3,
    X > 5, X < 9,
    Y > 5, Y < 9.

evaluateLion(X, Y, [_, _, _, _], 4) :-
    Y = 11 - X,
    X > 5, X < 9,
    Y > 5, Y < 9.

evaluateLion(X, Y, [_, _, _, _], 4) :-
    Y = X - 3,
    X > 5, X < 9,
    Y > 5, Y < 9.

evaluateLion(_, _, _, 1).


/*
% Red
evaluateElephant(_, 4, 8).
evaluateElephant(_, 7, 8).
evaluateElephant(4, _, 8).
evaluateElephant(7, _, 8).
evaluateElephant(X, X, 8).


% Yellow
evaluateElephant(2, 5, 6).
evaluateElephant(2, 6, 6).
evaluateElephant(3, 5, 6).
evaluateElephant(3, 6, 6).

evaluateElephant(8, 5, 6).
evaluateElephant(8, 6, 6).
evaluateElephant(9, 5, 6).
evaluateElephant(9, 6, 6).

evaluateElephant(5, 2, 6).
evaluateElephant(5, 3, 6).
evaluateElephant(6, 2, 6).
evaluateElephant(6, 3, 6).

evaluateElephant(5, 8, 6).
evaluateElephant(5, 9, 6).
evaluateElephant(6, 8, 6).
evaluateElephant(6, 9, 6).

% Green
evaluateElephant(1, 5, 4).
evaluateElephant(1, 6, 4).

evaluateElephant(2, 3, 4).
evaluateElephant(3, 2, 4).

evaluateElephant(5, 1, 4).
evaluateElephant(6, 1, 4).

evaluateElephant(8, 2, 4).
evaluateElephant(9, 3, 4).

evaluateElephant(10, 5, 4).
evaluateElephant(10, 6, 4).

evaluateElephant(9, 8, 4).
evaluateElephant(8, 9, 4).

evaluateElephant(5, 10, 4).
evaluateElephant(6, 10, 4).

evaluateElephant(3, 9, 4).
evaluateElephant(2, 8, 4).

% Blue

evaluateElephant(_, _, 2).

% Mouse

% Red
evaluateMouse(_, 4, 7).
evaluateMouse(_, 7, 7).
evaluateMouse(4, _, 7).
evaluateMouse(7, _, 7).

% Yellow
evaluateMouse(X, Y, 5) :- 
    X > 4, X < 7,
    Y > 4, Y < 7.

% Green

evaluateMouse(X, Y, 3) :- 
    X > 4, X < 7,
    Y > 0, Y < 4.

evaluateMouse(X, Y, 3) :- 
    X > 4, X < 7,
    Y > 7, Y < 11.

evaluateMouse(X, Y, 3) :- 
    X > 0, X < 4,
    Y > 4, Y < 7.

evaluateMouse(X, Y, 3) :- 
    X > 7, X < 11,
    Y > 4, Y < 7.

% Blue

evaluateMouse(_, _, 1).

% Lion

% Red

evaluateLion(X, X, 7).
evaluateLion(X, Y, 7) :- Y = 11 - X.
evaluateLion(X, Y, 7) :- Y = X - 3.
evaluateLion(X, Y, 7) :- Y = 8 - X.
evaluateLion(X, Y, 7) :- Y = X + 3.
evaluateLion(X, Y, 7) :- Y = 14 - X.

% Yellow

evaluateLion(4, 5, 5).
evaluateLion(4, 6, 5).
evaluateLion(5, 4, 5).
evaluateLion(6, 4, 5).
evaluateLion(7, 5, 5).
evaluateLion(7, 6, 5).
evaluateLion(5, 7, 5).
evaluateLion(6, 7, 5).

% Green

evaluateLion(3, 2, 3).
evaluateLion(4, 2, 3). 
evaluateLion(4, 3, 3). 
evaluateLion(2, 3, 3). 
evaluateLion(2, 4, 3). 
evaluateLion(3, 4, 3). 
evaluateLion(7, 2, 3). 
evaluateLion(7, 3, 3). 
evaluateLion(8, 2, 3). 
evaluateLion(9, 3, 3). 
evaluateLion(9, 4, 3). 
evaluateLion(8, 4, 3). 
evaluateLion(2, 7, 3). 
evaluateLion(3, 7, 3). 
evaluateLion(2, 8, 3). 
evaluateLion(3, 9, 3). 
evaluateLion(4, 9, 3). 
evaluateLion(4, 8, 3). 
evaluateLion(8, 7, 3). 
evaluateLion(9, 7, 3). 
evaluateLion(9, 8, 3). 
evaluateLion(7, 8, 3). 
evaluateLion(7, 9, 3). 
evaluateLion(8, 9, 3). 

% Blue
evaluateLion(_, _, 1).

*/


getGoalsPieces(Board, [G1, G2, G3, G4]) :-
    getPiece(4, 4, Board, G1),
    getPiece(4, 7, Board, G2),
    getPiece(7, 4, Board, G3),
    getPiece(7, 7, Board, G4).


evaluateBigBrain(Board, Player, Value) :-
    getGoalsPieces(Board, Goals),
    getBoardPoints(Board, 1, Player, Goals, Points),
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is Points * -1 - NumScaredOtherPlayer.

getBoardPoints([], _, _, _, 0).
getBoardPoints([Xs|Rest], X, Player, Goals, Points) :-
    getRowPoints(Xs, X, 1, Player, Goals, XPoints),
    X1 is X + 1,
    getBoardPoints(Rest, X1, Player, Goals, RestPoints),
    Points is XPoints + RestPoints.

getRowPoints([], _, _, _, _, 0).
getRowPoints([Piece|Rest], X, Y, 0, Goals, Points) :-
    Y1 is Y + 1,
    Piece < 4,
    evaluatePiece(Piece, X, Y, Goals, Val),
    getRowPoints(Rest, X, Y1, 0, Goals, RestPoints),
    Points is Val + RestPoints.
getRowPoints([Piece|Rest], X, Y, 1, Goals, Points) :-
    Y1 is Y + 1,
    Piece > 3,
    evaluatePiece(Piece, X, Y, Goals, Val),
    getRowPoints(Rest, X, Y1, 1, Goals, RestPoints),
    Points is Val + RestPoints.

getRowPoints([_|Rest], X, Y, Player, Goals, Points) :-
    Y1 is Y + 1,
    getRowPoints(Rest, X, Y1, Player, Goals, Points).

test111(X) :-
    get_initial_board(B),
    evaluateBigBrain(B, 0, X).