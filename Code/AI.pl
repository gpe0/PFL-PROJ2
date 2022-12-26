% evaluatePiece(+Piece, +X, +Y, -Points)

% Targets
evaluatePiece(_, X, Y, 100) :- targetPosition(X, Y).

evaluatePiece(Piece, X, Y, Value) :-
    mouse(Piece),
    evaluateMouse(X, Y, Value).
evaluatePiece(Piece, X, Y, Value) :-
    lion(Piece),
    evaluateLion(X, Y, Value).
evaluatePiece(Piece, X, Y, Value) :-
    elephant(Piece),
    evaluateElephant(X, Y, Value).

% Elephants

% Red
evaluateElephant(_, 4, 8).
evaluateElephant(_, 7, 8).
evaluateElephant(4, _, 8).
evaluateElephant(7, _, 8).
evaluateElephant(X, X, 8).
evaluateElephant(X, Y, 8) :- Y = 11 - X.

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