% =========================================================================
% CHANGE MAIN LOOP TO NOT DRAW BOARD
% =========================================================================

statsGame(Winner) :-
    get_initial_board(InitialBoard),
    setBoard(InitialBoard),
    switchPlayer,
    !,
    repeat,
    board(B),
    playerTurn(Player),
    turn(B, Player),
    switchPlayer,
    gameOver(Winner),
    Winner < 3,
    !.

% =========================================================================
% LOOP GAMES
% =========================================================================

loopGame(N, Wins1) :-
    loopGameAux(N, 0, Wins1).

loopGameAux(0, Acc, Acc).
loopGameAux(N, Acc, Wins1) :-
    write('playing game..'), nl,
    statsGame(Winner),
    Winner = 1,
    N1 is N - 1,
    A1 is Acc + 1,
    loopGameAux(N1, A1, Wins1).
loopGameAux(N, Acc, Wins1) :-
    N1 is N - 1,
    loopGameAux(N1, Acc, Wins1).

% =========================================================================
% PRINT STATS
% =========================================================================

getStats :-
    write('============================'), nl,
    write('=          STATS           ='), nl,
    write('============================'), nl,

    retractall(evaluationType(_)),
    asserta(evaluationType(1)),

    N = 10,

    retractall(playerType(_,_)),
    asserta(playerType(0, 2)), asserta(playerType(1, 1)),
    loopGame(N, P1),
    P2 is N - P1,

    write('Greedy Bot vs Random Bot'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P1), nl,
    write('Random Bot 2 wins: '), write(P2), nl,

    retractall(playerType(_,_)),
    asserta(playerType(0, 2)), asserta(playerType(1, 2)),
    loopGame(N, P3),
    P4 is N - P3,

    write('Greedy Bot vs Greedy Bot'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P3), nl,
    write('Greedy Bot 2 wins: '), write(P4), nl,

    retractall(playerType(_,_)),
    asserta(playerType(0, 3)), asserta(playerType(1, 2)),
    loopGame(N, P5),
    P6 is N - P5,

    write('MiniMax Bot vs Greedy Bot'), nl,
    write('Number of games: '), write(N), nl,
    write('MiniMax wins: '), write(P5), nl,
    write('Greedy  wins: '), write(P6), nl,
    
    retractall(playerType(_,_)),
    asserta(playerType(0, 3)), asserta(playerType(1, 3)),
    loopGame(N, P7),
    P8 is N - P7,

    write('MiniMax Bot vs MiniMax Bot'), nl,
    write('Number of games: '), write(N), nl,
    write('MiniMax 1 wins: '), write(P7), nl,
    write('MiniMax 2  wins: '), write(P8), nl.
