drawHeader :- 
    write('      A       B       C       D       E       F       G       H       I       J     '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

drawFooter :-
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    write('      A       B       C       D       E       F       G       H       I       J     '), nl.

drawNRow(1) :-
    write('  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . | '), nl,
    write('1 |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |1'), nl,
    write('  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . | '), nl.

drawNRow(N) :-
    0 =:= N mod 2,
    N < 10,
    write('  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . | '), nl,
    write(N), write(' |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |'), write(N), nl,
    write('  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . | '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

drawNRow(N) :-
    N < 10,
    write('  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       | '), nl,
    write(N), write(' | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |'), write(N), nl,
    write('  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       | '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

drawNRow(N) :-
    write('  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . | '), nl,
    write(N), write('|       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |'), write(N), nl,
    write('  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . | '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

% For now, empty hard-coded board.
drawBoard :-
    drawHeader,
    drawNRow(10), drawNRow(9), drawNRow(8), drawNRow(7), drawNRow(6), drawNRow(5), drawNRow(4), drawNRow(3), drawNRow(2), drawNRow(1), 
    drawFooter.