:- include('board.pl').
:- include('move.pl').
:- include('ia3.pl').
:- set_prolog_stack(global, limit(100 000 000 000)).
:- set_prolog_stack(trail,  limit(20 000 000 000)).
:- set_prolog_stack(local,  limit(2 000 000 000)).

menu(1) :- write('\n--- Placement des pieces ---\n'), initBoard(P), menuPlacer(P,1).
menu(2) :- write('\n--- Placement des pieces ---\n'), plateauJvIA(P), asserta(pieceCapture(1,0)), asserta(pieceCapture(2,0)), affichePlateau2(P,36), nl, menuPlacerIA(P,2).
menu(3) :- write('\n--- Plateau initial ---\n'), plateauIAvIA(P), asserta(pieceCapture(1,0)), asserta(pieceCapture(2,0)), affichePlateau2(P,36), nl, menuIAvsIA(P).
menu(4).

menu(5) :- write('\n--- Placement des pieces ---\n'), plateauBLOCAGE(P), asserta(pieceCapture(1,0)), asserta(pieceCapture(2,4)), affichePlateau2(P,36), nl, premierTourIA(P,1).

exec :- repeat, write('\n--- Menu Principal ---\n\n\t1. Joueur vs Joueur\n\t2. Joueur vs IA\n\t3. IA vs IA\n\t4. Quitter\n'), read(X), nl, menu(X).