% plateau(+Liste)
% Prédicat définissant l'état du plateau, pattern de la liste : [[X,Y,ValKhan,Piece,Joueur,Khan]|Q]
plateau([
	[1,1,2,'O',0,0],[2,1,2,'O',0,0],[3,1,2,'O',0,0],[4,1,2,'O',0,0],[5,1,2,'O',0,0],[6,1,2,'O',0,0],
	[1,2,2,'O',0,0],[2,2,1,'O',0,0],[3,2,2,'O',0,0],[4,2,1,'O',0,0],[5,2,3,'O',0,0],[6,2,2,'O',0,0],
	[1,3,2,'O',0,0],[2,3,3,'O',0,0],[3,3,1,'O',0,0],[4,3,2,'O',0,0],[5,3,1,'O',0,0],[6,3,3,'O',0,0],
	[1,4,2,'O',0,0],[2,4,1,'O',0,0],[3,4,3,'O',0,0],[4,4,1,'O',0,0],[5,4,3,'O',0,0],[6,4,1,'O',0,0],
	[1,5,1,'O',0,0],[2,5,2,'O',0,0],[3,5,1,'O',0,0],[4,5,3,'O',0,0],[5,5,1,'O',0,0],[6,5,2,'O',0,0],
	[1,6,3,'O',0,0],[2,6,2,'O',0,0],[3,6,2,'O',0,0],[4,6,1,'O',0,0],[5,6,3,'O',0,0],[6,6,2,'O',0,0]
]).

% affichePlateau(+Plateau,+N)
% Affiche le Plateau (liste) de N éléments, version pour GNU pl
% affichePlateau([],0).
% affichePlateau([[_,_,V,X,J,K]|Q],N) :- 0 is N mod 6, M is N-1, nl, write('('), write(V), write(','), write(X), write(','), write(J), write(') '), affichePlateau(Q,M).
% affichePlateau([[_,_,V,X,J,K]|Q],N) :- \+ 0 is N mod 6, M is N-1, write('('), write(V), write(','), write(X), write(','), write(J), write(') '),affichePlateau(Q,M).

% affichePlateau2(+Plateau,+N)
% Affiche le Plateau (liste) de N éléments, version pour SWI pl
affichePlateau2([],0).
affichePlateau2([[_,_,V,X,0,0]|Q],N) :- 0 is N mod 6, M is N-1, nl, ansi_format([bg(white)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,0,1]|Q],N) :- 0 is N mod 6, M is N-1, nl, ansi_format([bold,bg(white)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,0,0]|Q],N) :- \+ 0 is N mod 6, M is N-1, ansi_format([bg(white)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,0,1]|Q],N) :- \+ 0 is N mod 6, M is N-1, ansi_format([bold,bg(white)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,1,0]|Q],N) :- 0 is N mod 6, M is N-1, nl, ansi_format([bg(red)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,1,1]|Q],N) :- 0 is N mod 6, M is N-1, nl, ansi_format([bold,bg(red)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,1,0]|Q],N) :- \+ 0 is N mod 6, M is N-1, ansi_format([bg(red)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,1,1]|Q],N) :- \+ 0 is N mod 6, M is N-1, ansi_format([bold,bg(red)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,2,0]|Q],N) :- 0 is N mod 6, M is N-1, nl, ansi_format([bg(yellow)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,2,1]|Q],N) :- 0 is N mod 6, M is N-1, nl, ansi_format([bold,bg(yellow)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,2,0]|Q],N) :- \+ 0 is N mod 6, M is N-1, ansi_format([bg(yellow)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).
affichePlateau2([[_,_,V,X,2,1]|Q],N) :- \+ 0 is N mod 6, M is N-1, ansi_format([bold,bg(yellow)], ' ~w ~w ', [V, X]), affichePlateau2(Q,M).

%ansi_format([bold,bg(red)], ' ~d ~d ~d ', [V, X, J]),

% initBoard(-Plateau)
% Affiche et renvoie la liste Plateau 
initBoard(P) :- plateau(P), affichePlateau2(P,36).

% placerPiece(+Plateau, -NouveauPlateau, +CoordX, +CoordY, +Piece, +J)
% Place la Piece du joueur J aux coordonnees X,Y dans NouveauPlateau
placerPiece([],[],_,_,_,_).
placerPiece([[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1],X,Y,Piece,J) :- U \= X, V \= Y, placerPiece(Q,Q1,X,Y,Piece,J).
placerPiece([[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1],X,Y,Piece,J) :- U \= X, placerPiece(Q,Q1,X,Y,Piece,J).
placerPiece([[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1],X,Y,Piece,J) :- V \= Y, placerPiece(Q,Q1,X,Y,Piece,J).
%placerPiece([[X,Y,Val,_,_,K]|Q],[[X,Y,Val,'S',J,K]|Q1],X,Y,' ',J) :- placerPiece(Q,Q1,X,Y,' ',J).
placerPiece([[X,Y,Val,_,_,K]|Q],[[X,Y,Val,Piece,J,K]|Q1],X,Y,Piece,J) :- placerPiece(Q,Q1,X,Y,Piece,J).

% placerKhan(+Case, +Plateau, -NouveauPlateau)
% Place le Khan sur la Case en entrée et renvoie la liste NouveauPlateau
placerKhan(_,[],[]).
placerKhan((X,Y),[[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1]) :- U \= X, V \= Y, placerKhan((X,Y),Q,Q1).
placerKhan((X,Y),[[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1]) :- U \= X, placerKhan((X,Y),Q,Q1).
placerKhan((X,Y),[[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1]) :- V \= Y, placerKhan((X,Y),Q,Q1).
placerKhan((X,Y),[[X,Y,Val,Piece,J,_]|Q],[[X,Y,Val,Piece,J,1]|Q1]) :- placerKhan((X,Y),Q,Q1).

% retirerKhan(+Case, +Plateau, -NouveauPlateau)
% Retire le Khan de la Case en entrée et renvoie la liste NouveauPlateau
retirerKhan(_,[],[]).
retirerKhan((X,Y),[[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1]) :- U \= X, V \= Y, retirerKhan((X,Y),Q,Q1).
retirerKhan((X,Y),[[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1]) :- U \= X, retirerKhan((X,Y),Q,Q1).
retirerKhan((X,Y),[[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1]) :- V \= Y, retirerKhan((X,Y),Q,Q1).
retirerKhan((X,Y),[[X,Y,Val,Piece,J,_]|Q],[[X,Y,Val,Piece,J,0]|Q1]) :- retirerKhan((X,Y),Q,Q1).

% effacerCase(+Plateau, -NouveauPlateau, +CoordX, +CoordY)
% Remet à zéro l'etat d'une case sur le plateau
effacerCase([],[],_,_).
effacerCase([[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1],X,Y) :- U \= X, V \= Y, effacerCase(Q,Q1,X,Y).
effacerCase([[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1],X,Y) :- U \= X, effacerCase(Q,Q1,X,Y).
effacerCase([[U,V,Val,O,I,K]|Q],[[U,V,Val,O,I,K]|Q1],X,Y) :- V \= Y, effacerCase(Q,Q1,X,Y).
effacerCase([[X,Y,Val,_,_,_]|Q],[[X,Y,Val,' ',0,0]|Q1],X,Y) :- effacerCase(Q,Q1,X,Y).

% choixCoords(+Plateau, +Joueur, +Piece +Limite, -Liste)
% Affiche les choix possibles pour placer les pieces, on restreint le placement des joueurs
choixCoords([],_,_,_,[]).
choixCoords([[X,Y,_,' ',_,_]|Q1],1,Piece,Lim,[[[Piece,(X,Y)]|_]|Q2]) :- Y < Lim,choixCoords(Q1,1,Piece,Lim,Q2).
choixCoords([[X,Y,_,' ',_,_]|Q1],2,Piece,Lim,[[[Piece,(X,Y)]|_]|Q2]) :- Y > Lim,choixCoords(Q1,2,Piece,Lim,Q2).
choixCoords([[_,_,_,_,_,_]|Q1],J,Piece,Lim,Q2) :- choixCoords(Q1,J,Piece,Lim,Q2).

% placerCoord(+NumPiece, +Plateau, +Joueur)
% Menu pour choisir l'emplacement des pieces
placerCoord(3,P,_) :- placementOK(P),
	write('\n\n\n ---------- Placement termine ! ---------'),
	premierTour(P,1).
placerCoord(2,P,1) :- write('Choisissez les coordonnees (X,Y) ou placer une Kalista :\n'),
	choixCoords(P,1,'K',3,L),
	afficheCoups2(L,1),
	nl,read(Choix),
	nth1(Choix,L,[[Piece,(X,Y)]|_]),
	placerPiece(P,P2,X,Y,Piece,1),
	affichePlateau2(P2,36),
	nl,
	menuPlacer(P2,2).
placerCoord(2,P,2) :- write('Choisissez les coordonnees (X,Y) ou placer une Kalista :\n'),
	choixCoords(P,2,'K',4,L),
	afficheCoups2(L,1),
	nl,read(Choix),
	nth1(Choix,L,[[Piece,(X,Y)]|_]),
	placerPiece(P,P2,X,Y,Piece,2),
	affichePlateau2(P2,36),
	nl,
	menuPlacer(P2,1).
placerCoord(1,P,1) :- write('Choisissez les coordonnees (X,Y) ou placer un sbire :\n'),
	choixCoords(P,1,'S',3,L),
	afficheCoups2(L,1),
	nl,read(Choix),
	nth1(Choix,L,[[Piece,(X,Y)]|_]),
	placerPiece(P,P2,X,Y,Piece,1),
	affichePlateau2(P2,36),
	nl,
	menuPlacer(P2,2).
placerCoord(1,P,2) :- write('Choisissez les coordonnees (X,Y) ou placer un sbire :\n'),
	choixCoords(P,2,'S',4,L),
	afficheCoups2(L,1),
	nl,read(Choix),
	nth1(Choix,L,[[Piece,(X,Y)]|_]),
	placerPiece(P,P2,X,Y,Piece,2),
	affichePlateau2(P2,36),
	nl,
	menuPlacer(P2,1).
	
% placementOK(+Plateau)
% Renvoie vrai si toutes les pieces sont bien placees
placementOK(P) :- nbSbires(P,6,1), nbSbires(P,6,2), nbKalista(P,1,1), nbKalista(P,1,2).

% menuPlacer(+Plateau, +Joueur)
% Menu permettant aux joueurs de placer leurs pieces
menuPlacer(P,J) :- placementOK(P), premierTour(P,1).
menuPlacer(P,J) :- nbSbires(P,N,J),
	N < 6,
	nbKalista(P,M,J),
	M < 1,
	repeat,
	write('\nJoueur : '),
	write(J),
	write('\n\n1. Placer un sbire\n\n2. Placer une Kalista\n\n'),
	read(X),
	placerCoord(X,P,J).
menuPlacer(P,J) :- nbSbires(P,N,J),
	N < 6,
	repeat,
	write('\nJoueur : '),
	write(J),
	write('\n\n1. Placer un sbire\n\n'),
	read(X),
	X is 1,
	placerCoord(1,P,J).
menuPlacer(P,J) :- nbKalista(P,M,J),
	M < 1,
	repeat,
	write('\nJoueur : '),
	write(J),
	write('\n\n2. Placer une Kalista\n\n'),
	read(X),
	X is 2,
	placerCoord(2,P,J).