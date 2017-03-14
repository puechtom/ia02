% case_voisine(+X, +Y, -U, -V)
% Renvoie vrai si la case (X,Y) est à côté de la case (U,V)
case_voisine(X, Y, X, V) :- V is Y+1, 0 < V, V < 7.
case_voisine(X, Y, X, V) :- V is Y-1, 0 < V, V < 7.
case_voisine(X, Y, U, Y) :- U is X+1, 0 < U, U < 7.
case_voisine(X, Y, U, Y) :- U is X-1, 0 < U, U < 7.

% case_voisine(+I, +J)
% Renvoie vrai si la case I = (X,Y) est à côté de la case J = (U,V)
case_voisine((X,Y),(X,V)) :- V is Y+1, 0 < V, V < 7.
case_voisine((X,Y),(X,V)) :- V is Y-1, 0 < V, V < 7.
case_voisine((X,Y),(U,Y)) :- U is X+1, 0 < U, U < 7.
case_voisine((X,Y),(U,Y)) :- U is X-1, 0 < U, U < 7.

% cases_voisines(+I, -ListeCases)
% Renvoie une liste des cases pouvant être atteintes
cases_voisines(I,L) :- setof(J,case_voisine(I,J),L).

% chemin(+X, +Y, +N, -Liste)
% Renvoie dans Liste un chemin de N cases, Liste de la forme : [(X1,Y1),(X2,Y2),...]
chemin(_,_,0,[]).
chemin(X,Y,N,[(U,V)|Q]) :- case_voisine(X,Y,U,V), N > 0, M is N-1, chemin(U,V,M,Q).

% ajoutCaseDepart(+X, +Y, +ListeChemins, -NouvelleListe)
% Renvoie la nouvelle liste avec la case départ en tête
ajoutCaseDepart(_,_,[],[]).
ajoutCaseDepart(X,Y,[Chemin|Q],[[(X,Y)|Chemin]|L]) :- ajoutCaseDepart(X,Y,Q,L).

% listeChemin(+X, +Y, +N, -Liste)
% Renvoie dans Liste tous les chemins de longueur N à partir de (X,Y)
listeChemin(X,Y,N,Res) :- setof(L,chemin(X,Y,N,L),Liste), ajoutCaseDepart(X,Y,Liste,Res).

% sansCircuit(+Chemin)
% Renvoie vrai si le Chemin ne passe pas deux fois par la même case
sansCircuit([]).
sansCircuit([T|Q]) :- \+ member(T,Q), sansCircuit(Q).

% caseOccupee(+X, +Y, +Plateau)
% Renvoie vrai si la case (X,Y) est occupee par une piece
caseOccupee(X,Y,[[X,Y,_,'K',_,_]|_]).
caseOccupee(X,Y,[[X,Y,_,'S',_,_]|_]).
caseOccupee(X,Y,[[_,_,_,_,_,_]|Q]) :- caseOccupee(X,Y,Q).

% caseOccupeePar(+X, +Y, +Plateau, -Piece)
% Renvoie la Piece qui occupe la case (X,Y) du Plateau
caseOccupeePar(0,0,_,'S').
caseOccupeePar(X,Y,[[X,Y,_,'K',_,_]|_],'K').
caseOccupeePar(X,Y,[[X,Y,_,'S',_,_]|_],'S').
caseOccupeePar(X,Y,[[X,Y,_,' ',_,_]|_],' ').
caseOccupeePar(X,Y,[[_,_,_,_,_,_]|Q],Piece) :- caseOccupeePar(X,Y,Q,Piece).

% caseOccupeeJoueur(+X, +Y, +Plateau, -Joueur)
% Renvoie le Joueur qui occupe la case (X,Y) du Plateau
caseOccupeeJoueur(X,Y,[[X,Y,_,_,1,_]|_],1).
caseOccupeeJoueur(X,Y,[[X,Y,_,_,2,_]|_],2).
caseOccupeeJoueur(X,Y,[[_,_,_,_,_,_]|Q],Joueur) :- caseOccupeeJoueur(X,Y,Q,Joueur).

% isClear(+Chemin, +Plateau)
% Renvoie vrai si il n'y a pas de pièce sur le Chemin
isClear([],_).
isClear([(X,Y)|Q],P) :- \+ caseOccupee(X,Y,P) ,isClear(Q,P).

% isMove(+Chemin, +Plateau, +Joueur)
% Renvoie vrai si le chemin (ou coup à jouer) est valable
isMove([T|Q],P,J1) :- sansCircuit([T|Q]), reverse(Q,[(X,Y)|Q1]), caseOccupeeJoueur(X,Y,P,J2), \+ J1 is J2, isClear(Q1,P).
isMove([T|Q],P,_) :- sansCircuit([T|Q]), isClear(Q,P).

% listeMove(+Plateau, +Joueur, +cheminListePiece, -Liste)
% Renvoie la liste des coups possibles en retirant ceux qui ne correspondent pas aux règles du jeu
listeMove(_,_,[],[]).
listeMove(P,J,[T|Q],[T|L]) :- isMove(T,P,J), listeMove(P,J,Q,L).
listeMove(P,J,[T|Q],L) :- \+ isMove(T,P,J), listeMove(P,J,Q,L).

% listeMoveAll(+Plateau, +Joueur, +ListePieceAJouer, -ListeMove)
% Renvoie la liste complète des coups à jouer (pour chaque piece d'un joueur)
listeMoveAll(_,_,[],[]).
listeMoveAll(P,J,[[X,Y,VK,Piece,J,_]|Q1],Q2) :- listeChemin(X,Y,VK,Chem), listeMove(P,J,Chem,[]), listeMoveAll(P,J,Q1,Q2).
listeMoveAll(P,J,[[X,Y,VK,Piece,J,_]|Q1],[[[Piece,(X,Y)]|Coups]|Q2]) :- listeChemin(X,Y,VK,Chem), listeMove(P,J,Chem,Coups), listeMoveAll(P,J,Q1,Q2).

plateau_test([
	[1,1,1,'O',0,0],[1,2,2,'O',0,0],[1,3,2,'S',1,0],[1,4,3,'O',0,0],[1,5,1,'S',1,0],[1,6,2,'O',0,0],
	[2,1,3,'O',0,0],[2,2,1,'O',0,0],[2,3,3,'O',0,0],[2,4,1,'O',0,0],[2,5,3,'O',0,0],[2,6,2,'O',0,0],
	[3,1,2,'O',0,0],[3,2,3,'K',1,0],[3,3,1,'O',0,0],[3,4,2,'O',1,0],[3,5,1,'O',0,0],[3,6,3,'S',2,0],
	[4,1,2,'S',2,0],[4,2,1,'O',0,0],[4,3,3,'O',0,0],[4,4,1,'K',2,0],[4,5,3,'O',0,0],[4,6,1,'O',0,0],
	[5,1,1,'S',1,0],[5,2,2,'O',0,0],[5,3,1,'O',0,0],[5,4,3,'O',0,0],[5,5,1,'O',0,0],[5,6,2,'S',2,0],
	[6,1,3,'O',0,0],[6,2,2,'O',0,0],[6,3,2,'O',0,0],[6,4,1,'O',0,0],[6,5,3,'O',0,0],[6,6,2,'O',0,0]
	]).

% valeurKhan(+Plateau, -Valeur)
% Renvoie la valeur de la case du Khan
valeurKhan([],V) :- var(V), V is 0. 
valeurKhan([[_,_,V,_,_,1]|_],V).
valeurKhan([[_,_,_,_,_,0]|Q],V) :- valeurKhan(Q,V).

% pieceKhan(+Plateau, -X, -Y)
% Renvoie la coord de la case du khan
%pieceKhan([],V) :- var(V), V is 0. 
pieceKhan([[X,Y,_,_,_,1]|_],X, Y).
pieceKhan([[_,_,_,_,_,0]|Q],X, Y) :- pieceKhan(Q,X,Y).
pieceKhan([[_,_,_,_,_,_]|Q],X, Y).

% joueurSuivant(+Plateau, -Joueur)
% Le joueur suivant est celui qui ne possède pas le Khan (à voir si c'est vraiment utile)
joueurSuivant([[_,_,_,_,1,1]|_],2).
joueurSuivant([[_,_,_,_,2,1]|_],1).
joueurSuivant([[_,_,_,_,_,0]|Q],J) :- joueurSuivant(Q,J).

% casesJoueur(+Joueur, +Plateau, -ListeCases)
% Renvoie la liste des cases occupees par un joueur
casesJoueur(_,[],[]).
casesJoueur(J,[[X,Y,Z,Piece,J,K]|Q],[[X,Y,Z,Piece,J,K]|Q2]) :- casesJoueur(J,Q,Q2), !.
casesJoueur(J,[[_,_,_,_,_,_]|Q],L) :- casesJoueur(J,Q,L).

% piecesAJouer(+Plateau, +ValeurKhan, +Joueur, -ListeCases)
% Renvoie les cases dont les pieces peuvent etre jouees
piecesAJouer([],_,_,[]).
piecesAJouer([[X,Y,VK,Piece,J,K]|Q1],0,J,[[X,Y,VK,Piece,J,K]|Q2]) :- piecesAJouer(Q1,0,J,Q2), !.
piecesAJouer([[X,Y,VK,Piece,J,K]|Q1],VK,J,[[X,Y,VK,Piece,J,K]|Q2]) :- piecesAJouer(Q1,VK,J,Q2), !.
piecesAJouer([[_,_,_,_,_,_]|Q1],VK,J,Q2) :- piecesAJouer(Q1,VK,J,Q2).

% afficheCoups(+ListeCoups)
afficheCoups([]).
afficheCoups([[[Piece,Position]|Arrivees]|Q]) :- nl, write('Piece '), write(Piece), write(' en ('), write(Position), write('), coups possibles : '), nl, write(Arrivees), afficheCoups(Q).

% afficheCoups2(+ListeCoups)
afficheCoups2([], _).
afficheCoups2([[[Piece,Position]|Arrivees]|Q], N) :- 
	nl, write(N), write('. '), write(Piece), write(' ('), write(Position), write(')  '), M is N+1, afficheCoups2(Q, M).

% afficheCoups2(+ListeCoups)
affichePos([], _, []).
affichePos([T|Q], N, [T1|Q1]) :- 
	reverse(T, [T1|_]), nl, write(N), write('. '), write(T1), write('  '), M is N+1, affichePos(Q, M, Q1).
	

% piecesKhan(+Plateau, +ValeurKhan, +Joueur, -ListeCases)
% Renvoie les cases correspondantes à la valeur du Khan
piecesKhan([],_,_,[]).
piecesKhan([[X,Y,VK,' ',_,_]|Q1],VK,J,[[[(X,Y),(0,0)],[(X,Y)]]|Q2]) :- piecesKhan(Q1,VK,J,Q2), !.
piecesKhan([[_,_,_,_,_,_]|Q1],VK,J,Q2) :- piecesKhan(Q1,VK,J,Q2).

% Menus se lancant lorsque des pieces capturees peuvent etre remises en jeu
deblocage(P,J,Coups) :- pieceCapture(J,0), lancerDeblocage(1,J,P,Coups).	
deblocage(P,J,Coups) :- write('\n1. Jouer une des pieces deja sur le plateau\n2. Remettre une piece en jeu\n'), read(Choix), lancerDeblocage(Choix,J,P,Coups).
lancerDeblocage(1,J,P,Coups) :- piecesAJouer(P,_,J,L1), listeMoveAll(P,J,L1,Coups).
lancerDeblocage(2,J,P,Coups) :- pieceCapture(J,N), N>0, retract(pieceCapture(J,N)), M is N-1, asserta(pieceCapture(J,M)), valeurKhan(P,VK), nl, write(VK), nl, piecesKhan(P,VK,J,Coups).
lancerDeblocage(2,J,P,Coups) :- piecesAJouer(P,_,J,L1), listeMoveAll(P,J,L1,Coups).

% possibleMoves(+Board, +Player, -PossibleMoveList)
% Renvoie dans PossibleMoveList la liste des coups possibles pour le joueur, pattern : [[[Piece,Case],Coups]|Q]
possibleMoves(P,J,Coups) :- valeurKhan(P,VK), piecesAJouer(P,VK,J,L), listeMoveAll(P,J,L,[]), deblocage(P,J,Coups), !.
possibleMoves(P,J,Coups) :- valeurKhan(P,VK), piecesAJouer(P,VK,J,L), listeMoveAll(P,J,L,Coups), !.
possibleMoves(P,J,Coups) :- piecesAJouer(P,_,J,L), listeMoveAll(P,J,L,Coups).

% capturerPiece(+Case, +Joueur)
% Incrémente la valeur des pieces capturees pour le Joueur
capturerPiece((X,Y),J) :- pieceCapture(J,N), retract(pieceCapture(J,N)), M is N+1, asserta(pieceCapture(J,M)).

% move(+CaseDepart, +CaseArrivee, +Joueur, +Plateau, -NouveauPlateau)
% Modifie le plateau pour bouger une piece en CaseDepart = (X,Y) vers CaseArrivee = (X1,Y1)
move((X,Y),(X1,Y1),J,P,P6) :- 
	caseOccupeePar(X1,Y1,P,'S'),
	changerJ(J,Adv),
	capturerPiece((X1,Y1),Adv),
	effacerCase(P,P2,X1,Y1),
	caseOccupeePar(X,Y,P2,Piece),
	placerPiece(P2,P3,X1,Y1,Piece,J),
	pieceKhan(P3, XK, YK),
	retirerKhan((XK,YK), P3, P4),
	placerKhan((X1,Y1),P4, P5),
	effacerCase(P5,P6,X,Y).
move((X,Y),(X1,Y1),J,P2,P6) :- 
	caseOccupeePar(X1,Y1,P2,_),
	caseOccupeePar(X,Y,P2,Piece),
	placerPiece(P2,P3,X1,Y1,Piece,J),
	pieceKhan(P3, XK, YK),
	retirerKhan((XK,YK), P3, P4),
	placerKhan((X1,Y1),P4, P5),
	effacerCase(P5,P6,X,Y).

% premierTour(+Plateau, +Joueur)
% Joue le premier tour pour le joueur 1, on ne prend pas en compte le Khan puisqu'il n'y en a pas
premierTour(P,1) :- write('\n\n\n ----------- Tour du joueur 1 -----------'),
	piecesAJouer(P,_,J,L),
	listeMoveAll(P,J,L,Coups),
	afficheCoups2(Coups, 1),
	write('\nChoix ? Bouger la piece numero '),
	nl,read(A),
	nth1(A, Coups, [[Piece1,(X,Y)]|Chemin1]),
	write('Deplacer la piece '), write(Piece1), write(' '), write((X,Y)), write(' en position numero '), nl,
	affichePos(Chemin1,1,Choix),
	read(B),
	nth1(B, Choix, (X1,Y1)),
	write((X1,Y1)), nl,
	move((X,Y),(X1,Y1),1,P,NewP),
	placerKhan((X1,Y1),NewP,P2),
	tour(P2,2).

% tour(+Plateau, +Joueur)
% Menu servant à jouer les pièces d'un joueur ou vérifier que la partie est terminée
tour(P,J) :- changerJ(J, Adversaire), partieFinie(P,Adversaire), write('\nJoueur '), write(Adversaire), write(' gagne la partie !').
tour(P,1) :- 
	affichePlateau2(P,36),
	write('\n\n\n ----------- Tour du joueur 1 -----------'),
	possibleMoves(P,1,[[[Piece,Position]|Chemin]|Q]),
	afficheCoups2([[[Piece,Position]|Chemin]|Q], 1),
	write('\nChoix ? Bouger la piece numero '),
	nl,read(A),
	nth1(A, [[[Piece,Position]|Chemin]|Q], [[Piece1,(X,Y)]|Chemin1]),
	write('Deplacer la piece '), write(Piece1), write(' '), write((X,Y)), write(' en position numero '), nl,
	affichePos(Chemin1,1,Choix),
	read(B),
	nth1(B, Choix, (X1,Y1)),
	write((X1,Y1)), nl,
	move((X,Y),(X1,Y1),1,P,NewP),
	tour(NewP,2).
tour(P,2) :- 
	affichePlateau2(P,36),
	write('\n\n\n ----------- Tour du joueur 2 -----------'),
	possibleMoves(P,2,[[[Piece,Position]|Chemin]|Q]),
	afficheCoups2([[[Piece,Position]|Chemin]|Q], 1),
	write('\nChoix ? Bouger la piece numero '),
	nl,read(A),
	nth1(A, [[[Piece,Position]|Chemin]|Q], [[Piece1,(X,Y)]|Chemin1]),
	write('Deplacer la piece '), write(Piece1), write(' '), write((X,Y)), write(' en position numero '), nl,
	affichePos(Chemin1,1,Choix),
	read(B),
	nth1(B, Choix, (X1,Y1)),
	write((X1,Y1)), nl,
	move((X,Y),(X1,Y1),2,P,NewP),
	tour(NewP,1).