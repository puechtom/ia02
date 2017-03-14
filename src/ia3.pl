% listeMoveIA(+Plateau, +Joueur, +cheminListePiece, -Liste)
% Renvoie la liste des coups possibles en retirant ceux qui ne correspondent pas aux règles du jeu
listeMoveIA(_,_,[],[]).
listeMoveIA(P,J,[[TC|QC]|Q],[[TC,TR]|L]) :- isMove([TC|QC],P,J), reverse(QC,[TR|_]), listeMoveIA(P,J,Q,L).
listeMoveIA(P,J,[T|Q],L) :- \+ isMove(T,P,J), listeMoveIA(P,J,Q,L).

% listeMoveAllIA(+Plateau, +Joueur, +ListePieceAJouer, -ListeMove)
% Renvoie la liste complète des coups à jouer (pour chaque piece d'un joueur)
listeMoveAllIA(_,_,[],[]).
listeMoveAllIA(P,J,[[X,Y,VK,Piece,J,_]|Q1],Res) :- listeChemin(X,Y,VK,Chem), listeMoveIA(P,J,Chem,Coups), listeMoveAllIA(P,J,Q1,Q2), append(Coups,Q2,Res).

% possibleMovesIA()
% Renvoie une liste de coups possibles selon le pattern suivant : [[(X,Y)|Q1]|Q2]
possibleMovesIA(P,J,Coups) :- valeurKhan(P,VK), piecesAJouer(P,VK,J,L), listeMoveAllIA(P,J,L,[]), piecesAJouer(P,_,J,L1), listeMoveAllIA(P,J,L1,Coups), !.
possibleMovesIA(P,J,Coups) :- valeurKhan(P,VK), piecesAJouer(P,VK,J,L), listeMoveAllIA(P,J,L,Coups).
firstPossibleMovesIA(P,J,Coups) :- piecesAJouer(P,_,J,L), listeMoveAllIA(P,J,L,Coups).

nextPosList([], []).
nextPosList([T|Q],[X|Q2]) :- reverse(T,[X|_]), nextPosList(Q,Q2).

% nbKalista(+Plateau, -NbKalista, +Joueur).
% Renvoie le nombre de Kalista pour un Joueur donné
nbKalista([],0,_).
nbKalista([[_,_,_,' ',_,_]|Q],N,J) :- nbKalista(Q,N,J).
nbKalista([[_,_,_,'S',_,_]|Q],N,J) :- nbKalista(Q,N,J).
nbKalista([[_,_,_,'K',J,_]|Q],NB,J) :- nbKalista(Q,N,J),! , NB is N+1.
nbKalista([[_,_,_,'K',_,_]|Q],N,J) :- nbKalista(Q,N,J).

% nbSbires(+Plateau, -NbSbires, +Joueur).
% Renvoie le nombre de sbires pour un Joueur donné
nbSbires([],0,_).
nbSbires([[_,_,_,' ',_,_]|Q],N,J) :- nbSbires(Q,N,J).
nbSbires([[_,_,_,'K',_,_]|Q],N,J) :- nbSbires(Q,N,J).
nbSbires([[_,_,_,'S',J,_]|Q],NB,J) :- nbSbires(Q,N,J), !, NB is N+1.
nbSbires([[_,_,_,'S',_,_]|Q],N,J) :- nbSbires(Q,N,J).

% nbCasesDouble(+Plateau, -NbSbires, +Joueur).
% Renvoie le nombre de sbires pour un Joueur donné
nbCasesSimple([],0,_).
nbCasesSimple([[_,_,1,'K',J,_]|Q],N,J) :- nbCasesSimple(Q,N,J).
nbCasesSimple([[_,_,1,'S',J,_]|Q],NB,J) :- nbCasesSimple(Q,N,J), !, NB is N+1.
nbCasesSimple([[_,_,_,_,_,_]|Q],N,J) :- nbCasesSimple(Q,N,J).

% nbCasesDouble(+Plateau, -NbSbires, +Joueur).
% Renvoie le nombre de sbires pour un Joueur donné
nbCasesDouble([],0,_).
nbCasesDouble([[_,_,2,'K',J,_]|Q],N,J) :- nbCasesDouble(Q,N,J).
nbCasesDouble([[_,_,2,'S',J,_]|Q],NB,J) :- nbCasesDouble(Q,N,J), !, NB is N+1.
nbCasesDouble([[_,_,_,_,_,_]|Q],N,J) :- nbCasesDouble(Q,N,J).

% nbCasesDouble(+Plateau, -NbSbires, +Joueur).
% Renvoie le nombre de sbires pour un Joueur donné
nbCasesTriple([],0,_).
nbCasesTriple([[_,_,3,'K',J,_]|Q],N,J) :- nbCasesTriple(Q,N,J).
nbCasesTriple([[_,_,3,'S',J,_]|Q],NB,J) :- nbCasesTriple(Q,N,J), !, NB is N+1.
nbCasesTriple([[_,_,_,_,_,_]|Q],N,J) :- nbCasesTriple(Q,N,J).

% nbCasesDouble(+Plateau, -NbSbires, +Joueur).
% Renvoie le nombre de sbires pour un Joueur donné
%desobeirKhan(+NS,+ND,+NT,-C).
desobeirKhan(0,0,_,3).
desobeirKhan(_,0,0,3).
desobeirKhan(0,_,0,3).
desobeirKhan(_,_,0,1).
desobeirKhan(0,_,_,1).
desobeirKhan(_,0,_,1).
desobeirKhan(_,_,_,0).

% valeurKalista(+Plateau, -NbSbires, +Joueur).
% Renvoie le nombre de sbires pour un Joueur donné
valeurKalista([],0,_).
valeurKalista([[_,_,ValK,'K',J,_]|Q],ValK,J).
valeurKalista([[_,_,_,_,_,_]|Q],ValK,J) :- valeurKalista(Q,ValK,J).

% partieFinie(+Plateau, +Joueur)
% Détermine si le Joueur a gagne ou non
partieFinie(P,1) :- nbKalista(P,1,1), nbKalista(P,0,2).
partieFinie(P,2) :- nbKalista(P,1,2), nbKalista(P,0,1).

% changerJ(+J1, -J2)
% Renvoie le numéro du joueur adverse
changerJ(1, 2).
changerJ(2, 1).

% eval(+Plateau, +Joueur, -Score)
% Evalue un score selon l'etat du plateau
eval(P,J,20) :- partieFinie(P,J), !.
eval(P,1,-20) :- partieFinie(P,2), !.
eval(P,2,-20) :- partieFinie(P,1), !.
eval(P,1,Score) :- nbCasesSimple(P,NS,1), nbCasesDouble(P,ND,1), nbCasesTriple(P,NT,1), desobeirKhan(NS,ND,NT,C), valeurKalista(P,ValK,1), nbSbires(P,N1,1), nbSbires(P,N2,2), N1 >= N2, Diff is N2-N1, Score is Diff+ValK+C.
eval(P,1,Score) :- nbCasesSimple(P,NS,1), nbCasesDouble(P,ND,1), nbCasesTriple(P,NT,1), desobeirKhan(NS,ND,NT,C), valeurKalista(P,ValK,1), nbSbires(P,N1,1), nbSbires(P,N2,2), N1 < N2, Diff is N2-N1, Score is Diff+ValK+C.
eval(P,2,Score) :- nbCasesSimple(P,NS,2), nbCasesDouble(P,ND,2), nbCasesTriple(P,NT,2), desobeirKhan(NS,ND,NT,C), valeurKalista(P,ValK,2), nbSbires(P,N1,1), nbSbires(P,N2,2), N2 >= N1, Diff is N1-N2, Score is Diff+ValK+C.
eval(P,2,Score) :- nbCasesSimple(P,NS,2), nbCasesDouble(P,ND,2), nbCasesTriple(P,NT,2), desobeirKhan(NS,ND,NT,C), valeurKalista(P,ValK,2), nbSbires(P,N1,1), nbSbires(P,N2,2), N2 < N1, Diff is N1-N2, Score is Diff+ValK+C.
eval(_,_,0) :- write('###################################### ERREUR ######################################\n').


% max(+Plateau, +Joueur, +ListeCoups, +Profondeur, -ListeVal)
% min(+Plateau, +Joueur, +ListeCoups, +Profondeur, -ListeVal)
% Implémentation de l'algorithme min-max
max1(P,J,[],_,_). %:- %write('Arret'), write('\n').
max1(P,J,_,0,[Val]) :- eval(P,J,Val).				% on est sur une feuille, on evalue donc le coup courant
max1(P,J,[[CaseDepart|Chem]|Q],Prof,[ValMin|QVal]) :-
	reverse([CaseDepart|Chem],[CaseArrivee|_]),
	move(CaseDepart,CaseArrivee,J,P,NewP),				% on simule un coup
	changerJ(J, NJ),
	possibleMovesIA(NewP,NJ,Coups), 											% on met à jour la liste des coups possibles
	Prof > 0,ProfTemp is Prof-1, 
	%write([CaseDepart,CaseArrivee]), write('\n'),
	min(NewP,NJ,Coups,ProfTemp,LVal),
	min_list(LVal,ValMin),
	%write(LVal), write('\n'),
	%write(Q), write('\n'), write('\n'),
	max1(P,J,Q,Prof,QVal).

min(P,J,[],_,_).
min(P,J,_,0,[Val]) :- eval(P,J,Val).
min(P,J,[[CaseDepart|Chem]|Q],Prof,[ValMax|QVal]) :-
	reverse([CaseDepart|Chem],[CaseArrivee|_]),
	move(CaseDepart,CaseArrivee,J,P,NewP),
	changerJ(J, NJ),
	possibleMovesIA(NewP,NJ,Coups),
	Prof > 0,ProfTemp is Prof-1,
	max1(NewP,NJ,Coups,ProfTemp,LVal),
	%write(Coups), write('\n'),
	max_list(LVal,ValMax),
	min(P,J,Q,Prof,QVal).

% meilleurCoup(+Plateau, +Joueur, -Coup)
% Renvoie le meilleur coup calcule par l'IA	
meilleurCoup(P,J,MeilleurCoup) :- possibleMovesIA(P,J,Coups), max1(P,J,Coups,2,LVal), max_list(LVal,Max), nth1(N,LVal,Max), nth1(N,Coups,MeilleurCoup).

% GNU :  plateau_test(P), possibleMovesIA(P,1,Coups), write(Coups), write('\n'), max1(P,1,Coups,2,LVal), max_list(LVal,Max), nth1(N,LVal,Max), nth1(N,Coups,MeilleurCoup).
% SWI :  plateau_test(P), possibleMovesIA(P,1,Coups), max1(P,1,Coups,2,LVal), max_list(LVal,Max), element(N,LVal,Max), element(N,Coups,MeilleurCoup).


plateauVIDE([
	[1,1,2,' ',0,0],[2,1,2,' ',0,0],[3,1,3,' ',0,0],[4,1,1,' ',0,0],[5,1,2,' ',0,0],[6,1,2,' ',0,0],
	[1,2,1,' ',0,0],[2,2,3,' ',0,0],[3,2,1,' ',0,0],[4,2,3,' ',0,0],[5,2,1,' ',0,0],[6,2,3,' ',0,0],
	[1,3,3,' ',0,0],[2,3,1,' ',0,0],[3,3,2,' ',0,0],[4,3,2,' ',0,0],[5,3,3,' ',0,0],[6,3,1,' ',0,0],
	[1,4,2,' ',0,0],[2,4,3,' ',0,0],[3,4,1,' ',0,0],[4,4,3,' ',0,0],[5,4,1,' ',0,0],[6,4,2,' ',0,0],
	[1,5,2,' ',0,0],[2,5,1,' ',0,0],[3,5,3,' ',0,0],[4,5,1,' ',0,0],[5,5,3,' ',0,0],[6,5,2,' ',0,0],
	[1,6,1,' ',0,0],[2,6,3,' ',0,0],[3,6,2,' ',0,0],[4,6,2,' ',0,0],[5,6,1,' ',0,0],[6,6,3,' ',0,0]
]).

plateauJvIA([
	[1,1,2,' ',0,0],[2,1,2,' ',0,0],[3,1,3,'K',1,0],[4,1,1,' ',0,0],[5,1,2,' ',0,0],[6,1,2,' ',0,0],
	[1,2,1,'S',1,0],[2,2,3,'S',1,0],[3,2,1,'S',1,0],[4,2,3,'S',1,0],[5,2,1,'S',1,0],[6,2,3,'S',1,0],
	[1,3,3,' ',0,0],[2,3,1,' ',0,0],[3,3,2,' ',0,0],[4,3,2,' ',0,0],[5,3,3,' ',0,0],[6,3,1,' ',0,0],
	[1,4,2,' ',0,0],[2,4,3,' ',0,0],[3,4,1,' ',0,0],[4,4,3,' ',0,0],[5,4,1,' ',0,0],[6,4,2,' ',0,0],
	[1,5,2,' ',0,0],[2,5,1,' ',0,0],[3,5,3,' ',0,0],[4,5,1,' ',0,0],[5,5,3,' ',0,0],[6,5,2,' ',0,0],
	[1,6,1,' ',0,0],[2,6,3,' ',0,0],[3,6,2,' ',0,0],[4,6,2,' ',0,0],[5,6,1,' ',0,0],[6,6,3,' ',0,0]
]).

plateauBLOCAGE([
	[1,1,2,' ',0,0],[2,1,2,' ',0,0],[3,1,3,'K',1,0],[4,1,1,' ',0,0],[5,1,2,' ',0,0],[6,1,2,' ',0,0],
	[1,2,1,'S',1,0],[2,2,3,'S',1,0],[3,2,1,'S',1,0],[4,2,3,'S',1,0],[5,2,1,'S',1,0],[6,2,3,'S',1,0],
	[1,3,3,' ',0,0],[2,3,1,' ',0,0],[3,3,2,' ',0,0],[4,3,2,' ',0,0],[5,3,3,' ',0,0],[6,3,1,' ',0,0],
	[1,4,2,' ',0,0],[2,4,3,' ',0,0],[3,4,1,' ',0,0],[4,4,3,' ',0,0],[5,4,1,' ',0,0],[6,4,2,' ',0,0],
	[1,5,2,' ',0,0],[2,5,1,' ',0,0],[3,5,3,' ',0,0],[4,5,1,'S',2,0],[5,5,3,' ',0,0],[6,5,2,' ',0,0],
	[1,6,1,' ',0,0],[2,6,3,' ',0,0],[3,6,2,' ',0,0],[4,6,2,' ',0,0],[5,6,1,'K',2,0],[6,6,3,' ',0,0]
]).

plateauIAvIA([
	[1,1,2,' ',0,0],[2,1,2,' ',0,0],[3,1,3,'K',1,0],[4,1,1,' ',0,0],[5,1,2,' ',0,0],[6,1,2,' ',0,0],
	[1,2,1,'S',1,0],[2,2,3,'S',1,0],[3,2,1,'S',1,0],[4,2,3,'S',1,0],[5,2,1,'S',1,0],[6,2,3,'S',1,0],
	[1,3,3,' ',0,0],[2,3,1,' ',0,0],[3,3,2,' ',0,0],[4,3,2,' ',0,0],[5,3,3,' ',0,0],[6,3,1,' ',0,0],
	[1,4,2,' ',0,0],[2,4,3,' ',0,0],[3,4,1,' ',0,0],[4,4,3,' ',0,0],[5,4,1,' ',0,0],[6,4,2,' ',0,0],
	[1,5,2,'S',2,0],[2,5,1,'S',2,0],[3,5,3,'S',2,0],[4,5,1,'S',2,0],[5,5,3,'S',2,0],[6,5,2,'S',2,0],
	[1,6,1,' ',0,0],[2,6,3,'K',2,0],[3,6,2,' ',0,0],[4,6,2,' ',0,0],[5,6,1,' ',0,0],[6,6,3,' ',0,0]
]).

plateauIA([
	[1,1,1,'K',2,0],[2,1,2,'S',2,0],[3,1,2,'O',0,0],[4,1,3,'S',1,0],[5,1,1,'O',0,0],[6,1,2,'S',1,0],
	[1,2,3,'S',2,0],[2,2,1,'O',0,0],[3,2,3,'S',1,0],[4,2,1,'O',0,0],[5,2,3,'S',2,0],[6,2,2,'S',1,0],
	[1,3,2,'O',0,0],[2,3,3,'O',0,0],[3,3,1,'O',0,0],[4,3,2,'O',0,0],[5,3,1,'O',0,0],[6,3,3,'O',0,0],
	[1,4,2,'O',0,0],[2,4,1,'O',0,0],[3,4,3,'O',0,0],[4,4,1,'O',0,0],[5,4,3,'O',0,0],[6,4,1,'O',0,0],
	[1,5,1,'S',2,0],[2,5,2,'O',0,0],[3,5,1,'O',0,0],[4,5,3,'O',0,0],[5,5,1,'K',1,0],[6,5,2,'O',0,0],
	[1,6,3,'O',0,0],[2,6,2,'O',0,0],[3,6,2,'O',0,0],[4,6,1,'O',0,0],[5,6,3,'O',0,0],[6,6,2,'O',0,0]
]).

plateauIA02([
	[1,1,1,'K',2,0],[2,1,2,'S',2,0],[3,1,2,'O',0,0],[4,1,3,'S',2,0],[5,1,1,'O',0,0],[6,1,2,'S',2,0],
	[1,2,3,'S',2,0],[2,2,1,'O',0,0],[3,2,3,'S',2,0],[4,2,1,'O',0,0],[5,2,3,'S',2,0],[6,2,2,'O',0,0],
	[1,3,2,'O',0,0],[2,3,3,'O',0,0],[3,3,1,'O',0,0],[4,3,2,'O',0,0],[5,3,1,'O',0,0],[6,3,3,'O',0,0],
	[1,4,2,'O',0,0],[2,4,1,'O',0,0],[3,4,3,'O',0,0],[4,4,1,'O',0,0],[5,4,3,'O',0,0],[6,4,1,'O',0,0],
	[1,5,1,'O',0,0],[2,5,2,'S',2,0],[3,5,1,'O',0,0],[4,5,3,'S',1,0],[5,5,1,'K',1,0],[6,5,2,'S',1,0],
	[1,6,3,'O',0,0],[2,6,2,'O',0,0],[3,6,2,'S',1,0],[4,6,1,'O',0,0],[5,6,3,'S',1,0],[6,6,2,'O',0,0]
]).

plateauIA03([
	[1,1,2,'S',1,0],[2,1,2,'O',0,0],[3,1,2,'S',1,0],[4,1,2,'O',0,0],[5,1,2,'S',1,0],[6,1,2,'O',0,0],
	[1,2,2,'O',0,0],[2,2,1,'S',1,0],[3,2,2,'O',0,0],[4,2,1,'S',1,0],[5,2,3,'K',1,0],[6,2,2,'S',1,0],
	[1,3,2,'O',0,0],[2,3,3,'O',0,0],[3,3,1,'O',0,0],[4,3,2,'O',0,0],[5,3,1,'O',0,0],[6,3,3,'O',0,0],
	[1,4,2,'O',0,0],[2,4,1,'O',0,0],[3,4,3,'O',0,0],[4,4,1,'O',0,0],[5,4,3,'O',0,0],[6,4,1,'O',0,0],
	[1,5,1,'O',0,0],[2,5,2,'O',0,0],[3,5,1,'O',0,0],[4,5,3,'O',0,0],[5,5,1,'O',0,0],[6,5,2,'O',0,0],
	[1,6,3,'O',0,0],[2,6,2,'O',0,0],[3,6,2,'O',0,0],[4,6,1,'O',0,0],[5,6,3,'O',0,0],[6,6,2,'O',0,0]
]).

plateauBUG([
	[1,1,1,'K',2,0],[2,1,2,'O',0,1],[3,1,2,'O',0,0],[4,1,3,'S',2,0],[5,1,1,'O',0,0],[6,1,2,'O',0,0],
	[1,2,3,'S',2,0],[2,2,1,'O',0,0],[3,2,3,'S',2,0],[4,2,1,'S',1,1],[5,2,3,'S',2,0],[6,2,2,'O',0,0],
	[1,3,2,'O',0,0],[2,3,3,'S',1,0],[3,3,1,'O',0,0],[4,3,2,'O',0,0],[5,3,1,'O',0,0],[6,3,3,'O',0,0],
	[1,4,2,'S',1,1],[2,4,1,'O',0,0],[3,4,3,'O',0,0],[4,4,1,'O',0,0],[5,4,3,'O',0,0],[6,4,1,'O',0,0],
	[1,5,1,'O',0,0],[2,5,2,'S',1,0],[3,5,1,'O',0,0],[4,5,3,'O',0,0],[5,5,1,'K',1,0],[6,5,2,'S',1,0],
	[1,6,3,'O',0,0],[2,6,2,'O',0,0],[3,6,2,'S',1,0],[4,6,1,'O',0,0],[5,6,3,'S',1,0],[6,6,2,'O',0,0]
]).

plateauBUG2([
	[1,1,1,'O',0,0],[2,1,2,'K',2,0],[3,1,2,'O',0,0],[4,1,3,'S',2,0],[5,1,1,'O',0,0],[6,1,2,'S',2,0],
	[1,2,3,'S',2,0],[2,2,1,'O',0,0],[3,2,3,'S',2,0],[4,2,1,'O',0,0],[5,2,3,'S',2,0],[6,2,2,'O',0,0],
	[1,3,2,'O',0,0],[2,3,3,'S',2,0],[3,3,1,'O',0,0],[4,3,2,'O',0,0],[5,3,1,'O',0,0],[6,3,3,'O',0,0],
	[1,4,2,'S',1,1],[2,4,1,'S',1,0],[3,4,3,'O',0,0],[4,4,1,'O',0,0],[5,4,3,'O',0,0],[6,4,1,'O',0,0],
	[1,5,1,'O',0,0],[2,5,2,'O',0,0],[3,5,1,'O',0,0],[4,5,3,'O',0,0],[5,5,1,'K',1,0],[6,5,2,'S',1,0],
	[1,6,3,'O',0,0],[2,6,2,'O',0,0],[3,6,2,'O',0,0],[4,6,1,'O',0,0],[5,6,3,'S',1,0],[6,6,2,'O',0,0]
]).

% moveIA(+Plateau, +Joueur, -Coup, -NouveauPlateau)
% Renvoie le coup que joue l'IA ainsi que le nouveau plateau
moveIA(P,J,[CaseDepart,CaseArrivee],NewP) :- meilleurCoup(P,J,[CaseDepart,CaseArrivee]), move(CaseDepart,CaseArrivee,J,P,NewP).

% firstMoveIA(+Plateau, +Joueur, +ListeCases, -NouveauPlateau)
% Joue le premier mouvement de l'IA
firstMoveIA(P,J,[CaseDepart,CaseArrivee],NewP) :- meilleurCoup(P,J,[CaseDepart,CaseArrivee]), move(CaseDepart,CaseArrivee,J,P,NewP).
	
% tourIA(+Plateau, +Joueur)
% Menus permettant de faire une partie Joueur contre IA
tourIA(P,1) :-
	write('\n\n\n ----------- Tour de l"IA -----------'),
	moveIA(P,1,[CaseDepart,CaseArrivee],NewP),
	write('\nOrdi joue la piece en '),write(CaseDepart),write(' vers '), write(CaseArrivee), nl,
	tourIA2(NewP,2).

tourIA2(P,2) :- 
	affichePlateau2(P,36),
	write('\n\n\n ----------- A vous de jouer -----------'),
	possibleMoves(P,2,[[[Piece,Position]|Chemin]|Q]),
	afficheCoups2([[[Piece,Position]|Chemin]|Q], 1),
	write('\nChoix ? Bouger la piece numero '),
	read(A),
	%write([[[Piece,Position]|Chemin]|Q]), nl,
	nth1(A, [[[Piece,Position]|Chemin]|Q], [[Piece1,(X,Y)]|Chemin1]),
	write('Deplacer la piece '), write(Piece1), write(' '), write((X,Y)), write(' en position numero '), nl,
	affichePos(Chemin1,1,Choix),
	read(B),
	%write(Choix), nl,
	nth1(B, Choix, (X1,Y1)),
	write((X1,Y1)), nl,
	move((X,Y),(X1,Y1),2,P,NewP),
	affichePlateau2(NewP,36),
	tourIA(NewP,1).

% premierTourIA(+Plateau, +Joueur)
% Joue le premier tour de l'IA (ici l'IA est le joueur 1)
premierTourIA(P,1) :-
	firstMoveIA(P,1,[CaseDepart,CaseArrivee],P1),
	write('\n\n\n ----------- Tour de l"IA -----------'),
	write('\nOrdi joue la piece en '),write(CaseDepart),write(' vers '), write(CaseArrivee), nl,
	placerKhan(CaseArrivee,P1,P2), tourIA2(P2,2).

% menuPlacerIA(+Plateau, +Joueur)
% Menu permettant de placer les pieces pour une partie Joueur contre IA
menuPlacerIA(P,J) :- placementOK(P), premierTourIA(P,1).	
menuPlacerIA(P,J) :- nbSbires(P,N,J),
	N < 6,
	nbKalista(P,M,J),
	M < 1,
	repeat,
	write('\nJoueur : '),
	write(J),
	write('\n\n1. Placer un sbire\n\n2. Placer une Kalista\n\n'),
	read(X),
	placerCoordIA(X,P,J).
menuPlacerIA(P,J) :- nbSbires(P,N,J),
	N < 6,
	repeat,
	write('\nJoueur : '),
	write(J),
	write('\n\n1. Placer un sbire\n\n'),
	read(X),
	X is 1,
	placerCoordIA(1,P,J).
menuPlacerIA(P,J) :- nbKalista(P,M,J),
	M < 1,
	repeat,
	write('\nJoueur : '),
	write(J),
	write('\n\n2. Placer une Kalista\n\n'),
	read(X),
	X is 2,
	placerCoordIA(2,P,J).

% placerCoord(+NumPiece, +Plateau, +Joueur)
% Menu pour placer les pieces d'un joueur
placerCoordIA(2,P,2) :- write('Choisissez les coordonnees (X,Y) ou placer une Kalista :\n'),
	choixCoords(P,2,'K',4,L),
	afficheCoups2(L,1),
	nl,read(Choix),
	nth1(Choix,L,[[Piece,(X,Y)]|_]),
	placerPiece(P,P2,X,Y,Piece,2),
	affichePlateau2(P2,36),
	nl,
	menuPlacerIA(P2,2).
placerCoordIA(1,P,2) :- write('Choisissez les coordonnees (X,Y) ou placer un sbire :\n'),
	choixCoords(P,2,'S',4,L),
	afficheCoups2(L,1),
	nl,read(Choix),
	nth1(Choix,L,[[Piece,(X,Y)]|_]),
	placerPiece(P,P2,X,Y,Piece,2),
	affichePlateau2(P2,36),
	nl,
	menuPlacerIA(P2,2).

menuIAvsIA(P) :- playIA(P,1).

playIA(P, J) :- partieFinie(P,J), write('\nJoueur '), write(J), write(' gagne la partie !').
playIA(P, J) :- changerJ(J, NJ), partieFinie(P,NJ), write('\nJoueur '), write(NJ), write(' gagne la partie !').
playIA(P, J) :-
	write('\n\n\nTour du J'), write(J), nl,
	moveIA(P,J,[CaseDepart,CaseArrivee],NP),
	write('J'), write(J), write(' joue la piece en '), write(CaseDepart), write(' vers '), write(CaseArrivee),
	affichePlateau2(NP, 36),
	%sleep(2),
	changerJ(J, NJ),
	playIA(NP, NJ).