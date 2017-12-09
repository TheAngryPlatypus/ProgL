%Q1
remove_item(X,[],[]).
remove_item(X,[X|Xs],Ys):-remove_item(X,Xs,Ys),!.
remove_item(X,[O|Xs],Ys):-remove_item(X,Xs,Yt), append([O],Yt,Ys).

%Q2
remove_items([],Y,Y) .
remove_items([X|Xs], Y, O):- remove_item(X, Y, Xv), remove_items(Xs, Xv, O).

%Q3
intersection([],_,[]) .
intersection(_,[],[]).
intersection([X|Xs], [X|Ys], I):-intersection(Xs, Ys, X1), append(X1,[X],Xl), remove_dups(Xl, I), !.
intersection([X|Xs],[Y|Ys],I):-intersection([X|Xs], Ys ,X1), intersection([Y|Ys], Xs, X2), append(X1,X2,Xl), remove_dups(Xl, I),  !.

%Q4
is_set([]).
is_set([L|Ls]):- not((member(L, Ls))), is_set(Ls).

%Q5
disjoint_union(X,Y,U):- intersection(X,Y,I), remove_items(I,X,A1), remove_items(I, Y, A2), append(A1,A2, U).

%Q6
remove_dups([],[]).
remove_dups([X|Xs],O):- remove_item(X,Xs,Xn), remove_dups(Xn,Y), append(Y,[X], O),!.

%Q7
union(L1,L2, X):-intersection(L1,L2,Y1), disjoint_union(L1,L2,Y2), append(Y1,Y2,Yt), remove_dups(Yt,X).