/* YOUR CODE HERE (Problem 1, delete the following line) */
range(S,E,M) :- M>=S, E>=M,true.
range(S,E,M) :- false.

?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Problem 2, delete the following line) */
reverseL([],[]).
reverseL([H | T],RevX) :- reverseL(T, B), append(B,[H],RevX).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 3, delete the following line) */
memberL(X,[]) :- false.
memberL(X,[H | T]) :- X==H, true.
memberL(X,[H | T ]) :- not(X==H), X=H.
memberL(X, [H | T]) :- memberL(X,T).

?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Problem 4, delete the following line) */
zip([],[],[]).
zip([H|T],[],[]).
zip([],[H|T],[]). 
zip([X | Xs], [Y | Ys], XYs) :- zip(Xs,Ys,B), append([X-Y],B,XYs).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
insert(X,[Y | Ys],[X, Y | Ys]):- X =< Y.
insert(X, [Y | Ys],[Y |Zs]):- not(X=<Y),insert(X,Ys,Zs).
insert(X,[],[X]).
insert(X,[X|Xs], [Y|Ys]):- false. 

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Problem 6, delete the following line) */
inListH(X,[Y|Ys]) :- X==Y, true.
inListH(X,[Y|Ys]) :- not(X==Y),inListH(X,Ys).
inListH(X,[]):- false.

remove_duplicates([],[]).
remove_duplicates(X,Y) :- remove_duplicatesH(X,[],Y).
remove_duplicatesH([], Acc, Acc).
remove_duplicatesH([H|T],Acc,N) :- not(inListH(H,Acc)), append(Acc,[H],W), remove_duplicatesH(T,W,N).
remove_duplicatesH([H|T],Acc,N) :- inListH(H,Acc), remove_duplicatesH(T,Acc,N).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Problem 7, delete the following line) */
inList(X,[]):- false.
inList(X,[Y|Ys]) :- X==Y, true.
inList(X,[Y|Ys]) :- not(X==Y),inList(X,Ys).

intersectionL([], L2, []).
intersectionL([L |Ls],L2,[L | B]) :- inList(L,L2), intersectionL(Ls,L2, B).
intersectionL([L |Ls],L2, B) :- not(inList(L,L2)), intersectionL(Ls,L2, B).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

/* YOUR CODE HERE (Problem 8, delete the following line) */
partition([X],[X],[]).
partition(L, P, S) :- not(L=[X]), prefix(P,L),suffix(S,L),length(L,Le),V is div(Le,2), length(P,V), R is Le - V, length(S,R).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 9, delete the following line) */
merge([],[],[]).
merge([],[Z|Zs],[Z|Zs]).
merge([Z | Zs],[],[Z |Zs]).
merge([X | Xs],[Y|Ys],Zs) :- X=<Y, merge(Xs,[Y|Ys],B), append([X],B,Zs).
merge([X | Xs],[Y|Ys],Zs) :- X>Y, merge([X | Xs],Ys,B), append([Y],B,Zs).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 10, delete the following line) */
mergesort(B,SL):- not(B=[]),not(B=[L]),partition(B,Ls,Rs), mergesort(Ls,W1),mergesort(Rs,W2), merge(W1,W2,SL).  
mergesort([L],[L]).
mergesort([],[]).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
