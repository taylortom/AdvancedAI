testAddition(X, Y, Z) :-
Z is X+Y.



countDown(Remaining):-
nl, write(' Remaing : ' - Remaining),nl,ttyflush,
Remaining > 0,
SubRemaing is Remaining-1, 
countDown(SubRemaing).