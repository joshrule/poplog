[].
concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).
?- concat([a,b,c],[d,e,f],[a,b,c,d,e,f,g]).
