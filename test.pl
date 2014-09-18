man(socrates).
mortal(X) :- man(X).
?- mortal(X).
g(A,C) :- g(A,B),g(B,C).
g(three,two).
g(two,one).
?- g(three,one).
