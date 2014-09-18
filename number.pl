concat([],L,L).
concat(list(X,L1),L2,list(X,L3)) :- concat(L1,L2,L3).
next(A, B) :- eq10(X, U), next1(Y, V), concat(X,Y,A), concat(U,V,B).
next(A, U) :- next10(X, U), concat(X,nine,A).
next(X, Y) :- next1(X, Y).
prev(X, Y) :- next(Y, X).
next1(one, two).
next1(two, three).
next1(three, four).
next1(five, six).
next1(six, seven).
next1(seven, eight).
next1(eight, nine).
next10(twenty, thirty).
next10(thirty, forty).
next10(forty, fifty).
next10(fifty, sixty).
next10(sixty, seventy).
next10(seventy, eighty).
next10(eighty, ninety).
eq1(one, one).
eq1(two, two).
eq1(three, three).
eq1(four, four).
eq1(five, five).
eq1(six, six).
eq1(seven, seven).
eq1(eight, eight).
eq1(nine, nine).
eq(ten, ten).
eq10(twenty, twenty).
eq10(thirty, thirty).
eq10(forty, forty).
eq10(fifty, fifty).
eq10(sixty, sixty).
eq10(seventy, seventy).
eq10(eighty, eighty).
eq10(ninety, ninety).
num1(one).
num1(two).
num1(three).
num1(four).
num1(five).
num1(six).
num1(seven).
num1(eight).
num1(nine).
num10(ten).
num10(twenty).
num10(thirty).
num10(forty).
num10(fifty).
num10(sixty).
num10(seventy).
num10(eighty).
num10(ninety).
numDigit(X, Y) :- numDigit1(X, Y).
numDigit(A, B) :- numDigit10(X, U), numDigit1(Y, V), concat(X,Y,A), concat(U,V,B).
numDigit(X, A) :- numDigit10(X, Y), concat(Y, 0, A).
numDigit1(one, 1).
numDigit1(two, 2).
numDigit1(three, 3).
numDigit1(four, 4).
numDigit1(five, 5).
numDigit1(six, 6).
numDigit1(seven, 7).
numDigit1(eight, 8).
numDigit1(nine, 9).
numDigit10(twenty, 2).
numDigit10(thirty, 3).
numDigit10(forty, 4).
numDigit10(fifty, 5).
numDigit10(sixty, 6).
numDigit10(seventy, 7).
numDigit10(eighty, 8).
numDigit10(ninety, 9).
greater1(one,two).
greater1(one,three).
greater1(one,four).
greater1(one,five).
greater1(one,six).
greater1(one,seven).
greater1(one,eight).
greater1(one,nine).
greater1(one,ten).
greater1(two,three).
greater1(two,four).
greater1(two,five).
greater1(two,six).
greater1(two,seven).
greater1(two,eight).
greater1(two,nine).
greater1(two,ten).
greater1(three,four).
greater1(three,five).
greater1(three,six).
greater1(three,seven).
greater1(three,eight).
greater1(three,nine).
greater1(three,ten).
greater1(four,five).
greater1(four,six).
greater1(four,seven).
greater1(four,eight).
greater1(four,nine).
greater1(four,ten).
greater1(five,six).
greater1(five,seven).
greater1(five,eight).
greater1(five,nine).
greater1(five,ten).
greater1(six,seven).
greater1(six,eight).
greater1(six,nine).
greater1(six,ten).
greater1(seven,eight).
greater1(seven,nine).
greater1(seven,ten).
greater1(eight,nine).
greater1(eight,ten).
greater1(nine,ten).
less1(X, Y) :- greater1(Y, X).
% approximate magnitude
% for any given query of some percept, the database should include an approximate magnitude. That percept will be some mean (variance scales deterministically with mean).
AM(scene1,1837)
% we also have the ability to add, subtract, multiply, or divide approximate magnitude estimates from each other
amDivide(scene1,scene2)
amMultiply(scene1,scene2)
