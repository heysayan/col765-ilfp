% Test program for the Prolog-like Resolution Engine
% This file demonstrates facts, rules, and queries

% Facts about family relationships
father(ramesh, rohan).
mother(sita, rohan).
wife(sita, ramesh).
wife(babita, ramesh).

son(luv, ram).
son(luv, sita).
son(kush, ram).
son(kush, sita).

husband(ram, sita).

friend(hanuman, ram).
Not friend(ravan, ram).

% Rules
mother(X, Y) :- wife(X, Z), father(Z, Y).
child(A, B) :- son(A, B).
child(C, D) :- daughter(C, D).
parent(E, F) :- child(F, E).
married(G, H) :- wife(G, H).
married(I, J) :- husband(I, J).

% Queries
Q= father(ramesh, X).
Q= father(ramesh, M), mother(N, rohan).
Q= parent(M, kush).
Q= married(W, ram), Not friend(V, ram), friend(K, ram), son(O, ram).
