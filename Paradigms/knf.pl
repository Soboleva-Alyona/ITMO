knf_from_expr(and(E, E), E) :- !. 
knf_from_expr(and(_, 0), 0) :- !.
knf_from_expr(and(0, _), 0) :- !.
% :NOTE: * Это так не работает
knf_from_expr(and(E, 1), R) :- knf_from_expr(E, 1), !.


knf_from_expr(or(E, 1), 1) :- !. 
knf_from_expr(or(1, E), 1) :- !. 

knf_from_expr(or(E, 0), R) :- knf_from_expr(E, R), !.
knf_from_expr(or(0, E), R) :- knf_from_expr(E, R), !.
knf_from_expr(or(E, E), E) :- !.
knf_from_expr(or(not(E), not(E)), not(E)) :- !.

dznk(and(E1, E2), 0) :- 
	dznk(E1, 1),
	dznk(E2, 1), !.
	
dznk(or(E1, E2), 1) :-
	knf_from_expr(E1, R1),
	dznk(R1, 1),
	dznk(E2, 1), !.

dznk(E, 1) :- atom(E).
dznk(E, 1) :- E is 1.
dznk(E, 1) :- E is 0.
dznk(not(E), 1) :- atom(E). 

%%%
%or

knf_from_expr(or(and(E1, E2), E3), and(R1, R2)) :-
	knf_from_expr(or(E1, E3), R1),
	knf_from_expr(or(E2, E3), R2), !. 


knf_from_expr(or(E1, E2), and(R1, R2)) :- 
	knf_from_expr(E1, and(K11, K12)),
	knf_from_expr(E2, K2), 
	knf_from_expr(or(K11, K2), R1), 
	knf_from_expr(or(K12, K2), R2), !.
	
knf_from_expr(or(E1, E2), and(K121, K122)) :-
	knf_from_expr(E1, K1),
	knf_from_expr(E2, and(K21, K22)), 
	knf_from_expr(or(K1, K21), K121), 
	knf_from_expr(or(K1, K22), K122), !.


knf_from_expr(or(E1,E2), or(R1, R2)) :-
	dznk(E1, 1), dznk(E2, 1),
	knf_from_expr(E1, R1), knf_from_expr(E2, R2), !. 

%%%
%and

knf_from_expr(and(E1, E2), and(R1, R2)) :- knf_from_expr(E1, R1), knf_from_expr(E2, R2), !.

%%%
%not
knf_from_expr(not(not(E)), R ) :- knf_from_expr(E, R), !.
knf_from_expr(not(and(E, E)), not(E)) :- !.
	
knf_from_expr(not(or(E1, E2)), and(R1, R2)) :- 
	knf_from_expr(not(E1), R1),
	knf_from_expr(not(E2), R2), !. 

knf_from_expr(not(and(E, E)), or(R1, R2)) :- 
	knf_from_expr(not(E1), R1),
	knf_from_expr(not(E2), R2), !.

knf_from_expr(not(and(E1, E2)), or(R1, R2)) :- 
	knf_from_expr(not(E1), R1),
	knf_from_expr(not(E2), R2), !. 

knf_from_expr(not(E), not(R)) :-
	knf_from_expr(E, R),!. 

knf_from_expr(not(1), 0) :- !.
knf_from_expr(not(0), 1) :- !.

%%%
% :NOTE: # Набор правил не приводит к КНФ, например knf_from_expr(not(and(a, not(a))), R).
knf_from_expr(1, 1) :- !. 
knf_from_expr(0, 0) :- !. 
knf_from_expr(Expr, Expr).

