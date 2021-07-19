% :NOTE: проинициализировать простые/составные числа
prime(2).
prime(3).
prime(P) :- integer(P), P > 3, not(0 is mod (P, 2)), \+ has_d(P,3).  

has_d(N,L) :- 0 is N mod L.
has_d(N,L) :- L * L < N, L2 is L + 2, has_d(N,L2).

composite(N) :- not(prime(N)).


prime_divisors(1, []) :- !.
prime_divisors(N,L) :- prime_divisors(N,L,2).

prime_divisors(1,[],_) :- !.
prime_divisors(N,[F|L],F) :-               
   integer(N), 0 is N mod F, R is div(N, F), !, prime_divisors(R,L,F).
prime_divisors(N,L,F) :- 
   integer(N), next_d(N,F,NF), prime_divisors(N,L,NF).     

prime_divisors(N, L ,F) :- 
		\+(integer(N)), nums_list(N, 2, L) .

nums_list(1, _, []).
nums_list(N, Prev, [H | T]) :-
    prime(H),
    Prev =< H,
    nums_list(N1, H, T),
    N is N1 * H.

next_d(_,2,3) :- !.
next_d(N,F,NF) :- F * F < N, !, NF is F + 2.
next_d(N,_,N).                            

%gcd 

mult(1, []) :- !.
mult(R, [H | T]) :- mult(Rest, T), R is H * Rest.

delete(X,[X|T],T) :- !.
delete(X,[Y|T],[Y|T1]):-delete(X,T,T1).

gcd(A, B, GCD) :- 
	prime_divisors(A, L1), prime_divisors(B, L2),
	common(L1, L2, R), mult(GCD, R).

common([], _, [1]) :- !.
common([H | T], L2, [H | L1]) :- member(H, L2), !, delete(H, L2, L2R), common(T, L2R, L1).
common([_ | T], L2, L1) :- common(T, L2, L1).












