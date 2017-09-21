test(N,M):-
!,
member(N,[1,2]),
member(M,[3,4]).

testo(N,M,K,J):-
test(N,M),
!,
test(K,J).