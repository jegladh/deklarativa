%Author: Jesper Gladh
%Solution based on farmer1.pl from the example code
%%steelkey brasskey box robot hand hand
%
invalid(state(X,_,_,Y,_,_)) :- %Robot i rum2 utan steelkey
	X \== r2,
	Y == r2.
invalid(state(X,_,_,Y,_,_)) :- %robot i rum1 utan steelkey
	X \== r1,
	Y == r1.
invalid(state(_,X,_,Y,_,_)) :- %Går in i rum3 utan brassKey
	X \== r3,
	Y == r3.
invalid(state(X,_,_,_,_,_)) :- %steelkey i rum 3
	X == r3.

invalid(state(_,X,_,Y,_,_)) :- %går ut ur rum 3 utan brassKey
	Y == r1,
	X == r3.

newmove(r1, Move) :-
	Move = r2.
newmove(r1, Move) :-
	Move = r3.
newmove(r2, Move) :-
	Move = r1.
newmove(r3, Move) :-
	Move = r1.
   /* newmove sätter move till ett rum */

%checking hands solutions fick jag hjälp 
%av Dennis peersson att göra

%Roboten har "2 händer" och denna funktion gör
%att var föremålet ligger inte spelar någon roll

checkhands(Lhand, Rhand, Atom1, Atom2):-
	(Lhand == Atom1, Rhand == Atom2);
	(Rhand == Atom1, Lhand == Atom2).	

%funktioner för att flytta roboten och föremål till nytt rum 
move(state(X, BrassKey, Box, X, Lhand, Rhand), State, TheMove):-
	State = state(Y, BrassKey, Box, Y, Lhand, Rhand),
	newmove(X, Y),
	checkhands(Lhand, Rhand, steelKey, empty),
	TheMove = [move(Y)].

move(state(SteelKey, X, Box, X, Lhand, Rhand), State, TheMove):-
	State = state(SteelKey, Y, Box, Y, Lhand, Rhand),
	newmove(X, Y),
	checkhands(Lhand, Rhand, brassKey, empty),
	TheMove = [move(Y)].

move(state(SteelKey, BrassKey, X, X, Lhand, Rhand), State, TheMove):-
	State = state(SteelKey, BrassKey, Y, Y, Lhand, Rhand),
	newmove(X, Y),
	checkhands(Lhand, Rhand, box, empty),
	TheMove = [move(Y)].

move(state(X, X, Box, X, Lhand, Rhand), State, TheMove):-
	State = state(Y, Y, Box, Y, Lhand, Rhand),
	newmove(X, Y),
	checkhands(Lhand, Rhand, steelKey, brassKey),
	TheMove = [move(Y)].

move(state(SteelKey, X, X, X, Lhand, Rhand), State, TheMove):-
	State = state(SteelKey, Y, Y, Y, Lhand, Rhand),
	newmove(X, Y),
	checkhands(Lhand, Rhand, brassKey, box),
	TheMove = [move(Y)].

move(state(X, BrassKey, X, X, Lhand, Rhand), State, TheMove):-
	State = state(Y, BrassKey, Y, Y, Lhand, Rhand),
	newmove(X, Y),
	checkhands(Lhand, Rhand, steelKey, box),
	TheMove = [move(Y)].




%funktioner för att plocka upp eller släppa föremål
move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	Lhand == empty,
	not(Rhand == steelKey),
	Robot == SteelKey,
	State = state(SteelKey, BrassKey, Box, Robot, steelKey, Rhand),
	TheMove = [picked_up_steelKey].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	Rhand == empty,
	not(Lhand == steelKey),
	Robot == SteelKey,
	State = state(SteelKey, BrassKey, Box, Robot, Lhand, steelKey),
	TheMove = [picked_up_steelKey].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	Rhand == empty,
	not(Lhand == brassKey),
	Robot == BrassKey,
	State = state(SteelKey, BrassKey, Box, Robot, Lhand, brassKey),
	TheMove = [picked_up_brassKey].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	Lhand == empty,
	not(Rhand == brassKey),
	Robot == BrassKey,
	State = state(SteelKey, BrassKey, Box, Robot, brassKey,Rhand),
	TheMove = [picked_up_brassKey].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	Lhand == empty,
	not(Rhand == box),
	Robot == Box, 
	State = state(SteelKey, BrassKey, Box, Robot, box, Rhand),
	TheMove = [picked_up_box].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	Rhand == empty,
	not(Lhand == box),
	Robot == Box,
	State = state(SteelKey, BrassKey, Box, Robot, Lhand, box),
	TheMove = [picked_up_box].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	not(Rhand == empty),
	State = state(SteelKey, BrassKey, Box, Robot, Lhand, empty),
	TheMove = [dropped_right(Rhand)].

move(state(SteelKey, BrassKey, Box, Robot, Lhand, Rhand), State, TheMove):-
	not(Lhand == empty),
	State = state(SteelKey, BrassKey, Box, Robot,empty ,Rhand),
	TheMove = [dropped_left(Lhand)].


%Roboten måste vara i rum2 med steelKey och låda = win!
solve(state(Dest, _, Dest, Dest, Lhand, Rhand), Dest, _, []) :-
	(Dest == r2, Lhand == empty, not(Rhand == box));
	(Dest == r2, Rhand == empty, not(Lhand == box)).

solve(State, Dest, N, Trace) :-
    N > 0,
    move(State, NewState, [A]),
    not(invalid(NewState)),
    solve(NewState, Dest, N-1, TraceCo),
    Trace = [(A) | TraceCo].

%steelKey, BrassKey, Box, Robot
init(state(r1,r2,r3,r1,empty,empty)).
start(T) :- init(X), solve(X, r2, 12, T).





