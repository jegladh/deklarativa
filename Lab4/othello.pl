/* ----------------------------------------------------------
Solutions for this lab has been discussed with Dennis Persson,
Claes Andersson, Erik Karlsson and Lousie Olofsson

% Surname: Glad
% First Name: JEsper


  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%           initialize(InitialState,InitialPlyr).
%          winner(State,Plyr) 
%          tie(State)
%          terminal(State) 
%           moves(Plyr,State,MvList)
%           nextState(Plyr,Move,State,NewState,NextPlyr)
%           validmove(Plyr,State,Proposed)
%           h(State,Val)  (see question 2 in the handout)
%           lowerBound(B)
%           upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 



% given helper: Inital state of the board 
initBoard([ [.,.,.,.,.,.], [.,.,.,.,.,.],[.,.,1,2,.,.], [.,.,2,1,.,.], [.,.,.,.,.,.], [.,.,.,.,.,.] ]).

testBoard1([ [.,1,.,.,.,.], 
             [.,2,.,.,.,.],
             [.,.,2,1,.,.],
             [.,.,1,2,.,.],
             [.,.,.,.,1,.],
             [.,.,.,.,.,.] ]).
testBoardW([ [1,1,1,1,2,1],
			 [2,1,1,2,1,1],
			 [2,2,1,2,1,1],
			 [2,2,2,1,2,1],
			 [2,1,2,2,1,1],
			 [1,1,1,1,1,1] ]).

testBoard3([ [.,2,2,2,2,1], 
             [2,.,.,.,.,.],
             [2,.,.,.,.,.],
             [2,.,.,.,.,.],
             [2,.,.,.,.,.],
             [1,.,.,.,.,.] ]).

%Check CPU win condition.
%initBoard([[2,2,2,2,1,2],[2,2,2,2,1,2],[2,2,2,2,1,2],[2,2,2,2,1,2],[2,2,2,2,1,2],[1,1,1,1,1,.]]).
%Check both have to n.
%initBoard([[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,2,2,.,.],[.,.,2,2,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]]).
%Check Tie condition.
%initBoard([[1,2,2,1,1,1],[2,2,2,2,2,2],[2,2,2,2,2,2],[2,2,2,2,2,2],[2,2,2,2,2,2],[1,2,2,2,2,.]]).
%Check Win condition.
%initBoard([[1,1,1,1,2,1],[1,1,1,1,2,1],[1,1,1,1,2,1],[1,1,1,1,2,1],[1,1,1,1,2,1],[2,2,2,2,2,.]]).

%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

/*Uses "initBoard" as startboard and player 1 starts */
initialize(InitialState,InitialPlyr) :-
	initBoard(InitialState),
	InitialPlyr = 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
%Checks if terminal state and then counts bricks for each player
%The player with least amonut of bricks wins
winner(State,Plyr):-
	terminal(State),
	findall(K, get(State,_,K), Boardlist),
	countPlyr1(Boardlist, 0, Value1),
	countPlyr2(Boardlist, 0, Value2),
	Value1 \== Value2,
	(Value2 < Value1 -> Plyr = 2;
	Plyr = 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
%checks if terminal state and then counts each players bricks
%if brick count is the same its a draw
tie(State):-
	terminal(State),
	findall(K, get(State,_,K), Boardlist),
	countPlyr1(Boardlist, 0, Value1),
	countPlyr2(Boardlist, 0, Value2),
	Value2 == Value1.


%%%%%%%%%%%%%%%%%%%%%%%%%%count the amount of bricks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Count was done with Vikhram and dennis%
%will get a list from functions tie and winner, this list is the state of the board
%check the board for 1's and 2's and returns the amount to tie and winner so they can compare
countPlyr1([], NewValue, NewValue).
countPlyr1([H|T], Value, Result):-
	(H == 1 ->
	NewValue is Value+1,
	countPlyr1(T,NewValue,Result);
	countPlyr1(T,Value,Result)).

countPlyr2([], NewValue, NewValue).
countPlyr2([H|T], Value, Result):-
	(H == 2 -> 
	NewValue is Value+1,
	countPlyr2(T,NewValue,Result);
	countPlyr2(T,Value,Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal 
%Finds all valid moves for both players in [X Y ] form.
%Counts the amount of valid moves and if == 0 for both we have a terminal state  
terminal(State) :-
	findall([X, Y], validmove(1 ,State,[X, Y], Direction), AllMoves1),
	length(AllMoves1, LenMoves1),
	LenMoves1 == 0,
	findall([X, Y], validmove(2,State,[X, Y], Direction), AllMoves2),
	length(AllMoves2, LenMoves2),
	LenMoves2 == 0.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%Worked on moves with Erik Karlsson
%finds all valid moves on the form of [X Y]. 
%then the list is sorted and after that we check for the lenght
%and if the lenght is zero the only valid move is pass (n)
moves(Plyr, State, MvList):-
	findall([X, Y], validmove(Plyr,State,[X, Y], Direction),
	AllMoves),
	sort(AllMoves, AllSortedMoves), 
	length(AllMoves, LenMoves),
	(LenMoves == 0 -> MvList = [n];
	MvList = AllSortedMoves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%nextState(1,[2,3]
%[.,.,.,.,.,2],
%[1,.,2,.,2,.],
%[.,1,2,2,.,.],
%[1,1,.,2,2,2],
%[.,1,2,1,.,.],
%[1,.,2,.,1,.]], _, 2).  Will return false since 2,3 is illegal
% set( Board, NewBoard, [X, Y], Value) 

/*directionList(Plyr, State, Move, Dir):-
	not(checkEast(Plyr, State, Move)),
	Dir = east;
	not(checkWest(Plyr, State, Move)),
	Dir = west;
	not(checkNorth(Plyr, State, Move)),
	Dir = north;
	not(checkSouth(Plyr, State, Move)),
	Dir = south;
	not(checkNorthEast(Plyr, State, Move)),
	Dir = northeast;
	not(checkNorthWest(Plyr, State, Move)),
	Dir = northwest;
	not(checkSouthEast(Plyr, State, Move)),
	Dir = southeast;
	not(checkSouthWest(Plyr, State, Move)),
	Dir = southwest
*/
%Case if one player passes, we just change player
nextState(Plyr, n, State, State, NextPlyr, AllDir):-
	(Plyr == 1 -> NextPlyr is 2;
	NextPlyr is 1).

%first finds all directions we wanna flip in and then calls
%reversi which is our recursive flip function
%when all flips are done we set the brick that was propesd and chenge player
nextState(Plyr, Move, State, NewState1, NextPlyr, AllDir):-
findall(Direction, validmove(Plyr, State, Move, Direction), AllDir),
 reversi(Plyr, State, AllDir, Move, NewState),

 set(NewState, NewState1, Move, Plyr),
 (Plyr == 1 -> NextPlyr is 2;
 NextPlyr is 1).

%BAse case if only one flip direction is left
reversi(Plyr, State, [Head|[]], Move, NewState):-
	flip(Plyr,State, Head, NewState, Move).

%calls flip functions with the list of legal flips found in the nextState function
%flips thedirection that comes first in the list and then rucursivky
%calls reversi again with the head removed so we work through every direction
reversi(Plyr, State, [Head|RestDir], Move, NewState):-
	flip(Plyr,State, Head, MidState, Move),
	reversi(Plyr, MidState, RestDir, Move, NewState).


%Flip functions, checks player and if its the opponent
%we uppdate the board and keep going until we find our own brick
%since we have called validmove before we know that we can flip
%every enemy brick in the given direction until we meet a firendly brick
flip(Plyr, State, east, EndState, [X, Y]):-
	NewX is X+1,
	get(State, [NewX, Y], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [NewX, Y], Plyr),
	flip(Plyr, MidState, east, EndState, [NewX, Y])).


flip(Plyr, State, west, EndState, [X, Y]):-
	NewX is X-1,
	get(State, [NewX, Y], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [NewX, Y], Plyr),
	flip(Plyr, MidState, west, EndState, [NewX, Y])).

flip(Plyr, State, north, EndState, [X, Y]):-
	NewY is Y-1,
	get(State, [X, NewY], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [X, NewY], Plyr),
	flip(Plyr, MidState, north, EndState ,[X, NewY])).


flip(Plyr, State, south, EndState, [X, Y]):-
	NewY is Y+1,
	get(State, [X, NewY], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [X, NewY], Plyr),
	flip(Plyr, MidState, south, EndState, [X, NewY])).

flip(Plyr, State, northeast, EndState, [X, Y]):-
	NewX is X+1,
	NewY is Y-1,
	get(State, [NewX, NewY], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [NewX, NewY], Plyr),
	flip(Plyr, MidState, northeast, EndState, [NewX, NewY])).

flip(Plyr, State, northwest, EndState, [X, Y]):-
	NewX is X-1,
	NewY is Y-1,
	get(State, [NewX, NewY], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [NewX, NewY], Plyr),
	flip(Plyr, MidState,northwest, EndState, [NewX, NewY])).

flip(Plyr, State, southeast, EndState, [X, Y]):-
	NewX is X+1,
	NewY is Y+1,
	get(State, [NewX, NewY], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [NewX, NewY], Plyr),
	flip(Plyr, MidState,southeast, EndState, [NewX, NewY])).

flip(Plyr, State, southwest, EndState, [X, Y]):-
	NewX is X-1,
	NewY is Y+1,
	get(State, [NewX, NewY], Value),
	(Value == Plyr -> true,
	EndState = State
	;
	set(State, MidState, [NewX, NewY], Plyr),
	flip(Plyr, MidState,southwest ,EndState, [NewX, NewY])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

%Solutions for validmove discussed with Louise Olofsson, Dennis Persson%

%If no valid moves player can pass on "n")
validmove(Plyr,State, n , _):-
	moves(Plyr, State, MvList),
	write(MvList),
	MvList == [n].
 
%

%Check if can place and if right is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewX is X+1,
	get(State, [NewX, Y], Value),
	not(isEmpty(_,State, [NewX, Y])),
	Value \== Plyr,
	not(checkEast(Plyr, State,[NewX, Y]))	,
	Direction = east.

%Check if can place and if left is legal%
validmove(Plyr,State,[X, Y], Direction ) :-
	isEmpty(Plyr, State, [X, Y]),
	NewX is X-1,
	get(State, [NewX, Y], Value),
	not(isEmpty(_,State, [NewX, Y])),
	Value \== Plyr,
	not(checkWest(Plyr, State,[NewX, Y]))	,
	Direction = west.
%Check if can place and if up is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewY is Y-1,
	get(State, [X, NewY], Value),
	not(isEmpty(_,State, [X, NewY])),
	Value \== Plyr,
	not(checkNorth(Plyr, State,[X, NewY]))	,
	Direction = north.
%Check if can place and if down is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewY is Y+1,
	get(State, [X, NewY], Value),
	not(isEmpty(_,State, [X, NewY])),
	Value \== Plyr,
	not(checkSouth(Plyr, State,[X, NewY]))	,
	Direction = south.

%Check if can place and if northeast is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewX is X+1,
	NewY is Y-1,
	get(State, [NewX, NewY], Value),
	not(isEmpty(_,State, [NewX, NewY])),
	Value \== Plyr,
	not(checkNorthEast(Plyr, State,[NewX, NewY]))	,
	Direction = northeast.

%Check if can place and if north west is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewX is X-1,
	NewY is Y-1,
	get(State, [NewX, NewY], Value),
	not(isEmpty(_,State, [NewX, NewY])),
	Value \== Plyr,
	not(checkNorthWest(Plyr, State,[NewX, NewY]))	,
	Direction = northwest.

%Check if can place and if south east is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewX is X+1,
	NewY is Y+1,
	get(State, [NewX, NewY], Value),
	not(isEmpty(_,State, [NewX, NewY])),
	Value \== Plyr,
	not(checkSouthEast(Plyr, State,[NewX, NewY]))	,
	Direction = southeast.

%Check if can place and if south west is legal%
validmove(Plyr,State,[X, Y], Direction) :-
	isEmpty(Plyr, State, [X, Y]),
	NewX is X-1,
	NewY is Y+1,
	get(State, [NewX, NewY], Value),
	not(isEmpty(_,State, [NewX, NewY])),
	Value \== Plyr,
	not(checkSouthWest(Plyr, State,[NewX, NewY])),
	Direction = southwest.

%the recursion step, will keep checking in the direction until
%a firendly brick is found
checkEast(_,_,[5,_]).
checkEast(Plyr,State,[X, Y]) :-
	NewX is X+1,
	get(State, [NewX, Y], Value),
	(isEmpty(_, State, [NewX, Y]) -> true;
	Value \== Plyr,
	checkEast(Plyr, State, [NewX, Y])).


checkWest(_,_,[0,_]).
checkWest(Plyr,State,[X, Y]) :-
	NewX is X-1,
	get(State, [NewX, Y], Value),
	(isEmpty(_, State, [NewX, Y]) -> true;
	Value \== Plyr,
	checkWest(Plyr, State, [NewX, Y])).

checkNorth(_,_,[_,0]).
checkNorth(Plyr,State,[X, Y]) :-
	NewY is Y-1,
	get(State, [X, NewY], Value),
	(isEmpty(_, State, [X, NewY]) -> true;
	Value \== Plyr,
	checkNorth(Plyr, State, [X, NewY])).

checkSouth(_,_,[_,5]).
checkSouth(Plyr,State,[X, Y]) :-
	NewY is Y+1,
	get(State, [X, NewY], Value),
	(isEmpty(_, State, [X, NewY]) -> true;
	Value \== Plyr,
	checkSouth(Plyr, State, [X, NewY])).

%The diagonal check have 2 base cases so we never go out of bounds
checkNorthEast(_,_,[_,0]).
checkNorthEast(_,_,[5,_]).
checkNorthEast(Plyr,State,[X, Y]) :-
	NewX is X+1,
	NewY is Y-1,
	get(State, [NewX, NewY], Value),
	(isEmpty(_, State, [NewX, NewY]) -> true;
	Value \== Plyr,
	checkNorthEast(Plyr, State, [NewX, NewY])).

checkNorthWest(_,_,[_,0]).
checkNorthWest(_,_,[0,_]).
checkNorthWest(Plyr,State,[X, Y]) :-
	NewX is X-1,
	NewY is Y-1,
	get(State, [NewX, NewY], Value),
	(isEmpty(_, State, [NewX, NewY]) -> true;
	Value \== Plyr,
	checkNorthWest(Plyr, State, [NewX, NewY])).

checkSouthEast(_,_,[_,5]).
checkSouthEast(_,_,[5,_]).
checkSouthEast(Plyr,State,[X, Y]) :-
	NewX is X+1,
	NewY is Y+1,
	get(State, [NewX, NewY], Value),
	(isEmpty(_, State, [NewX, NewY]) -> true;
	Value \== Plyr,
	checkSouthEast(Plyr, State, [NewX, NewY])).

checkSouthWest(_,_,[_,5]).
checkSouthWest(_,_,[0,_]).
checkSouthWest(Plyr,State,[X, Y]) :-
	NewX is X-1,
	NewY is Y+1,
	get(State, [NewX, NewY], Value),
	(isEmpty(_, State, [NewX, NewY]) -> true;
	Value \== Plyr,
	checkSouthWest(Plyr, State, [NewX, NewY])).

%Is empty was done since it was annoying to type this in every function
%Idea was from Lousie Olofsson
isEmpty(_, State, [X, Y]):-
	get(State, [X, Y], Value),
	Value == '.'.








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, 1) :-
	winner(State, 1), !.
h(State, -1) :-
	winner(State,2), !.
h(State, 0) :-
	tie(State), !.
h(_,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1), set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value)
    :- setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :- 
	Y > 0, 
	Y1 is Y-1, 
	set( RestRows, NewRestRows, [X, Y1], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
