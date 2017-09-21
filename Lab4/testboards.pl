testBoard1([ [.,1,.,.,.,.], 
             [.,2,.,.,.,.],
             [.,x,2,1,.,.],
             [.,.,1,2,.,.],
             [.,.,.,.,1,.],
             [.,.,.,.,.,.] ]).

testBoard2([ [.,2,.,.,.,2], 
             [.,.,1,.,1,.],
             [.,.,.,1,.,.],
             [.,.,1,1,1,.],
             [.,1,.,1,.,.],
             [.,.,.,2,.,.] ]).

testBoard3([ [.,.,.,2,.,.], 
             [.,2,.,1,1,.],
             [2,1,1,1,.,.],
             [2,1,1,.,1,2],
             [.,1,.,1,.,.],
             [2,.,.,2,2,.] ]).
%     Plyr has a higher score than the other player 
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
tie(State):-
      terminal(State),
      findall(K, get(State,_,K), Boardlist),
      countPlyr1(Boardlist, 0, Value1),
      countPlyr2(Boardlist, 0, Value2),
      Value2 == Value1.


%%%%%%%%%%%%%%%%%%%%%%%%%%count the amount of bricks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
