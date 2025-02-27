sudokuT([-1,
0,2,6,0,0,0,8,1,0,
3,0,0,7,0,8,0,0,6,
4,0,0,0,5,0,0,0,7,
0,5,0,1,0,7,0,9,0,
0,0,3,9,0,5,1,0,0,
0,4,0,3,0,2,0,5,0,
1,0,0,0,3,0,0,0,2,
5,0,0,2,0,4,0,0,9,
0,3,8,0,0,0,4,6,1]).

%****************************************************************
play():-
   sudokuT(State),
   path([State],[]). %path(Open,Close)

%****************************************************************
path([],_):-
     write('No solution'),nl,!.

path([Goal|_], _ ):-
     isGoal(Goal),
     write('A solution is found'), nl ,
     printTable(Goal),!.

path([OPH|OPT],Close):-
    expand(OPH,[OPH|OPT],Close,Children),
	append(Children,OPT,NewOpen),
    path(NewOpen,[OPH|Close]).

isGoal(Goal):-
	not(member(0,Goal)).
%******************************************************************
printTable([H|T]):-
   printT(T,1).
   
printT([],_):-!.
printT([H|T],Index):-
   Index < 82 ,
   write(H),
   printLine(Index),
   NIndex is Index+1,
   printT(T,NIndex),!.

printLine(Temp):-
   NTemp is Temp mod 9,
   ( NTemp is 0)-> nl.
printLine(T).

%***************************************************************

/*Goal
[-1,
7,2,6,4,9,3,8,1,5,
3,1,5,7,2,8,9,4,6,
4,8,9,6,5,1,2,3,7,
8,5,2,1,4,7,6,9,3,
6,7,3,9,8,5,1,2,4,
9,4,1,3,6,2,7,5,8,
1,9,4,8,3,6,5,7,2,
5,6,7,2,1,4,3,8,9,
2,3,8,5,7,9,4,6,1]
*/