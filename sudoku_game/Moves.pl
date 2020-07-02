%gets all Children of State that aren't in Open or Close
expand(State,Open,Close,Children):-
   findall(Child,expanding(State,Open,Close,Child),Children).

%get One Child from state that isn't in Open or close lists
expanding(State,OPEN,CLOSE,Child):-
   indexOfZero(State,Index),
   setValue(NV,State,Index,Child),
   isSafe(NV,Index,Child),
   not(member(Child,OPEN)),
   not(member(Child,CLOSE)).

%*****************************************************************
%for each empty slot in the table we can set 9 values from 1 to 9
%setValue(value,List,Index,newList)
setValue(1,State,Index,Child):-replace(State,Index,1,Child).
setValue(2,State,Index,Child):-replace(State,Index,2,Child).
setValue(3,State,Index,Child):-replace(State,Index,3,Child).
setValue(4,State,Index,Child):-replace(State,Index,4,Child).
setValue(5,State,Index,Child):-replace(State,Index,5,Child).
setValue(6,State,Index,Child):-replace(State,Index,6,Child).
setValue(7,State,Index,Child):-replace(State,Index,7,Child).
setValue(8,State,Index,Child):-replace(State,Index,8,Child).
setValue(9,State,Index,Child):-replace(State,Index,9,Child).

%******************************************************************
% safeState after inserting the new value
% safe state occures when there are not two slots in same row or
% col. have same value.

isSafe(Value,Index,Table):-
	checkSquare(Index,Value,Table)
   ,safeRow(Value,Index,Table)
   ,safeCol(Value,Index,Table)
   ,!.

safeRow(Value,Index,Table):-
    Row is Index//9 ,
    Start is ((Row)*9)+1,
    !,checkRow(Value,Start,Table,1,0).

safeCol(Value,Index,Table):-
    C is Index mod 9,
   (C is 0)->
   (Col is 9 ,!,
    checkCol(Value,Col,Table,1,0)
   )
   ;!,NCOL is Index mod 9,checkCol(Value,NCOL,Table,1,0).


checkRow(_,_,_,_,Count):-  Count > 1 ,! ,fail,!.
checkRow(_,_,_,10,Count):- Count < 2 , !.
checkRow(Value,Index,Table,Temp,Count):-
   Temp < 10,
   elementAt(Index,Table,V),
   incrementCount(Value,V,Count,NCount),%increment count if V==Value
   NTemp is Temp+1,
   NIndex is Index+1,
   checkRow(Value,NIndex,Table,NTemp,NCount),!.


checkCol(_,_,_,_,Count):-  Count > 1 ,!,fail,!.
checkCol(_,_,_,10,Count):- Count < 2 , !.
checkCol(Value,Index,Table,Temp,Count):-
   Temp < 10,
   elementAt(Index,Table,V),
   incrementCount(Value,V,Count,NCount),%increment count if V==Value
   NTemp is Temp+1,
   NIndex is Index+9,
   checkCol(Value,NIndex,Table,NTemp,NCount),!.

checkSquare(Index,Value,Table):-
	squareIndex(Index,Row1),
	countInSquare(Row1,Value,Table,0,0,Count1),
	Row2 is Row1 + 9,
	countInSquare(Row2,Value,Table,0,0,Count2),
	Row3 is Row2 + 9,
	countInSquare(Row3,Value,Table,0,0,Count3),
	SUM is Count1+Count2+Count3,
	SUM < 2,!.

countInSquare(_,_,_,NCount,3,NCount):-!.
countInSquare(Index,Value,Table,Count,Temp,R):-
	Temp < 3,
	elementAt(Index,Table,V),
	incrementCount(Value,V,Count,NCount),
	NIndex is Index+1,
	NTemp is Temp+1,
	countInSquare(NIndex,Value,Table,NCount,NTemp,R).

squareIndex(Index,R):-
	Row is (Index//9)+1 ,
	Col is Index mod 9,
	startIndexOfSquare(Row,Col,R).

startIndexOfSquare(Row,Col,1):-
		Row < 4, Col < 4,!.
startIndexOfSquare(Row,Col,4):-
		Row < 4, Col < 7, !.
startIndexOfSquare(Row,Col,7):-
		Row < 4, Col >= 7,!.
		
startIndexOfSquare(Row,Col,28):-
		Row < 7, Row >= 4 ,Col < 4,!.
startIndexOfSquare(Row,Col,31):-
		Row < 7, Row >= 4 ,Col < 7,!.
startIndexOfSquare(Row,Col,34):-
		Row < 7, Row >= 4 ,Col >= 7,!.
		
startIndexOfSquare(Row,Col,55):-
		Row < 10, Row >= 7 ,Col < 4,!.
startIndexOfSquare(Row,Col,58):-
		Row < 10, Row >= 7 ,Col < 7,!.
startIndexOfSquare(Row,Col,61):-
		Row < 10, Row >= 7 ,Col >= 7,!.
		
%***************************************************************		
%increment the Counter "Count" if Value is equal to V.
incrementCount(Value,V,Count,NCount):-
   (Value is V)->
    NCount is Count+1;
    NCount is Count.

%return value at "Index" in list
%elementAT(Index,List,Reslut).
elementAt(0,[H|_],H):-!.
elementAt(Index , [_|T],X):-
    Index > 0,
    NIndex is Index -1,
    elementAt(NIndex,T,X),!.

%replace(OLDLIST , Index , NewValue , NewList)
replace([_|T], 0, X, [X|T]):-!.
replace([H|T], I, X, [H|R]):-
    I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L):-!.

%Return first index of zero in a List
%indexOfZero(List,Result)
indexOfZero(L,R):-
   indexOf(0,L,R),!.

% return the index of H in a List.
indexOf(H,[H|_],0):-!.
indexOf(X,[H|T],Index):-
    indexOf(X,T,NIndex),!,
    Index is NIndex+1.

