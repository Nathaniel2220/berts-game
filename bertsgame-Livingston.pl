% Nathaniel Livingston

% This solution works by returning a list of 0's or 1's which correspond to 
% whether or not you should click each tile in the first row. 0 means to not click, 
% 1 means to click it. Each subsequent row is clicked based on the previous row, so you 
% click each tile that has a blue tile above it. 
% As the program progresses, blue will often correspond to 0, and red to 1. 

bertgame(Size, CorrectFP):- 
    Index is Size-1,	% Index's start at 0, so plan accordingly
    length(Zeroes, Size),	% Create a list and...
    maplist(=(0),Zeroes),	% fill it with 0's so we have a fresh, empty first row.
    options(CorrectFP,Size), % come up with a possible solution and...
    calcLastRow(Result, CorrectFP, Zeroes, Size, Index), % see what it makes the last row!
    maplist(=(1),Result).	% If the last row (Result) is all 1's you've Succeeded!


member_(A, B) :- member(B, A).		% helps make the bins function work
bins(Size, Result) :-
    findall(L, (length(L, Size), maplist(member_([0, 1]), L)), Result). % instantiates Result as a list of all possible permutations of 0's and 1's of length Size

options(This,Size):-	% This becomes just one of the possibilities bins makes at a time
    bins(Size, Result),
    nth1(_,Result,This).
    

nextRow(FP, Target, NewFP, NewTarget,Size):- % Calculates the NextRow based on a given FiringPattern and Target
    Index is Size -1,
    shoot(FP, Target, Index,Size,X),	% Shoot the Target based on the FiringPattern
    damageReport(X,Index,Y),	% Get the report on the damage you caused..
    aim(Y,Index,NewFP),		% and aim accordingly!
    NewTarget = FP.


calcLastRow(LR,NewFP,NewTarget,Size,0):- % Calculates the LastRow by using nextRow over and over
	!,		%without this everything falls apart, don't try it.
    Index is Size -1,
    shoot(NewFP,NewTarget,Index, Size, Rubble), % You have to end with a shoot...
    damageReport(Rubble, Index, LR).   % and a damageReport. Now you have your last row. 
calcLastRow(LR,F,Target,Size,Times):- % recursively uses nextRow over and over.
    nextRow(F, Target, NewFP, NewTarget, Size),
    NewTimes is Times -1,
    calcLastRow(LR,NewFP,NewTarget, Size,NewTimes).	% Recurse!
    

aim(_, -1 ,_).		 % All the tiles accounted for
aim(Target, Index, Plan):-	% Comes up with a click pattern for the next row, which will be all the tiles that are blue on the previous row
    length(Target, Size),	% Get the length of the Target
    length(Plan, Size),		% And use that length to instantiate the Plan
    (   nth0(Index, Target, 0) ->  nth0(Index, Plan, 1);nth0(Index,Plan,0)), % If there's a 1 in Target, it will be a 0 in Plan and Vice-Versa
    NewIndex is Index-1,	% Get ready to check the next tile
    aim(Target, NewIndex, Plan). % Recurse


shoot(_,_,-1,_,_). % Shoot until there's nothing left
shoot(FirePattern, Target, Index, Size, Result):-	
	length(Result, Size), % The Result will be the same size
    Left is Index -1,
    Right is Index +1,
    MaxIndex is Size-1,
    
    (   Size = 1 ->  % If size is just 1...
    (nth0(Index, Target, A),
     nth0(Index, FirePattern, B), % Add the Target to the FirePattern, and get ready for damageReport
     X is A +B)
    ;(true)
    ),
       
    (   Size \= 1, Index = MaxIndex ->     % If it's larger than 1, and it's the rightmost tile...
    (nth0(Index, Target, A),
    nth0(Index, FirePattern, B),	% add the Target to the FiringPattern directly above, and 1 to the left
    nth0(Left, FirePattern, C),
    X is A+B+C)		% Prep for damageReport
    ;(true)
    ),
    
    (	Size \= 1, Index = 0 ->  % If it's larger than 1, and it's the leftmost tile...
    (nth0(Index, Target, A),
    nth0(Index, FirePattern, B),	% add the Target to the FiringPattern directly above, and 1 to the right
    nth0(Right, FirePattern, C),
    X is A+B+C)		% Prep for damageReport
    ;(true)
    ),
    
    (   Size \= 1, Index \= 0, Index \= MaxIndex ->  % If it's larger than 1, and not the first or last tile...
    (nth0(Index, Target, A),
    nth0(Index, FirePattern, B),	% add the Target to the FiringPattern above, to the left, and to the right
    nth0(Left, FirePattern, C),
    nth0(Right, FirePattern, D),
    X is A+B+C+D) 	% Prep for damageReport
    ;(true)
    ),
    
    nth0(Index, Result, X),	% set what you just calculated to the appropriate index of Result
    NewIndex is Index -1,
    shoot(FirePattern, Target, NewIndex, Size, Result).		% and recurse!

          

damageReport(_,-1,_). 	% report until there's nothing left!
damageReport(Rubble, Index, Report):-
    length(Rubble, Size),	% The report will be the same size...
    length(Report, Size),	% as the rubble
    nth0(Index, Rubble, X),
    Y is X mod 2,	% mod the Rubble by 2 to determine whether the tile is really red or blue, AKA 1 or 0
    nth0(Index, Report, Y),	% record the answer in Report
    NewIndex is Index -1,
    damageReport(Rubble, NewIndex, Report). % Recurse!

    

    