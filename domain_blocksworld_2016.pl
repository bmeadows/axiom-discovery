:- dynamic attemptsCount/1, goalState/1.

domain_specified_end :- attemptsCount(X), X >= 5.

num_possible_static_configs(5832).

% Each independent, for three blocks:
% Colour attribute (3 choices) per block, random, so 3^3 = 27 colour combinations
% Size attribute (2 choices) per block, random, so 2^3 = 8 size combinations
% Shape attribute (3 choices) per block, random, so 3^3 = 27 shape combinations
% 
% Multiply out to get total of 5832 possible attribute configurations.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

derived(clear(X)) :- currentState(static(block(X))), not(currentState(fluent(on(_,X)))).
derived(freehand(X)) :- currentState(static(gripper(X))), not(currentState(fluent(holding(X,_)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 'Completely' specified.
% In this particular goal state, b1 is a prism/pyramid and b2 is a cube, OR b1 is small and b2 is big.
% Colours don't matter, obviously.
domainGoalAction(puton(grip1,b2,b1)).

% The goal state only gives the UNEXPECTED literals arising from the action failure.
% In this case, the unexpected consequences are that b2 is on the table, and b1 is clear.
establishGoalState :-
	domainGoalAction(ACT),
	assert(goalState([
		fluent(on(b2,table)),
		clear(b1), % derived
		lastActionWas(ACT)
		])).

:- establishGoalState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

applyActionToState(A) :-
	attemptsCount(X),
	Y is X + 1,
	retractall(attemptsCount(_)),
	assert(attemptsCount(Y)),
	applyActionToStateBW(A).

% successful pickup
applyActionToStateBW(pickup(Hand, Block)) :-
	retractall(currentState(fluent(on(Block,_)))),
	assert(currentState(fluent(holding(Hand,Block)))),
	!.
	
% failed puton
applyActionToStateBW(puton(Hand, Block1, Block2)) :-
	not(currentState(static(shape(Block2, cube)))),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, table)))),
	!.
	
% failed puton
applyActionToStateBW(puton(Hand, Block1, Block2)) :-
	currentState(static(size(Block1, big))),
	currentState(static(size(Block2, small))),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, table)))),
	!.
	
% successful puton
applyActionToStateBW(puton(Hand, Block1, Block2)) :-
	currentState(static(shape(Block2, cube))),
	currentState(fluent(holding(Hand, Block1))),
	once(( currentState(static(size(Block1, small))) ; currentState(static(size(Block2, big))) )),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, Block2)))),
	!.

% successful putdown
applyActionToStateBW(putdown(Hand, Block)) :-
	retractall(currentState(fluent(holding(Hand, Block)))),
	assert(currentState(fluent(on(Block, table)))),
	!.

applyActionToStateBW(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	trace.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
validAction(pickup(Hand, Block)) :-
	derived(clear(Block)),
	derived(freehand(Hand)).

validAction(puton(Hand, Block1, Block2)) :-
	currentState(fluent(holding(Hand, Block1))),
	derived(clear(Block2)),
	currentState(static(block(Block1))),
	currentState(static(block(Block2))),
	Block1 \= Block2.

validAction(putdown(Hand, Block)) :-
	currentState(fluent(holding(Hand, Block))),
	currentState(static(block(Block))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getAllPossibleTests(List) :-
	List = [
		fluent( on(b1,b3)),
		fluent( on(b1,table)),
		fluent( on(b1,b2)),
		fluent( on(b2,b3)),
		fluent( on(b2,table)),
		fluent( on(b2,b1)),
		fluent( holding(grip1,b3)),
		fluent( holding(grip1,b1)),
		fluent( holding(grip1,b2)),
		%fluent( clear(b1)), derived!
		%fluent( clear(b2)), derived!
		%fluent( clear(b3)), derived!
		%fluent( clear(b4)), derived!
		%fluent( freehand(grip1)), derived!
		action( pickup(grip1,b3)),
		action( pickup(grip1,b1)),
		action( pickup(grip1,b2)),
		action( putdown(grip1,b3)),
		action( putdown(grip1,b1)),
		action( putdown(grip1,b2)),
		action( puton(grip1,b2,b1)),
		action( puton(grip1,b2,b3)),
		action( puton(grip1,b1,b2)),
		action( puton(grip1,b1,b3)),
		action( puton(grip1,b3,b2)),
		action( puton(grip1,b3,b1)),
		%
		static( shape(b1,cube) ),
		static( shape(b1,pyramid) ),
		static( shape(b1,prism) ),
		static( shape(b2,cube) ),
		static( shape(b2,pyramid) ),
		static( shape(b2,prism) ),
		static( shape(b3,cube) ),
		static( shape(b3,pyramid) ),
		static( shape(b3,prism) ),
		static( colour(b1,red) ),
		static( colour(b1,blue) ),
		static( colour(b1,green) ),
		static( colour(b2,red) ),
		static( colour(b2,blue) ),
		static( colour(b2,green) ),
		static( colour(b3,red) ),
		static( colour(b3,blue) ),
		static( colour(b3,green) ),
		static( size(b1,big) ),
		static( size(b1,small) ),
		static( size(b2,big) ),
		static( size(b2,small) ),
		static( size(b3,big) ),
		static( size(b3,small) )
	].
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resetStateAtRandom :-
	retractall(currentState(fluent(_))),
	retractall(attemptsCount(_)),
	assert(attemptsCount(0)),
	
	States = [
	%
	[ on(b1,b3), on(b3,table), on(b2,table) ],
	[ on(b2,b3), on(b1,table), on(b3,table) ],
	[ on(b1,b2), on(b3,table), on(b2,table) ],
	[ on(b2,b1), on(b3,table), on(b1,table) ],
	[ on(b3,b1), on(b1,table), on(b2,table) ],
	[ on(b3,b2), on(b1,table), on(b2,table) ],
	%
	[ on(b3,b2), on(b2,b1), on(b1,table) ],
	[ on(b3,b1), on(b1,b2), on(b2,table) ],
	[ on(b2,b1), on(b1,b3), on(b3,table) ],
	[ on(b2,b3), on(b3,b1), on(b1,table) ],
	[ on(b1,b2), on(b2,b3), on(b3,table) ],
	[ on(b1,b3), on(b3,b2), on(b2,table) ],
	%
	[ on(b1,table), on(b2,table), on(b2,table) ],
	%
	[ on(b3,table), on(b2,table), holding(grip1,b1) ],
	[ on(b1,table), on(b3,table), holding(grip1,b2) ],
	[ on(b1,table), on(b2,table), holding(grip1,b3) ],
	%
	[ holding(grip1,b1), on(b3,table), on(b2,b3) ],
	[ holding(grip1,b1), on(b2,table), on(b3,b2) ],
	[ holding(grip1,b2), on(b3,table), on(b1,b3) ],
	[ holding(grip1,b2), on(b1,table), on(b3,b1) ],
	[ holding(grip1,b3), on(b1,table), on(b2,b1) ],
	[ holding(grip1,b3), on(b2,table), on(b1,b2) ]
	%
	],
	random_member(S, States), assertFluents(S).

setRandomInitialStaticConfig :-
	retractall(currentState(static(_))),
	Colours = [red, green, blue],
	random_member(C1, Colours),
	random_member(C2, Colours),
	random_member(C3, Colours),
	Stats = [block(b1), block(b2), block(b3), gripper(grip1), colour(b1,C1), colour(b2,C2), colour(b3,C3)],
	assertStatics(Stats),
	Shapes = [prism, pyramid, cube],
	random_member(S1, Shapes),
	random_member(S2, Shapes),
	random_member(S3, Shapes),
	ShStats = [shape(b1,S1), shape(b2,S2), shape(b3,S3)],
	assertStatics(ShStats),
	%
	Sizes = [big, small, big, small],
	random_member(I1, Sizes),
	random_member(I2, Sizes),
	random_member(I3, Sizes),
	SiStats = [size(b1,I1), size(b2,I2), size(b3,I3)],
	assertStatics(SiStats),
	!.

% The following code block would be used if shapes were determined without replacement, i.e., at least one of each shape was required (for four blocks).
/*
	ShapeAlts = [
		[shape(b1,prism), shape(b2,prism), shape(b3,cube), shape(b4,cube)],
		[shape(b1,cube), shape(b2,prism), shape(b3,prism), shape(b4,cube)],
		[shape(b1,cube), shape(b2,cube), shape(b3,prism), shape(b4,prism)],
		[shape(b1,prism), shape(b2,cube), shape(b3,cube), shape(b4,prism)],
		[shape(b1,prism), shape(b2,cube), shape(b3,prism), shape(b4,cube)],
		[shape(b1,cube), shape(b2,prism), shape(b3,cube), shape(b4,prism)],
		
		[shape(b1,cube), shape(b2,cube), shape(b3,cube), shape(b4,prism)],
		[shape(b1,cube), shape(b2,cube), shape(b3,prism), shape(b4,cube)],
		[shape(b1,cube), shape(b2,prism), shape(b3,cube), shape(b4,cube)],
		[shape(b1,prism), shape(b2,cube), shape(b3,cube), shape(b4,cube)],
		
		[shape(b1,prism), shape(b2,prism), shape(b3,prism), shape(b4,cube)],
		[shape(b1,prism), shape(b2,prism), shape(b3,cube), shape(b4,prism)],
		[shape(b1,prism), shape(b2,cube), shape(b3,prism), shape(b4,prism)],
		[shape(b1,cube), shape(b2,prism), shape(b3,prism), shape(b4,prism)]
		],*/
		/*
	X = [prism, pyramid, cube],
	findall(
		NN,
		(NN = [shape(b1,A),shape(b2,B),shape(b3,C),shape(b4,D)], member(A,X), member(B,X), member(C,X), member(D,X),
			(A == prism ; B == prism ; C == prism ; D == prism), (A == cube ; B == cube ; C == cube ; D == cube), (A == pyramid ; B == pyramid ; C == pyramid ; D == pyramid) ),
		ShapeAlts),
	random_member(ShapeStats, ShapeAlts),
	assertStatics(ShapeStats),
*/	

%

change_static_value(_, static(colour(Block,Value)), Return) :-
	Colours1 = [red, green, blue],
	select(Value, Colours1, Colours2),
	random_member(NewVal, Colours2),
	Return = static(colour(Block,NewVal)),
	!.

change_static_value(_, static(size(Block,small)), static(size(Block,big))) :- !.
change_static_value(_, static(size(Block,big)), static(size(Block,small))) :- !.

change_static_value(_, Old, static(shape(Block,NN))) :-
	Old = static(shape(Block,pyramid)), !,
	random_member(NN, [cube, prism]).
change_static_value(_, Old, static(shape(Block,NN))) :-
	Old = static(shape(Block,prism)), !,
	random_member(NN, [cube, pyramid]).
change_static_value(_, Old, static(shape(Block,NN))) :-
	Old = static(shape(Block,cube)), !,
	random_member(NN, [pyramid, prism]).

% For other, fixed statics like 'block(b1)' and 'gripper(grip1)'.
change_static_value(_ElidedList, AnythingElse, AnythingElse).
	
assertStatics([]).
assertStatics([A|B]) :-
	assert(currentState(static(A))),
	assertStatics(B).
	
assertFluents([]).
assertFluents([A|B]) :-
	assert(currentState(fluent(A))),
	assertFluents(B).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainChangeStatics(List) :-
	random_member(X,List), % Pick one literal to change at random
	% Note you never need to add or subtract static literals.
	select(X, List, ElidedList),
	change_static_value(ElidedList, X, Y), % Call a domain function making a valid change to something other than the original value
	retractall(currentState(X)),
	(Y == [] -> true ; assert(currentState(Y))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In time, the goal itself may be reset, for broader learning.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_attribute_alternatives(shape(B, cube), [shape(B, prism), shape(B, pyramid)]).
domain_attribute_alternatives(shape(B, prism), [shape(B, cube), shape(B, pyramid)]).
domain_attribute_alternatives(shape(B, pyramid), [shape(B, prism), shape(B, cube)]).
domain_attribute_alternatives(size(B, small), [size(B, big)]).
domain_attribute_alternatives(size(B, big), [size(B, small)]).
domain_attribute_alternatives(colour(B, red), [colour(B, blue), colour(B, green)]).
domain_attribute_alternatives(colour(B, blue), [colour(B, red), colour(B, green)]).
domain_attribute_alternatives(colour(B, green), [colour(B, blue), colour(B, red)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

