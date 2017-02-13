
:- dynamic attemptsCount/1, goalState/1.

domain_specified_end :- attemptsCount(X), X >= 5.

num_possible_attribute_configs(1296).

% Each independent, for two relevant blocks:
% => Colour x3 each block, random, so 3^2 = 9 colour combinations
% => Size x2 each block, random, so 2^2 = 4 size combinations
% => Shape x3 each block, random, so 3^2 = 9 shape combinations
% => Magnetic x2 each block, random, so 2^2 = 4 magnetism combinations

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

derived(clear(X)) :- currentState(attr(block(X))), not(currentState(fluent(on(_,X)))).
derived(freehand(X)) :- currentState(attr(gripper(X))), not(currentState(fluent(holding(X,_)))).
derived(magnetically_attached(X)) :- currentState(attr(block(X))), currentState(attr(magnetic(X,yes))), currentState(fluent(on(X,Y))), currentState(attr(magnetic(Y,yes))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_attribute_alternatives(shape(B, cube), [shape(B, prism), shape(B, pyramid)]).
domain_attribute_alternatives(shape(B, prism), [shape(B, cube), shape(B, pyramid)]).
domain_attribute_alternatives(shape(B, pyramid), [shape(B, prism), shape(B, cube)]).
domain_attribute_alternatives(size(B, small), [size(B, big)]).
domain_attribute_alternatives(size(B, big), [size(B, small)]).
domain_attribute_alternatives(magnetic(B, yes), [magnetic(B, no)]).
domain_attribute_alternatives(magnetic(B, no), [magnetic(B, yes)]).
domain_attribute_alternatives(colour(B, red), [colour(B, blue), colour(B, green)]).
domain_attribute_alternatives(colour(B, blue), [colour(B, red), colour(B, green)]).
domain_attribute_alternatives(colour(B, green), [colour(B, blue), colour(B, red)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainGoalAction(puton(grip1,b2,b1)).

establishGoalState :-
	domainGoalAction(ACT),
	% Unexpected observations
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

applyActionToStateBW(pickup(Hand, Block)) :-
	retractall(currentState(fluent(on(Block,_)))),
	assert(currentState(fluent(holding(Hand,Block)))),
	!.
	
% failed puton
applyActionToStateBW(puton(Hand, Block1, Block2)) :-
	not(currentState(attr(shape(Block2, cube)))),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, table)))),
	!.
% failed puton
applyActionToStateBW(puton(Hand, Block1, Block2)) :-
	currentState(attr(size(Block1, big))),
	currentState(attr(size(Block2, small))),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, table)))),
	!.
	
% succeeded puton
applyActionToStateBW(puton(Hand, Block1, Block2)) :-
	currentState(attr(shape(Block2, cube))),
	currentState(fluent(holding(Hand, Block1))),
	once(( currentState(attr(size(Block1, small))) ; currentState(attr(size(Block2, big))) )),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, Block2)))),
	!.

applyActionToStateBW(putdown(Hand, Block)) :-
	retractall(currentState(fluent(holding(Hand, Block)))),
	assert(currentState(fluent(on(Block, table)))),
	!.

applyActionToStateBW(donothing) :- !.

applyActionToStateBW(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	trace.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validAction(donothing).
	
validAction(pickup(Hand, Block)) :-
	derived(clear(Block)),
	not(derived(magnetically_attached(Block))),
	derived(freehand(Hand)).

validAction(puton(Hand, Block1, Block2)) :-
	currentState(fluent(holding(Hand, Block1))),
	derived(clear(Block2)),
	currentState(attr(block(Block1))),
	currentState(attr(block(Block2))),
	Block1 \= Block2.

validAction(putdown(Hand, Block)) :-
	currentState(fluent(holding(Hand, Block))),
	currentState(attr(block(Block))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests relevant to target action

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
		attr( magnetic(b1,yes) ),
		attr( magnetic(b1,no) ),
		attr( magnetic(b2,yes) ),
		attr( magnetic(b2,no) ),
		attr( shape(b1,cube) ),
		attr( shape(b1,pyramid) ),
		attr( shape(b1,prism) ),
		attr( shape(b2,cube) ),
		attr( shape(b2,pyramid) ),
		attr( shape(b2,prism) ),
		attr( colour(b1,red) ),
		attr( colour(b1,blue) ),
		attr( colour(b1,green) ),
		attr( colour(b2,red) ),
		attr( colour(b2,blue) ),
		attr( colour(b2,green) ),
		attr( size(b1,big) ),
		attr( size(b1,small) ),
		attr( size(b2,big) ),
		attr( size(b2,small) )
	].
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resetStateAtRandom :-
	retractall(currentState(fluent(_))),
	retractall(attemptsCount(_)),
	assert(attemptsCount(0)),
	
	States = [
	[ on(b1,b2), on(b3,table), on(b2,table) ],
	[ on(b2,b1), on(b3,table), on(b1,table) ],
	[ on(b1,table), on(b2,table), on(b3,table) ],
	[ on(b3,table), on(b2,table), holding(grip1,b1) ],
	[ on(b1,table), on(b3,table), holding(grip1,b2) ]
	],
	random_member(S, States), assertFluents(S).

setRandomInitialObjectConfig :-
	retractall(currentState(attr(_))),
	Colours = [red, green, blue],
	random_member(C1, Colours),
	random_member(C2, Colours),
	random_member(C3, Colours),
	Stats = [block(b1), block(b2), block(b3), nonactionblock(b3), gripper(grip1), colour(b1,C1), colour(b2,C2), colour(b3,C3)],
	assertAtts(Stats),
	
	Shapes = [prism, pyramid, cube],
	random_member(S1, Shapes),
	random_member(S2, Shapes),
	random_member(S3, Shapes),
	ShStats = [shape(b1,S1), shape(b2,S2), shape(b3,S3)],
	assertAtts(ShStats),
	%
	MAGS = [yes, no, yes, no],
	random_member(M1, MAGS),
	random_member(M2, MAGS),
	random_member(M3, MAGS),
	MStats = [magnetic(b1,M1), magnetic(b2,M2), magnetic(b3,M3)],
	assertAtts(MStats),
	%
	Sizes = [big, small, big, small],
	random_member(I1, Sizes),
	random_member(I2, Sizes),
	random_member(I3, Sizes),
	SiStats = [size(b1,I1), size(b2,I2), size(b3,I3)],
	assertAtts(SiStats),
	!.
	
%

change_att_value(_, attr(colour(Block,Value)), Return) :-
	Colours1 = [red, green, blue],
	select(Value, Colours1, Colours2),
	random_member(NewVal, Colours2),
	Return = attr(colour(Block,NewVal)),
	!.

change_att_value(_, attr(size(Block,small)), attr(size(Block,big))) :- !.
change_att_value(_, attr(size(Block,big)), attr(size(Block,small))) :- !.

change_att_value(_, attr(magnetic(Block,yes)), attr(magnetic(Block,no))) :- !.
change_att_value(_, attr(magnetic(Block,no)), attr(magnetic(Block,yes))) :- !.

change_att_value(_, Old, attr(shape(Block,NN))) :-
	Old = attr(shape(Block,pyramid)), !,
	random_member(NN, [cube, prism]).
change_att_value(_, Old, attr(shape(Block,NN))) :-
	Old = attr(shape(Block,prism)), !,
	random_member(NN, [cube, pyramid]).
change_att_value(_, Old, attr(shape(Block,NN))) :-
	Old = attr(shape(Block,cube)), !,
	random_member(NN, [pyramid, prism]).

% For other cases, e.g., class membership may be written as object attribute because it's non-fluent
change_att_value(_ElidedList, AnythingElse, AnythingElse).

assertAtts([]).
assertAtts([A|B]) :-
	assert(currentState(attr(A))),
	assertAtts(B).
	
assertFluents([]).
assertFluents([A|B]) :-
	assert(currentState(fluent(A))),
	assertFluents(B).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainChangeObjectAtts(List) :-
	random_member(X,List), % Pick one literal to change at random
	% Note you never need to add or subtract object attribute literals
	select(X, List, ElidedList),
	change_att_value(ElidedList, X, Y), % Call a domain function making a valid change to something other than the original value
	retractall(currentState(X)),
	(Y == [] -> true ; assert(currentState(Y))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
