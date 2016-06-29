
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

derived(clear(X)) :- static(block(X)), not(currentState(fluent(on(_,X)))).
derived(freehand(X)) :- static(gripper(X)), not(currentState(fluent(holding(X,_)))).

static(block(redtriangle)).
static(colour(redtriangle,red)).
static(shape(redtriangle,triangle)).
static(block(greentriangle)).
static(colour(greentriangle,green)).
static(shape(greentriangle,triangle)).
static(block(bluecube)).
static(colour(bluecube,blue)).
static(shape(bluecube,cube)).
static(block(yellowcube)).
static(colour(yellowcube,yellow)).
static(shape(yellowcube,cube)).
static(gripper(hand1)).

goalState([
	freehand(hand1), % derived
	%fluent(on(redtriangle,table)), fluent(on(greentriangle,table)),
	clear(redtriangle), % derived
	clear(greentriangle), % derived
	lastActionWas(puton(hand1,Bl,Tr)),
	block(Bl), % static
	shape(Tr,triangle) % static
	%fluent(on(bluecube,table)), clear(bluecube),
	%fluent(on(yellowcube,table)), clear(yellowcube)
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


applyActionToState(pickup(Hand, Block)) :-
	retractall(currentState(fluent(on(Block,_)))),
	assert(currentState(fluent(holding(Hand,Block)))),
	!.

% succeeded puton
applyActionToState(puton(Hand, Block1, Block2)) :-
	static(shape(Block2, cube)),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, Block2)))),
	!.

% failed puton
applyActionToState(puton(Hand, Block1, Block2)) :-
	not(static(shape(Block2, cube))),
	retractall(currentState(fluent(holding(Hand, Block1)))),
	assert(currentState(fluent(on(Block1, table)))),
	!.
	
applyActionToState(putdown(Hand, Block)) :-
	retractall(currentState(fluent(holding(Hand, Block)))),
	assert(currentState(fluent(on(Block, table)))),
	!.

applyActionToState(Something) :-
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
	static(block(Block1)),
	static(block(Block2)),
	Block1 \= Block2.

validAction(putdown(Hand, Block)) :-
	currentState(fluent(holding(Hand, Block))),
	static(block(Block)).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getAllPossibleTests(List) :-
	List = [
		fluent( on(redtriangle,bluecube)),
		fluent( on(redtriangle,yellowcube)),
		fluent( on(redtriangle,table)),
		fluent( on(redtriangle,greentriangle)),
		fluent( on(greentriangle,bluecube)),
		fluent( on(greentriangle,yellowcube)),
		fluent( on(greentriangle,table)),
		fluent( on(greentriangle,redtriangle)),
		fluent( holding(hand1,bluecube)),
		fluent( holding(hand1,yellowcube)),
		fluent( holding(hand1,redtriangle)),
		fluent( holding(hand1,greentriangle)),
		fluent( clear(redtriangle)),
		fluent( clear(greentriangle)),
		fluent( clear(bluecube)),
		fluent( clear(yellowcube)),
		fluent( freehand(hand1)),
		action( pickup(hand1,bluecube)),
		action( pickup(hand1,yellowcube)),
		action( pickup(hand1,redtriangle)),
		action( pickup(hand1,greentriangle)),
		action( putdown(hand1,bluecube)),
		action( putdown(hand1,yellowcube)),
		action( putdown(hand1,redtriangle)),
		action( putdown(hand1,greentriangle)),
		action( puton(hand1,greentriangle,redtriangle)),
		action( puton(hand1,greentriangle,bluecube)),
		action( puton(hand1,greentriangle,yellowcube)),
		action( puton(hand1,redtriangle,greentriangle)),
		action( puton(hand1,redtriangle,bluecube)),
		action( puton(hand1,redtriangle,yellowcube)),
		action( puton(hand1,bluecube,yellowcube)),
		action( puton(hand1,bluecube,greentriangle)),
		action( puton(hand1,bluecube,redtriangle)),
		action( puton(hand1,yellowcube,bluecube)),
		action( puton(hand1,yellowcube,greentriangle)),
		action( puton(hand1,yellowcube,redtriangle))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resetStateAtRandom :-
	retractall(currentState(_)),
	% Nine+three cases.
	% red on blue on yellow
	% red on yellow on blue
	% red on blue, yellow on table
	% red on yellow, blue on table
	% red on blue, yellow in hand
	% red on yellow, blue in hand
	% red, yellow, blue
	% red, yellow, blue in hand
	% red, blue, yellow in hand
	% >
	% blue, yellow, red in hand
	% blue on yellow, red in hand
	% yellow on blue, red in hand
	%
	% multiply by all possible places for the greentriangle...
	States = [
	%
	[ on(redtriangle,bluecube), on(bluecube,yellowcube), on(yellowcube,table), on(greentriangle,table) ],
	[ on(redtriangle,bluecube), on(bluecube,yellowcube), on(yellowcube,table), holding(hand1,greentriangle) ],
	%
	[ on(redtriangle,yellowcube), on(yellowcube,bluecube), on(bluecube,table), on(greentriangle,table) ],
	[ on(redtriangle,yellowcube), on(yellowcube,bluecube), on(bluecube,table), holding(hand1,greentriangle) ],
	%
	[ on(redtriangle,bluecube), on(bluecube,table), on(yellowcube,table), on(greentriangle,yellowcube) ],
	[ on(redtriangle,bluecube), on(bluecube,table), on(yellowcube,table), on(greentriangle,table) ],
	[ on(redtriangle,bluecube), on(bluecube,table), on(yellowcube,table), holding(hand1,greentriangle) ],
	%
	[ on(redtriangle,yellowcube), on(yellowcube,table), on(bluecube,table), on(greentriangle,bluecube) ],
	[ on(redtriangle,yellowcube), on(yellowcube,table), on(bluecube,table), on(greentriangle,table) ],
	[ on(redtriangle,yellowcube), on(yellowcube,table), on(bluecube,table), holding(hand1,greentriangle) ],
	%
	[ holding(hand1,yellowcube), on(redtriangle,bluecube), on(bluecube,table), on(greentriangle,table) ],
	%
	[ holding(hand1,bluecube), on(redtriangle,yellowcube), on(yellowcube,table), on(greentriangle,table) ],
	%
	[ on(redtriangle,table), on(bluecube,table), on(yellowcube,table), on(greentriangle,bluecube) ],
	[ on(redtriangle,table), on(bluecube,table), on(yellowcube,table), on(greentriangle,yellowcube) ],
	[ on(redtriangle,table), on(bluecube,table), on(yellowcube,table), on(greentriangle,table) ],
	[ on(redtriangle,table), on(bluecube,table), on(yellowcube,table), holding(hand1,greentriangle) ],
	%
	[ holding(hand1,bluecube), on(redtriangle,table), on(yellowcube,table), on(greentriangle,yellowcube) ],
	[ holding(hand1,bluecube), on(redtriangle,table), on(yellowcube,table), on(greentriangle,table) ],
	%
	[ holding(hand1,yellowcube), on(redtriangle,table), on(bluecube,table), on(greentriangle,bluecube) ],
	[ holding(hand1,yellowcube), on(redtriangle,table), on(bluecube,table), on(greentriangle,table) ],
	%
	[ holding(hand1,redtriangle), on(bluecube,table), on(yellowcube,table), on(greentriangle,bluecube) ],
	[ holding(hand1,redtriangle), on(bluecube,table), on(yellowcube,table), on(greentriangle,yellowcube) ],
	[ holding(hand1,redtriangle), on(bluecube,table), on(yellowcube,table), on(greentriangle,table) ],
	%
	[ holding(hand1,redtriangle), on(bluecube,table), on(yellowcube,bluecube), on(greentriangle,yellowcube) ],
	[ holding(hand1,redtriangle), on(bluecube,table), on(yellowcube,bluecube), on(greentriangle,table) ],
	%
	[ holding(hand1,redtriangle), on(yellowcube,table), on(bluecube,yellowcube), on(greentriangle,bluecube) ],
	[ holding(hand1,redtriangle), on(yellowcube,table), on(bluecube,yellowcube), on(greentriangle,table) ]
	%
	],
	random_member(S, States),
	assertFluents(S).
	
assertFluents([]).
assertFluents([A|B]) :-
	assert(currentState(fluent(A))),
	assertFluents(B).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Not used yet
% This is not an enumeration of all states
% It's weighted because of identical states resulting from different actions
% Revisit
resetGoalAtRandom :-
	retractall(goalState(_)),
	% Cases:
	% 1. blue fell off red; yellow and green separate ... looks identical to
	%		'yellow fell off red; blue and green separate'
	% 3. blue fell off red which is stacked on yellow; green separate
	% 4. yellow fell off red which is stacked on blue; green separate
	% 5. blue fell off red; green on yellow
	% 6. yellow fell off red; green on blue
	% Note many of these look the same as if things fell off green!! Or were just put down!!
	A = [
		freehand(hand1),
		fluent(on(redtriangle,table)), clear(redtriangle),
		fluent(on(bluecube,table)), clear(bluecube),
		fluent(on(yellowcube,table)), clear(yellowcube),
		fluent(on(greentriangle,table)), clear(greentriangle)
	],
	GoalStates = [
	A,
	A,
	[
		freehand(hand1),
		fluent(on(redtriangle,bluecube)), clear(redtriangle),
		fluent(on(bluecube,table)),
		fluent(on(yellowcube,table)), clear(yellowcube),
		fluent(on(greentriangle,table)), clear(greentriangle)
	],
	[
		freehand(hand1),
		fluent(on(redtriangle,yellowcube)), clear(redtriangle),
		fluent(on(bluecube,table)), clear(bluecube),
		fluent(on(yellowcube,table)),
		fluent(on(greentriangle,table)), clear(greentriangle)
	],
	[
		freehand(hand1),
		fluent(on(greentriangle,yellowcube)), clear(greentriangle),
		fluent(on(bluecube,table)), clear(bluecube),
		fluent(on(yellowcube,table)),
		fluent(on(redtriangle,table)), clear(redtriangle)
	],
	[
		freehand(hand1),
		fluent(on(greentriangle,bluecube)), clear(greentriangle),
		fluent(on(bluecube,table)),
		fluent(on(yellowcube,table)), clear(yellowcube),
		fluent(on(redtriangle,table)), clear(redtriangle)
	]
	],
	random_member(GoalState, GoalStates),
	assert(goalState(GoalState)).	
	
	
