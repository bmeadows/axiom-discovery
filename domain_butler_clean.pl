
:- dynamic goalState/1, step/1.

domain_specified_end :- step(last) ; currentState(fluent(hasDrink(p1))).

num_possible_attribute_configs(11664).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_attribute_alternatives(role(P, salesperson), [role(P, engineer), role(P, manager)]).
domain_attribute_alternatives(role(P, engineer), [role(P, salesperson), role(P, manager)]).
domain_attribute_alternatives(role(P, manager), [role(P, salesperson), role(P, engineer)]).

domain_attribute_alternatives(roomtype(Loc, workshop), [roomtype(Loc, kitchen), roomtype(Loc, office_space)]).
domain_attribute_alternatives(roomtype(Loc, kitchen), [roomtype(Loc, workshop), roomtype(Loc, office_space)]).
domain_attribute_alternatives(roomtype(Loc, office_space), [roomtype(Loc, workshop), roomtype(Loc, kitchen)]).

% These are actually used for checking / virtuous states, not for static configurations at all

domain_attribute_alternatives(status(Obj, intact), [status(Obj, broken)]).
domain_attribute_alternatives(status(Obj, broken), [status(Obj, intact)]).

domain_attribute_alternatives(contains(Cup, empty), [contains(Cup, drink)]).
domain_attribute_alternatives(contains(Cup, drink), [contains(Cup, empty)]).

domain_attribute_alternatives(height(Tab, high), [height(Tab, medium), height(Tab, low)]).
domain_attribute_alternatives(height(Tab, medium), [height(Tab, high), height(Tab, low)]).
domain_attribute_alternatives(height(Tab, low), [height(Tab, high), height(Tab, medium)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

derived(person(P)) :- currentState(attr(role(P, salesperson))).
derived(person(P)) :- currentState(attr(role(P, engineer))).
derived(person(P)) :- currentState(attr(role(P, manager))).

%derived(location(X)) :- attr(room(X)).
derived(location(loc1)).
derived(location(loc2)).

% 2 locations: loc1, loc2
% 3 people: p1, p2, p3
% 1 robot
% STATIC:
% 3 roles that can double up: engineer, salesperson, manager
% 3 room types that can double up: office_space, kitchen, workshop
% 2 object statuses for cups
% 2 contents for cups
% 3 table heights for tables
% ------each person either is or isn't using equipment------
% So 3*3*3*3*3*2*2*2*2*3    =11664  unique static configurations if NO equipment

/*
things with no interactions...
2x shelf, always intact
1x desk, always intact
1x elevator, not an object, no location
table1 is always intact
p3 has no location
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainGoalAction(serve(rob1,p1,loc1,cup1)).
unexpectedResult(not(fluent(hasDrink(p1)))).

%domainGoalAction(pickup(rob1,cup1,tab1)).
%unexpectedResult(not(fluent(holding(rob1,cup1)))).

establishGoalState :-
	domainGoalAction(ACT),
	% Unexpected observations
	unexpectedResult(X),
	assert(goalState([
		lastActionWas(ACT),
		X
		])).

:- establishGoalState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % % ORACLE % % %

applyActionToState(Action) :-
	step(I),
	% 1. Update time
	retractall(step(I)),
	(I == 1 -> J = 2 ; true),
	(I == 2 -> J = 3 ; true),
	(I == 3 -> J = 4 ; true),
	(I == 4 -> J = 5 ; true),
	(I == 5 -> J = 6 ; true),
	(I == 6 -> J = 7 ; true),
	(I == 7 -> J = 8 ; true),
	(I == 8 -> J = 9 ; true),
	(I == 9 -> J = last ; true),
	(I == last -> (J = last, trace) ; true),
	assert(step(J)),
	% 2. Apply action
	applyActionToStateFinal(Action).
	% 3. Move people who had been about to move

applyActionToStateFinal(move_to(Robot, Loc)) :-
	retractall(	currentState(fluent(at(Robot,_)))),
	assert(		currentState(fluent(at(Robot,Loc)))),
	!.

% Failed 'serve':

% "It's not safe to drink if doing engineering in a workshop."
% 1. Person's location is workshop. Person is engineer
applyActionToStateFinal(serve(_R, P, Loc, _Cup)) :-
	currentState(attr(roomtype(Loc, workshop))),
	currentState(attr(role(P, engineer))),
	!.

% "Can't serve an empty cup to someone."
% 2. The cup is empty.
applyActionToStateFinal(serve(_R, _P, _Loc, Cup)) :-
	currentState(attr(contains(Cup, empty))),
	!.
	
% Failed 'pickup':

% "Can't pickup a broken object."
% 1. Obj is broken
applyActionToStateFinal(pickup(_R, Obj, _Table)) :-
	currentState(attr(status(Obj, broken))),
	!.

% "Can't pickup from a high table."
% 2. The Table is high.
applyActionToStateFinal(pickup(_R, _Obj, Table)) :-
	currentState(attr(height(Table, high))),
	!.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, P, Loc, Cup)) :-
	currentState(fluent(at(R, Loc))),
	currentState(fluent(at(P, Loc))),
	currentState(fluent(holding(R, Cup))),
	not(currentState(fluent(hasDrink(P)))),
	assert(currentState(fluent(hasDrink(P)))),
	retract(currentState(fluent(holding(R, Cup)))),
	!.

% Succeeded pickup: none of the above cases applied
applyActionToStateFinal(pickup(R, Obj, Table)) :-
	currentState(fluent(at(R, Loc))),
	currentState(fluent(at(Table, Loc))),
	currentState(fluent(on(Obj, Table))),
	assert(currentState(fluent(holding(R, Obj)))),
	retract(currentState(fluent(on(Obj, Table)))),
	!.

applyActionToStateFinal(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	trace.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
validAction(move_to(Robot, Destination)) :-
	currentState(attr(robot(Robot))),
	derived(location(Destination)),
	not( currentState(fluent(at(Robot, Destination))) ).

validAction(serve(Robot, Person, Location, Cup)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(cup(Cup))),
	derived(person(Person)),
	currentState(fluent(at(Robot, Location))),
	currentState(fluent(at(Person, Location))),
	currentState(fluent(holding(Robot, Cup))),
	not(currentState(fluent(hasDrink(Person)))).
	
validAction(pickup(Robot, Object, Table)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(table(Table))),
	currentState(attr(object(Object))),
	currentState(fluent(at(Robot, Loc))),
	currentState(fluent(at(Table, Loc))),
	currentState(fluent(on(Object, Table))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests relevant to target action

getAllPossibleTests(List) :-
	List = [
		fluent( hasDrink(p1) ),
		fluent( at(p1, loc1) ),
		fluent( at(p1, loc2) ),
		fluent( at(p2, loc1) ),
		fluent( at(rob1, loc1) ),
		fluent( at(rob1, loc2) ),
		%
		action( move_to(rob1, loc1) ),
		action( move_to(rob1, loc2) ),
		action( serve(rob1, p1, loc1, cup1) ),
		action( serve(rob1, p1, loc2, cup1) ),
		action( serve(rob1, p2, loc1, cup1) ),
		action( serve(rob1, p2, loc2, cup1) ),
		action( serve(rob1, p1, loc1, cup2) ),
		action( serve(rob1, p1, loc2, cup2) ),
		action( serve(rob1, p2, loc1, cup2) ),
		action( serve(rob1, p2, loc2, cup2) ),
		%
		attr( roomtype(loc1, office_space) ),
		attr( roomtype(loc1, kitchen) ),
		attr( roomtype(loc1, workshop) ),
		attr( role(p1, salesperson)),
		attr( role(p1, engineer)),
		attr( role(p1, manager)),
		%
		fluent( on(cup1, tab1) ),
		fluent( on(cup2, tab1) ),
		fluent( holding(rob1, cup1) ),
		fluent( at(tab1, loc1) ),
		fluent( at(tab1, loc2) ),
		action( pickup(rob1, cup1, tab1) ),
		attr( status(cup1, intact)),
		attr( status(cup1, broken)),
		attr( status(tab1, intact)),
		attr( status(tab1, broken)),
		attr( contains(cup1, drink)),
		attr( contains(cup1, empty)),
		attr( height(tab1, high)),
		attr( height(tab1, medium)),
		attr( height(tab1, low))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
	retractall(step(_)),
	% Nobody has drink to begin with, for now
	assertOneXAtRandom([step(1), step(2), step(3), step(4)]),
	setupLocsAndMovements,
	!.

setupLocsAndMovements :-
	setupPLAM(p1),
	setupPLAM(p2),
	setupPLAM(rob1),
	setuptablesandcups.
setupPLAM(Person) :-
	List1 = [at(Person, loc1), at(Person, loc2)],
	assertOneFluentAtRandom(List1).
	
assertOneFluentAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).

assertOneXAtRandom(List) :-
	random_member(X, List),
	assert(X).
	
setuptablesandcups :-
	assert(currentState(fluent(at(tab1,loc1)))),
	CupStarts1 = [on(cup1,tab1), holding(rob1,cup1)],
	random_member(C1, CupStarts1),
	CupStarts2 = [on(cup2,tab1), holding(rob1,cup2)],
	random_member(C2, CupStarts2),
	assert(currentState(fluent(C1))),
	assert(currentState(fluent(C2))),
	assert(currentState(at(shelf1,loc1))),
	assert(currentState(at(shelf2,loc2))),
	assert(currentState(at(desk1,loc1))).

assertAtts([]).
assertAtts([A|B]) :-
	assert(currentState(attr(A))),
	assertAtts(B).
	
setRandomInitialObjectConfig :-
	retractall(currentState(attr(_))),
	Rooms = [workshop, office_space, kitchen],
	random_member(R1, Rooms),
	random_member(R2, Rooms),
	Stats = [roomtype(loc1,R1), roomtype(loc2,R2), robot(rob1), table(tab1), cup(cup1), cup(cup2), object(tab1), object(cup1), object(cup2), object(shelf1), object(shelf2), object(desk1), elevator(elevator1), connected(elevator1,loc1), connected(elevator1,loc2), connected(loc1,loc2), connected(loc2,loc1)],
	assertAtts(Stats),
	RoleAlts = [engineer, manager, salesperson, engineer, manager, salesperson],
	random_member(RA, RoleAlts),
	random_member(RB, RoleAlts),
	random_member(RC, RoleAlts),
	assertAtts([role(p1,RA), role(p2,RB), role(p3,RC)]),
	setupAffordanceProperties.
	
setupAffordanceProperties :-
	%
	StatusAlts = [broken, intact],
	random_member(S1, StatusAlts),
	random_member(S2, StatusAlts),
	assertAtts([status(cup1,S1), status(cup2,S2), status(tab1,intact)]),
	assertAtts([status(shelf1,intact), status(shelf2,intact), status(desk1,intact)]),
	%
	DAlts = [drink, empty],
	random_member(D1, DAlts),
	random_member(D2, DAlts),
	assertAtts([contains(cup1,D1), contains(cup2,D2)]),
	%
	HAlts = [low, medium, high],
	random_member(H1, HAlts),
	assertAtts([height(tab1,H1)]).
	
change_att_value(_, attr(role(P,Role)), Return) :-
	Roles1 = [engineer, manager, salesperson],
	select(Role, Roles1, Roles2),
	random_member(NewVal, Roles2),
	Return = attr(role(P,NewVal)),
	!.

change_att_value(_, attr(roomtype(R,Room)), Return) :-
	Rs1 = [workshop, office_space, kitchen],
	select(Room, Rs1, Rs2),
	random_member(NewVal, Rs2),
	Return = attr(roomtype(R,NewVal)),
	!.
	

% AFFORDANCES

% Some shouldn't ever change
change_att_value(_, attr(status(desk1,intact)), attr(status(desk1,intact))) :- !.
change_att_value(_, attr(status(shelf1,intact)), attr(status(shelf1,intact))) :- !.
change_att_value(_, attr(status(shelf2,intact)), attr(status(shelf2,intact))) :- !.
change_att_value(_, attr(status(tab1,intact)), attr(status(tab1,intact))) :- !.

change_att_value(_, attr(status(N,X)), Return) :-
	Ns1 = [intact, broken],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(status(N,NewVal)),
	!.
change_att_value(_, attr(contains(N,X)), Return) :-
	Ns1 = [drink, empty],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(contains(N,NewVal)),
	!.
change_att_value(_, attr(height(N,X)), Return) :-
	Ns1 = [low, medium, high],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(height(N,NewVal)),
	!.
	
% For other cases, e.g., class membership may be written as object attribute because it's non-fluent
change_att_value(_ElidedList, AnythingElse, AnythingElse) :- !.

domainChangeObjectAtts(List) :-
	!,
	domainChangeObjectAttsRand(List).
	
domainChangeObjectAttsRand(List) :-
	random_member(X,List), % Pick one literal to change at random
	% Note you never need to add or subtract object attr literals
	select(X, List, ElidedList),
	change_att_value(ElidedList, X, Y), % Call a domain function making a valid change to something other than the original value
	retractall(currentState(X)),
	(Y == [] -> true ; assert(currentState(Y))).

