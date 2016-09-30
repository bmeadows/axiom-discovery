
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

derived(is_morning) :- currentState(fluent(hour(10))).
derived(is_morning) :- currentState(fluent(hour(12))).
derived(is_afternoon) :- currentState(fluent(hour(2))).
derived(is_afternoon) :- currentState(fluent(hour(4))).
derived(is_evening) :- currentState(fluent(hour(6))).
derived(is_evening) :- currentState(fluent(hour(8))).

derived(person(P)) :- static(role(P, salesperson)).
derived(person(P)) :- static(role(P, engineer)).
derived(person(P)) :- static(role(P, manager)).

derived(location(X)) :- static(room(X)).

static(robot(rob1)).
static(role(p1, salesperson)).
static(role(p2, engineer)).
static(role(p3, manager)).
static(room(office_space)).
static(room(conference_room)).
static(room(kitchen)).
static(room(workshop)).

goalState([
	lastActionWas(serve(rob1,P)),
	person(P), % derived
	not(fluent(hasDrink(P))) % TODO ensure all forms of negation work
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % % ORACLE % % %

domain_specified_end :-
	currentState(fluent(hour(over))).

% System-internal symbol: moved_at(#person, #hour, #destination).
% 1. Set up oracle to check for relevant instances of moved_at.
% 2. Have it move the person BEFORE checking the results of the robot's action.
% 3. Allows the system to learn the failure mode where the robot and people are in different locations.
% 	 Ideally, this would successfully generalise from the (3 people)*(4 locations)*(3 other locations)=36 different cases.

applyActionToState(Action) :-
	currentState(fluent(hour(Hour))),
	% 1. Update time
	retractall(currentState(fluent(hour(Hour)))),
	(Hour == 10 -> NewHour = 12 ; true),
	(Hour == 12 -> NewHour = 2 ; true),
	(Hour == 2 -> NewHour = 4 ; true),
	(Hour == 4 -> NewHour = 6 ; true),
	(Hour == 6 -> NewHour = 8 ; true),
	(Hour == 8 -> NewHour = over ; true),
	(Hour == over -> (NewHour = over, trace) ; true),
	assert(currentState(fluent(hour(NewHour)))),
	% 2. Nobody is moving yet
	retractall(currentState(fluent(moving(_Person)))),
	% 3. Find people who are about to move
	forall(	(moved_at(P, H, _D), NewHour = H),
			assert(currentState(fluent(moving(P))))
		),
	% 4. Move people who had been about to move
	forall(	(moved_at(P, H, D), Hour = H),
			(	retractall(currentState(fluent(using_equipment(P)))), % 4.(a) No longer using equipment, if they were
				retractall(currentState(fluent(at(P,_Loc)))), % 4.(b) No longer at the previous location
				assert(currentState(fluent(at(P,D)))), % 4.(c) Now at the new location
				checkUsingEquipment(P) % 4.(d) Standard (dependent) chance of using equipment at the new location
			)
	),
	applyActionToStateFinal(Action).

applyActionToStateFinal(move_to(Robot, Loc)) :-
	retractall(	currentState(fluent(at(Robot,_)))),
	assert(		currentState(fluent(at(Robot,Loc)))),
	!.

% Failed 'serve': list each possible general case

% Somebody is using equipment
applyActionToStateFinal(serve(_R, P)) :-
	currentState(fluent(using_equipment(P))),
	!.
% Not co-located
applyActionToStateFinal(serve(R, P)) :-
	currentState(fluent(at(R, Loc1))),
	currentState(fluent(at(P, Loc2))),
	Loc1 \== Loc2,
	!.
% person's location is workshop.
applyActionToStateFinal(serve(_R, P)) :-
	currentState(fluent(at(P, workshop))),
	!.
% person's location is conference_room and hour is not 12.
applyActionToStateFinal(serve(_R, P)) :-
	not(currentState(fluent(hour(12)))),
	currentState(fluent(at(P, conference_room))),
	!.
% person is engineer and time is not afternoon.
applyActionToStateFinal(serve(_R, P)) :-
	static(role(P, engineer)),
	not(derived(is_afternoon)),
	!.

% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, P)) :-
	currentState(fluent(at(R, Loc1))),
	currentState(fluent(at(P, Loc1))),
	not(currentState(fluent(hasDrink(P)))),
	assert(currentState(fluent(hasDrink(P)))),
	!.
	
applyActionToStateFinal(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	trace.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
validAction(move_to(Robot, Destination)) :-
	static(robot(Robot)),
	derived(location(Destination)),
	not((  currentState(fluent(at(Robot, Destination)))  )).

validAction(serve(Robot, Person)) :-
	static(robot(Robot)),
	derived(person(Person)),
	currentState(fluent(at(Robot, Location))),
	currentState(fluent(at(Person, Location))),
	not(currentState(fluent(hasDrink(Person)))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getAllPossibleTests(List) :-
	List = [
		%fluent( hasDrink(p1) ),
		%fluent( hasDrink(p2) ),
		%fluent( hasDrink(p3) ),
		fluent( using_equipment(p1) ),
		fluent( using_equipment(p2) ),
		fluent( using_equipment(p3) ),
		fluent( hour(10) ),
		fluent( hour(12) ),
		fluent( hour(2) ),
		fluent( hour(4) ),
		fluent( hour(6) ),
		fluent( hour(8) ),
		fluent( at(p1, office_space) ),
		fluent( at(p1, conference_room) ),
		fluent( at(p1, kitchen) ),
		fluent( at(p1, workshop) ),
		fluent( at(p2, office_space) ),
		fluent( at(p2, conference_room) ),
		fluent( at(p2, kitchen) ),
		fluent( at(p2, workshop) ),
		fluent( at(p3, office_space) ),
		fluent( at(p3, conference_room) ),
		fluent( at(p3, kitchen) ),
		fluent( at(p3, workshop) ),
		fluent( at(rob1, office_space) ),
		fluent( at(rob1, conference_room) ),
		fluent( at(rob1, kitchen) ),
		fluent( at(rob1, workshop) ),
		%
		% [Special because they don't interact with domain knowledge at all]
		fluent( moving(p1) ),
		fluent( moving(p2) ),
		fluent( moving(p3) ),
		%
		action( move_to(rob1, office_space) ),
		action( move_to(rob1, conference_room) ),
		action( move_to(rob1, kitchen) ),
		action( move_to(rob1, workshop) ),
		action( serve(rob1, p1) ),
		action( serve(rob1, p2) ),
		action( serve(rob1, p3) )
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retractall(currentState(_)),
	% Nobody has drink to begin with, for now
	assertOneAtRandom([hour(10), hour(12), hour(2), hour(4), hour(6)]), % Can start at any hour of the day except the final one, for now - TODO think about more
	setupPeopleLocsAndMovements,
	assertOneAtRandom([at(rob1, office_space), at(rob1, conference_room), at(rob1, kitchen), at(rob1, workshop)]),
	% In certain settings people will be using equipment
	forall(		derived(person(P)),
				checkUsingEquipment(P) ),
	true.
	
checkUsingEquipment(Person) :-
	ignore(checkWorkshopEquipmentUse(Person)),
	ignore(checkKitchenEquipmentUse(Person)),
	ignore(checkOfficeEquipmentUse(Person)).

% An engineer in the workshop is using equipment
checkWorkshopEquipmentUse(P) :-
	static(role(P, engineer)),
	currentState(fluent(at(P, workshop))),
	chanceUsingEquipment(P, 100.0).

% Anyone in the kitchen has a 50% chance of using equipment
checkKitchenEquipmentUse(P) :-
	derived(person(P)),
	currentState(fluent(at(P, kitchen))),
	chanceUsingEquipment(P, 0.5).

% A salesperson in the office_space has a 50% chance of using equipment
checkOfficeEquipmentUse(P) :-
	static(role(P, salesperson)),
	currentState(fluent(at(P, office_space))),
	chanceUsingEquipment(P, 0.5).
	
chanceUsingEquipment(P, Prob) :-
	(random(Rand), Rand < Prob) -> assertOneAtRandom([using_equipment(P)]) ; true.

setupPeopleLocsAndMovements :-
	setupPLAM(p1),
	setupPLAM(p2),
	setupPLAM(p3).
setupPLAM(Person) :-
	Hours = [10, 12, 2, 4, 6, 8],
	List1 = [at(Person, office_space), at(Person, conference_room), at(Person, kitchen), at(Person, workshop)],
	assertOneAtRandom(List1),
	currentState(fluent(at(Person,Loc1))),
	select(at(Person,Loc1), List1, List2),
	random_member(at(_,Destination1), List2),
	random_member(H1, Hours),
	assert(moved_at(Person, H1, Destination1)).
	
assertOneAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).
	
%assertFluents([]).
%assertFluents([A|B]) :-
%	assert(currentState(fluent(A))),
%	assertFluents(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO
% Not used yet
% Defines 'similar' cases. Think more about question-begging aspect.
% Revisit

% resetGoalAtRandom :-
% ...

	
	
