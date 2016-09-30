:- dynamic goalState/1, hour/1.

domain_specified_end :- hour(over) ; currentState(fluent(hasDrink(p1))).

num_possible_static_configs(1008).

% All choices independent.
% For two locations, room type attribute (4 choices - office_space, conference_room, kitchen, workshop) each, random, so 4^2 = 16 room combinations
% For two people, role attribute (3 choices - engineer, salesperson, manager) each, random, so 3^2 = 9 role combinations
% For current day, day of week attribute (5 choices) each, random, so 7 day combinations
% 
% Multiply out to get total of 1008 possible attribute configurations.
%
% (Formerly, days of week were hours of day; each person could either be using or not using equipment at an hour; and this changed nondeterministically
% as hours progressed. This domain is now simplified and nondeterministic outside change is not within its scope.)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

derived(person(P)) :- currentState(static(role(P, salesperson))).
derived(person(P)) :- currentState(static(role(P, engineer))).
derived(person(P)) :- currentState(static(role(P, manager))).

derived(location(loc1)).
derived(location(loc2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainGoalAction(serve(rob1,p1,loc1)).

% The goal state only gives the UNEXPECTED literals arising from the action failure.
% In this case, the unexpected consequence is that p1 does not have a drink.
establishGoalState :-
	domainGoalAction(ACT),
	assert(goalState([
		lastActionWas(ACT),
		not(fluent(hasDrink(p1)))
		])).

:- establishGoalState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % % ORACLE % % %

applyActionToState(Action) :-
	hour(Hour),
	% 1. Update time
	retractall(hour(Hour)),
	(Hour == 9 -> NewHour = 10 ; true),
	(Hour == 10 -> NewHour = 11 ; true),
	(Hour == 11 -> NewHour = 12 ; true),
	(Hour == 12 -> NewHour = 1 ; true),
	(Hour == 1 -> NewHour = 2 ; true),
	(Hour == 2 -> NewHour = 3 ; true),
	(Hour == 3 -> NewHour = 4 ; true),
	(Hour == 4 -> NewHour = over ; true),
	(Hour == over -> (NewHour = over, trace) ; true),
	assert(hour(NewHour)),
	% 2. Apply action
	applyActionToStateFinal(Action).
	% 3. Move people who had been about to move
	
	% If this was in use, system-internal symbol: moved_at(#person, #hour, #destination).
	% I.  Set up oracle to check for relevant instances of moved_at.
	% II. Have it move the person AFTER checking the results of the robot's action.
	/*forall(	(moved_at(P, Hour, D)),
			(	retractall(currentState(fluent(at(P,_Loc)))), % 4.(b) No longer at the previous location
				assert(currentState(fluent(at(P,D))))
			)
			
	)*/

applyActionToStateFinal(move_to(Robot, Loc)) :-
	retractall(	currentState(fluent(at(Robot,_)))),
	assert(		currentState(fluent(at(Robot,Loc)))),
	!.

% Failed 'serve'
% "It's not safe to drink if doing engineering in a workshop."
% Person's location is a workshop. Person is an engineer.
applyActionToStateFinal(serve(_R, P, Loc)) :-
	currentState(static(roomtype(Loc, workshop))),
	currentState(static(role(P, engineer))),
	!.

% Failed 'serve'	
% "Managers only drink on Mondays."
% Person is a manager. Day is not Monday.
applyActionToStateFinal(serve(_R, P, _L)) :-
	currentState(static(role(P, manager))),
	not(currentState(static(weekday(monday)))),
	!.

% Failed 'serve'
% "Nobody drinks in the conference room on Friday."
% Person's location is a conference room. Day is Friday.
applyActionToStateFinal(serve(_R, _P, Loc)) :-
	currentState(static(weekday(friday))),
	currentState(static(roomtype(Loc, conference_room))),
	!.

% Former versions of this domain instead considered whether somebody was using equipment, either in a workshop or a kitchen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, P, Loc)) :-
	currentState(fluent(at(R, Loc))),
	currentState(fluent(at(P, Loc))),
	not(currentState(fluent(hasDrink(P)))),
	assert(currentState(fluent(hasDrink(P)))),
	!.
	
applyActionToStateFinal(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	trace.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
validAction(move_to(Robot, Destination)) :-
	currentState(static(robot(Robot))),
	derived(location(Destination)),
	not( currentState(fluent(at(Robot, Destination))) ).

validAction(serve(Robot, Person, Location)) :-
	currentState(static(robot(Robot))),
	derived(person(Person)),
	currentState(fluent(at(Robot, Location))),
	currentState(fluent(at(Person, Location))),
	not(currentState(fluent(hasDrink(Person)))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Anything that varies.
getAllPossibleTests(List) :-
	List = [
		fluent( hasDrink(p1) ),
		fluent( hasDrink(p2) ),
		fluent( at(p1, loc1) ),
		fluent( at(p1, loc2) ),
		fluent( at(p2, loc1) ),
		fluent( at(p2, loc2) ),
		fluent( at(rob1, loc1) ),
		fluent( at(rob1, loc2) ),
		%
		action( move_to(rob1, loc1) ),
		action( move_to(rob1, loc2) ),
		action( serve(rob1, p1, loc1) ),
		action( serve(rob1, p1, loc2) ),
		action( serve(rob1, p2, loc1) ),
		action( serve(rob1, p2, loc2) ),
		%
		%static( uses_k_equipment(p1,true) ),
		%static( uses_k_equipment(p1,false) ),
		%static( uses_k_equipment(p2,true) ),
		%static( uses_k_equipment(p2,false) ),
		static( roomtype(loc1, office_space) ),
		static( roomtype(loc1, conference_room) ),
		static( roomtype(loc1, kitchen) ),
		static( roomtype(loc1, workshop) ),
		static( roomtype(loc2, office_space) ),
		static( roomtype(loc2, conference_room) ),
		static( roomtype(loc2, kitchen) ),
		static( roomtype(loc2, workshop) ),
		static( role(p1, salesperson)),
		static( role(p2, salesperson)),
		static( role(p1, engineer)),
		static( role(p2, engineer)),
		static( role(p1, manager)),
		static( role(p2, manager)),
		static(weekday(monday)),
		static(weekday(tuesday)),
		static(weekday(wednesday)),
		static(weekday(thursday)),
		static(weekday(friday))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
	retractall(hour(_)),
	% Nobody has drink to begin with, for now
	assertOneXAtRandom([hour(9), hour(10), hour(11), hour(12)]), % Can start at any hour in the first half of the day, for now...
	setupLocsAndMovements,
	!.

setupLocsAndMovements :-
	setupPLAM(p1),
	setupPLAM(p2),
	setupPLAM(rob1).
setupPLAM(Person) :-
	List1 = [at(Person, loc1), at(Person, loc2)],
	assertOneFluentAtRandom(List1).

assertOneFluentAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).

assertOneXAtRandom(List) :-
	random_member(X, List),
	assert(X).
	
	
assertStatics([]).
assertStatics([A|B]) :-
	assert(currentState(static(A))),
	assertStatics(B).
	
setRandomInitialStaticConfig :-
	retractall(currentState(static(_))),
	Rooms = [workshop, office_space, conference_room, kitchen],
	random_member(R1, Rooms),
	random_member(R2, Rooms),
	Stats = [roomtype(loc1,R1), roomtype(loc2,R2), robot(rob1)],
	assertStatics(Stats),
	Days = [monday, tuesday, wednesday, thursday, friday],
	random_member(Day, Days),
	assertStatics([weekday(Day)]),
	RoleAlts = [engineer, manager, salesperson, engineer, manager, salesperson],
	random_member(RA, RoleAlts),
	random_member(RB, RoleAlts),
	assertStatics([role(p1,RA), role(p2,RB)]).
	
	/*
	...
	forall(		derived(person(P)),
				checkUsingEquipment(P) ). % In certain settings people will be using equipment
		
checkUsingEquipment(P) :-
	currentState(static(role(P, salesperson))),
	!,
	chanceUsingEquipment(P, 0.6).

checkUsingEquipment(P) :-
	currentState(static(role(P, engineer))),
	!,
	chanceUsingEquipment(P, 0.5).
	
checkUsingEquipment(P) :-
	currentState(static(role(P, manager))),
	!,
	chanceUsingEquipment(P, 0.4).
	
chanceUsingEquipment(P, Prob) :-
	(random(Rand), Rand < Prob) -> assertStatics([uses_k_equipment(P,true)]) ; assertStatics([uses_k_equipment(P,false)]).
*/

%

change_static_value(_, static(role(P,Role)), Return) :-
	Roles1 = [engineer, manager, salesperson],
	select(Role, Roles1, Roles2),
	random_member(NewVal, Roles2),
	Return = static(role(P,NewVal)),
	!.

change_static_value(_, static(roomtype(R,Room)), Return) :-
	Rs1 = [workshop, office_space, conference_room, kitchen],
	select(Room, Rs1, Rs2),
	random_member(NewVal, Rs2),
	Return = static(roomtype(R,NewVal)),
	!.
	
change_static_value(_, static(weekday(X)), Return) :-
	Days = [monday, tuesday, wednesday, thursday, friday],
	select(X, Days, Days2),
	random_member(NewVal, Days2),
	Return = static(weekday(NewVal)),
	!.

% For other, fixed statics like 'robot(rob1)'.
change_static_value(_ElidedList, AnythingElse, AnythingElse) :- !.

domainChangeStatics(List) :-
	!,
	domainChangeStaticsRand(List).
	
domainChangeStaticsRand(List) :-
	random_member(X,List), % Pick one literal to change at random
	% Note you never need to add or subtract static literals.
	select(X, List, ElidedList),
	change_static_value(ElidedList, X, Y), % Call a domain function making a valid change to something other than the original value
	retractall(currentState(X)),
	(Y == [] -> true ; assert(currentState(Y))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In time, the goal itself may be reset, for broader learning.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

domain_attribute_alternatives(role(P, salesperson), [role(P, engineer), role(P, manager)]).
domain_attribute_alternatives(role(P, engineer), [role(P, salesperson), role(P, manager)]).
domain_attribute_alternatives(role(P, manager), [role(P, salesperson), role(P, engineer)]).

domain_attribute_alternatives(roomtype(Loc, workshop), [roomtype(Loc, kitchen), roomtype(Loc, office_space), roomtype(Loc, conference_room)]).
domain_attribute_alternatives(roomtype(Loc, kitchen), [roomtype(Loc, workshop), roomtype(Loc, office_space), roomtype(Loc, conference_room)]).
domain_attribute_alternatives(roomtype(Loc, office_space), [roomtype(Loc, workshop), roomtype(Loc, kitchen), roomtype(Loc, conference_room)]).
domain_attribute_alternatives(roomtype(Loc, conference_room), [roomtype(Loc, workshop), roomtype(Loc, kitchen), roomtype(Loc, office_space)]).

domain_attribute_alternatives(weekday(monday), [weekday(tuesday), weekday(wednesday), weekday(thursday), weekday(friday)]).
domain_attribute_alternatives(weekday(tuesday), [weekday(monday), weekday(wednesday), weekday(thursday), weekday(friday)]).
domain_attribute_alternatives(weekday(wednesday), [weekday(monday), weekday(tuesday), weekday(thursday), weekday(friday)]).
domain_attribute_alternatives(weekday(thursday), [weekday(monday), weekday(tuesday), weekday(wednesday), weekday(friday)]).
domain_attribute_alternatives(weekday(friday), [weekday(monday), weekday(tuesday), weekday(wednesday), weekday(thursday)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

