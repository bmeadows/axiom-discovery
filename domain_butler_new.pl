:- dynamic step/1.

domain_specified_end :-	step(last), !.
domain_specified_end :-	domainGoalAction(serve(rob1,cup1,p1)), ( currentState(fluent(in_hand(P,cup1))), currentState(attr(person(P))) ), !.
domain_specified_end :-	domainGoalAction(pickup(rob1,cup1)), currentState(fluent(in_hand(rob1,cup1))), !.
domain_specified_end :-	( not(currentState(fluent(loc(cup1,_)))), not(currentState(fluent(in_hand(_,cup1)))) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_test_alternatives(attr(role_type(P, sales)), [attr(role_type(P, engineer)), attr(role_type(P, manager))]).
domain_test_alternatives(attr(role_type(P, engineer)), [attr(role_type(P, sales)), attr(role_type(P, manager))]).
domain_test_alternatives(attr(role_type(P, manager)), [attr(role_type(P, sales)), attr(role_type(P, engineer))]).

domain_test_alternatives(attr(obj_status(Obj, intact)), [attr(obj_status(Obj, damaged))]).
domain_test_alternatives(attr(obj_status(Obj, damaged)), [attr(obj_status(Obj, intact))]).

domain_test_alternatives(attr(obj_weight(X, heavy)), [attr(obj_weight(X, light))]).
domain_test_alternatives(attr(obj_weight(X, light)), [attr(obj_weight(X, heavy))]).

domain_test_alternatives(N, Return) :-
	N=fluent(_),
	select(N, [fluent(loc(X, office)), fluent(loc(X, library)), fluent(loc(X, workshop)), fluent(loc(X, kitchen)), fluent(in_hand(p1, X)), fluent(in_hand(p2, X)), fluent(in_hand(p3, X)), fluent(in_hand(rob1, X))], Return).
% Note that this can return some bad values, such as in_hand(p1, p2) or even in_hand(p1, p2), but these functions are only used to swap in and out literals,
% which is always followed by testing for 'physicalConstraintsViolated' (and starting again if there is a problem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainGoalAction(serve(rob1,cup1,p1)).
unexpectedResult(not(fluent(in_hand(p1,cup1)))).
failureStateFluents([loc(p1,workshop),loc(p2,library),loc(p3,office),loc(rob1,workshop),in_hand(rob1,cup1),in_hand(p2,cup2),loc(book1,library),loc(prin1,office),loc(shelf1,library),loc(shelf2,kitchen),loc(desk1,office),loc(tab1,workshop)]).

%domainGoalAction(pickup(rob1,cup1)).
%unexpectedResult(not(fluent(in_hand(rob1,cup1)))).
%failureStateFluents([loc(p1,workshop),loc(p2,library),loc(p3,office),loc(rob1,kitchen),loc(cup1,kitchen),in_hand(p2,cup2),loc(book1,library),loc(prin1,office),loc(shelf1,library),loc(shelf2,kitchen),loc(desk1,office),loc(tab1,workshop)]).

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
	(I == 9 -> J = last ; J is I + 1),
	assert(step(J)),
	% 2. Apply action
	applyActionToStateFinal(Action),
	applyNoiseWhereAppropriate.
	% 3. Move people who had been about to move

applyActionToStateFinal(move(Robot, Loc)) :-
	retractall(	currentState(fluent(loc(Robot,_)))),
	assert(		currentState(fluent(loc(Robot,Loc)))),
	!.

% Failed 'serve':

% "It's not safe to drink if doing engineering in a workshop."
applyActionToStateFinal(serve(R, _O, P)) :-
	currentState(fluent(loc(R, workshop))),
	currentState(fluent(loc(P, workshop))),
	currentState(attr(role_type(P, engineer))),
	!.
	
% "Can't serve a damaged object to someone."
applyActionToStateFinal(serve(_R, O, _P)) :-
	currentState(attr(obj_status(O, damaged))),
	!.
	
% Failed 'pickup':

% "Can't pick up a heavy object."
applyActionToStateFinal(pickup(_R, Obj)) :-
	currentState(attr(obj_weight(Obj, heavy))),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Succeeded serve: none of the above cases applied
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	!.

% Succeeded pickup: none of the above cases applied
applyActionToStateFinal(pickup(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	not(currentState(fluent(in_hand(_, Obj)))),
	assert(currentState(fluent(in_hand(R, Obj)))),
	retract(currentState(fluent(loc(Obj, Loc)))),
	!.

applyActionToStateFinal(putdown(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	!.

applyActionToStateFinal(donothing) :- !.

applyActionToStateFinal(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	noiseChancePercent(Noise), % Noise is likely to blame, because it can set up impossible situations - thus ignore it
	((Noise > 0) -> true ; trace).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
validAction(move(Robot, Destination)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(location(Destination))),
	not( currentState(fluent(loc(Robot, Destination))) ).

validAction(serve(Robot, Obj, Person)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(object(Obj))),
	currentState(attr(person(Person))),
	currentState(fluent(loc(Robot, Location))),
	currentState(fluent(loc(Person, Location))),
	currentState(fluent(in_hand(Robot, Obj))),
	not(currentState(fluent(in_hand(Person, _)))).
	
validAction(pickup(Robot, Object)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(object(Object))),
	currentState(fluent(loc(Robot, Loc))),
	currentState(fluent(loc(Object, Loc))),
	not(currentState(fluent(in_hand(_, Object)))),
	not(currentState(fluent(in_hand(Robot, _)))).
	
validAction(putdown(Robot, Object)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(object(Object))),
	%currentState(fluent(loc(Robot, Loc))),
	currentState(fluent(in_hand(Robot, Object))).

validAction(donothing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Observed failure state: Fluents depend on target action
setObservedFailureState :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	failureStateFluents(S),
	assertFluents(S).

% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	setupLocsRandomly,
	!.

setupLocsRandomly :-
	% 1. Do entity locations independently
	setupEntityL(rob1), setupEntityL(p1), setupEntityL(p2), setupEntityL(p3),
	% 2. Do fixed object locations
	assert(currentState(fluent(loc(tab1,workshop)))),
	assert(currentState(fluent(loc(shelf1,library)))),
	assert(currentState(fluent(loc(shelf2,kitchen)))),
	assert(currentState(fluent(loc(desk1,office)))),
	% 3. Do non-fixed object locations dependently, because only one can be in a hand
	setObjLocationsRandomlyUntilValid([cup1,cup2,book1,prin1]),
	!.
	
setupEntityL(Entity) :-
	List1 = [loc(Entity, office), loc(Entity, workshop), loc(Entity, kitchen), loc(Entity, library)],
	assertOneFluentAtRandom(List1).

setObjLocationsRandomlyUntilValid(List) :-
	retractAllLocations(List),
	randomiseAllLocations(List),
	!,
	(physicalConstraintsViolated -> setObjLocationsRandomlyUntilValid(List) ; true).

retractAllLocations([]) :- !.
retractAllLocations([A|B]) :-
	retractall(currentState(fluent(loc(A,_)))),
	retractall(currentState(fluent(in_hand(_,A)))),
	retractAllLocations(B).

randomiseAllLocations([]) :- !.
randomiseAllLocations([A|B]) :-
	S = [in_hand(rob1,A),in_hand(p1,A),in_hand(p2,A),in_hand(p3,A),loc(A,library),loc(A,workshop),loc(A,kitchen),loc(A,office)],
	random_member(F, S),
	assert(currentState(fluent(F))),
	randomiseAllLocations(B).

% Returns all physical states, even if they break constraints
getTheoreticalStatePermutation(List) :-
	tryalllocs([p1,p2,p3,rob1,cup1,cup2,book1,prin1],[p1,p2,p3,rob1],[workshop,library,kitchen,office],[],List1),
	append(List1, [loc(tab1,workshop), loc(shelf1,library), loc(shelf2,kitchen), loc(desk1,office)], List).

tryalllocs([],_,_,Return,Return).
tryalllocs([A|B],Entities,Places,Working,Return) :-
	not(member(A,Entities)), % Precludes people being assigned in_hand other people
	member(X,Entities),
	append(Working,[in_hand(X,A)],New),
	tryalllocs(B,Entities,Places,New,Return).
tryalllocs([A|B],Entities,Places,Working,Return) :-
	member(X,Places),
	append(Working,[loc(A,X)],New),
	tryalllocs(B,Entities,Places,New,Return).
	
assertOneFluentAtRandom(List) :-
	random_member(F, List),
	assert(currentState(fluent(F))).

assertOneXAtRandom(List) :-
	random_member(X, List),
	assert(X).
	
setRandomInitialObjectConfig :-
	retractall(currentState(attr(_))),
	Stats = [ location(kitchen), location(workshop), location(library), location(office),
			person(p1), person(p2), person(p3), robot(rob1),
			cup(cup1), cup(cup2), object(cup1), object(cup2),
			book(book1), object(book1), printer(prin1), object(prin1),
			desk(desk1), object(desk1), table(tab1), object(tab1),
			shelf(shelf1), object(shelf1), shelf(shelf2), object(shelf2),
			furniture(prin1), furniture(tab1), furniture(desk1), furniture(shelf1), furniture(shelf2) ],
	assertAtts(Stats),
	setupAffordancePropertiesRandomly.
	
setupAffordancePropertiesRandomly :-
	RTAlts = [engineer, manager, sales, engineer, manager, sales],
	random_member(RA, RTAlts),
	random_member(RB, RTAlts),
	random_member(RC, RTAlts),
	assertAtts([role_type(p1,RA), role_type(p2,RB), role_type(p3,RC)]),
	%
	StatusAlts = [damaged, intact],
	random_member(S1, StatusAlts),
	random_member(S2, StatusAlts),
	random_member(S3, StatusAlts),
	random_member(S4, StatusAlts),
	random_member(S5, StatusAlts),
	random_member(S6, StatusAlts),
	random_member(S7, StatusAlts),
	random_member(S8, StatusAlts),
	assertAtts([obj_status(cup1,S1), obj_status(cup2,S2), obj_status(tab1,S3), obj_status(shelf1,S4), obj_status(shelf2,S5), obj_status(desk1,S6), obj_status(book1,S7), obj_status(prin1,S8)]),
	%
	WAlts = [heavy, light],
	random_member(W1, WAlts),
	random_member(W2, WAlts),
	random_member(W3, WAlts),
	random_member(W4, WAlts),
	random_member(W5, WAlts),
	random_member(W6, WAlts),
	random_member(W7, WAlts),
	random_member(W8, WAlts),
	assertAtts([obj_weight(cup1,W1), obj_weight(cup2,W2), obj_weight(tab1,W3), obj_weight(shelf1,W4), obj_weight(shelf2,W5), obj_weight(desk1,W6), obj_weight(book1,W7), obj_weight(prin1,W8)])
	.

change_att_value(_, attr(role_type(P,CurrentRole)), Return) :-
	Roles1 = [engineer, manager, sales],
	select(CurrentRole, Roles1, Roles2),
	random_member(NewVal, Roles2),
	Return = attr(role_type(P,NewVal)),
	!.

change_att_value(_, attr(obj_status(N,X)), Return) :-
	Ns1 = [intact, damaged],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(obj_status(N,NewVal)),
	!.
change_att_value(_, attr(obj_weight(N,X)), Return) :-
	Ns1 = [heavy, light],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(obj_weight(N,NewVal)),
	!.
	
% For other cases, e.g., class membership may be written as object attribute because it's non-fluent
change_att_value(_ElidedList, AnythingElse, AnythingElse) :- !.

% This is passed in a list of RELEVANT object properties
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

	
% Domain description

domain(attr(person(p1))).
domain(attr(person(p2))).
domain(attr(person(p3))).
domain(attr(robot(rob1))).
domain(attr(location(office))).
domain(attr(location(workshop))).
domain(attr(location(kitchen))).
domain(attr(location(library))).
domain(attr(cup(cup1))).
domain(attr(cup(cup2))).
domain(attr(book(book1))).
domain(attr(printer(prin1))).
domain(attr(shelf(shelf1))).
domain(attr(shelf(shelf2))).
domain(attr(desk(desk1))).
domain(attr(table(tab1))).
domain(attr(entity(E))) :- domain(attr(robot(E))) ; domain(attr(person(E))).
domain(attr(furniture(O))) :- domain(attr(shelf(O))) ; domain(attr(desk(O))) ; domain(attr(table(O))) ; domain(attr(printer(O))).
domain(attr(object(O))) :- domain(attr(cup(O))) ; domain(attr(book(O))) ; domain(attr(furniture(O))).
domain(attr(role_type(P,R))) :- domain(attr(person(P))), member(R, [sales, engineer, manager]).
domain(attr(obj_status(O,S))) :- domain(attr(object(O))), member(S, [intact, damaged]).
domain(attr(obj_weight(O,S))) :- domain(attr(object(O))), member(S, [light, heavy]).

% Permissible fluents - (not actually needed??)
domain(fluent(loc(X,Y))) :- domain(attr(entity(X))), domain(attr(location(Y))).
domain(fluent(loc(X,Y))) :- domain(attr(object(X))), domain(attr(location(Y))).
domain(fluent(in_hand(E,O))) :- domain(attr(entity(E))), domain(attr(object(O))).
	
% Permissible actions
domain(action(pickup(R,O))) :- domain(attr(robot(R))), domain(attr(object(O))).
domain(action(putdown(R,O))) :- domain(attr(robot(R))), domain(attr(object(O))).
domain(action(move(R,L))) :- domain(attr(robot(R))), domain(attr(location(L))).
domain(action(serve(R,O,P))) :- domain(attr(robot(R))), domain(attr(person(P))), domain(attr(object(O))).



physicalConstraintsViolated :- currentState(fluent(loc(O,L1))), currentState(fluent(loc(O,L2))), L1 \= L2. % One thing in two places
physicalConstraintsViolated :- currentState(fluent(loc(O,_))), currentState(fluent(in_hand(_,O))). % Anything simultaneously in hand and at a place
physicalConstraintsViolated :- currentState(fluent(in_hand(H1,O))), currentState(fluent(in_hand(H2,O))), H1 \= H2. % Anything simultaneously in two different hands
physicalConstraintsViolated :- currentState(fluent(in_hand(H,O1))), currentState(fluent(in_hand(H,O2))), O1 \= O2. % Same entity has two things in hand
physicalConstraintsViolated :- (currentState(attr(robot(E))) ; currentState(attr(person(E)))), not(currentState(fluent(loc(E,_)))). % An entity not at a place
physicalConstraintsViolated :- (	currentState(attr(cup(O))) ;
									currentState(attr(shelf(O))) ;
									currentState(attr(table(O))) ;
									currentState(attr(desk(O))) ;
									currentState(attr(book(O))) ;
									currentState(attr(printer(O)))
								), not(currentState(fluent(loc(O,_)))), not(currentState(fluent(in_hand(_,O)))). % An object neither in_hand nor at a place


% Physical configurations:
% Assuming furniture location is fixed...
% 3+1 agents in 4 locations = 4^4 = 256
% 3 movable objects that can be held (at most 1 per agent) or at locations (any number at a location) = 222
% 222 * 256 = 56,832 physical configurations

% Object property configurations:
% For 8 objects
% 3^3 * 2^8 * 2^8 (role_type, obj_status, obj_weight)
% 27 * 256 * 256
% = 1,769,472 object property configurations



% Returns a number uniquely identifying the target affordance, or returns it back for "does not match any target affordance"
% 1. Damaged object
domainAffordanceClassifier([ [], [attr(obj_status(cup1, intact))] ], 1) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(obj_status(cup1, damaged))], [] ], 1) :- domainGoalAction(serve(rob1,cup1,p1)), !.
% 2. Engineer/workshop
domainAffordanceClassifier([ [attr(role_type(p1, engineer)), fluent(loc(rob1, workshop))], [] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1, engineer)), fluent(loc(p1, workshop))], [] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(p1,library)),fluent(loc(p1,office))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,library)),fluent(loc(p1,office)),fluent(loc(rob1,kitchen))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(p1,office)),fluent(loc(rob1,library))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(p1,library)),fluent(loc(rob1,office))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,office)),fluent(loc(rob1,kitchen)),fluent(loc(rob1,library))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,library)),fluent(loc(rob1,kitchen)),fluent(loc(rob1,office))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(rob1,library)),fluent(loc(rob1,office))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(rob1,kitchen)),fluent(loc(rob1,library)),fluent(loc(rob1,office))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [fluent(loc(p1,workshop))], [attr(role_type(p1,manager)),attr(role_type(p1,sales))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAffordanceClassifier([ [fluent(loc(rob1,workshop))], [attr(role_type(p1,manager)),attr(role_type(p1,sales))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
% Catch case: Everything else
domainAffordanceClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- domainGoalAction(serve(rob1,cup1,p1)), !.

% 3. Heavy
domainAffordanceClassifier([ [attr(obj_weight(cup1, heavy))], [] ], 3) :- domainGoalAction(pickup(rob1,cup1)), !.
domainAffordanceClassifier([ [], [attr(obj_weight(cup1, light))] ], 3) :- domainGoalAction(pickup(rob1,cup1)), !.
% Catch case: Everything else
domainAffordanceClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- domainGoalAction(pickup(rob1,cup1)), !.



/* Robot butler 'serve' action */
cached :- 
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)),
	assert(allValidTests([attr(obj_status(cup1,damaged)),attr(obj_status(cup1,intact)),attr(obj_weight(cup1,heavy)),attr(obj_weight(cup1,light)),
		attr(role_type(p1,engineer)),attr(role_type(p1,manager)),attr(role_type(p1,sales)),fluent(loc(p1,workshop)),fluent(loc(rob1,workshop)),
		fluent(in_hand(rob1,cup1)),fluent(loc(p1,library)),fluent(loc(rob1,library)),fluent(loc(p1,kitchen)),fluent(loc(rob1,kitchen)),
		fluent(loc(p1,office)),fluent(loc(rob1,office)),action(pickup(rob1,cup1)),action(putdown(rob1,cup1)),action(move(rob1,office)),
		action(move(rob1,workshop)),action(move(rob1,kitchen)),action(move(rob1,library)),action(serve(rob1,cup1,p1))])),
	assert(num_possible_attribute_configs(12)),
	assert(usableActionList([action(pickup(rob1, cup1)), action(putdown(rob1, cup1)), action(move(rob1, office)), action(move(rob1, workshop)), 
		action(move(rob1, kitchen)), action(move(rob1, library)), action(serve(rob1, cup1, p1))])). 

/* Robot butler 'pickup' action 
cached :- 
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)),
	assert(allValidTests([attr(obj_status(cup1,damaged)),attr(obj_status(cup1,intact)),attr(obj_weight(cup1,heavy)),attr(obj_weight(cup1,light)),
		fluent(loc(rob1,workshop)),fluent(loc(cup1,workshop)),fluent(loc(rob1,library)),fluent(loc(cup1,library)),fluent(loc(rob1,kitchen)),
		fluent(loc(cup1,kitchen)),fluent(loc(rob1,office)),fluent(loc(cup1,office)),action(pickup(rob1,cup1)),action(putdown(rob1,cup1)),
		action(move(rob1,office)),action(move(rob1,workshop)),action(move(rob1,kitchen)),action(move(rob1,library))])),
	assert(num_possible_attribute_configs(4)),
	assert(usableActionList([action(pickup(rob1, cup1)), action(putdown(rob1, cup1)), action(move(rob1, office)), action(move(rob1, workshop)), 
		action(move(rob1, kitchen)), action(move(rob1, library))])).
*/
