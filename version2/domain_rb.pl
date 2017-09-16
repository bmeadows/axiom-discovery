:- dynamic step/1.
:- discontiguous actionDescription/1, impossible_if/2.


domain_specified_end :-	step(last), !.
domain_specified_end :-	domainGoalAction(serve(rob1,cup1,p1)), ( currentState(fluent(in_hand(P,cup1))), currentState(attr(person(P))) ), !.
domain_specified_end :-	domainGoalAction(pickup(rob1,X)), currentState(fluent(in_hand(_,X))), !.
domain_specified_end :-	domainGoalAction(affix_label(rob1,X)), currentState(fluent(labelled(X,true))), !.

domain_specified_end :-	( not(currentState(fluent(loc(cup1,_)))), not(currentState(fluent(in_hand(_,cup1)))) ).
domain_specified_end :-	( not(currentState(fluent(loc(book1,_)))), not(currentState(fluent(in_hand(_,book1)))) ).
domain_specified_end :-	( not(currentState(fluent(loc(prin1,_)))), not(currentState(fluent(in_hand(_,prin1)))) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_test_alternatives(attr(arm_type(X, electromagnetic)), [attr(arm_type(X, pneumatic))]).
domain_test_alternatives(attr(arm_type(X, pneumatic)), [attr(arm_type(X, electromagnetic))]).

domain_test_alternatives(attr(role_type(P, sales)), [attr(role_type(P, engineer)), attr(role_type(P, manager))]).
domain_test_alternatives(attr(role_type(P, engineer)), [attr(role_type(P, sales)), attr(role_type(P, manager))]).
domain_test_alternatives(attr(role_type(P, manager)), [attr(role_type(P, sales)), attr(role_type(P, engineer))]).

domain_test_alternatives(attr(obj_weight(X, heavy)), [attr(obj_weight(X, light))]).
domain_test_alternatives(attr(obj_weight(X, light)), [attr(obj_weight(X, heavy))]).

domain_test_alternatives(attr(surface(X, brittle)), [attr(surface(X, hard))]).
domain_test_alternatives(attr(surface(X, hard)), [attr(surface(X, brittle))]).

domain_test_alternatives(fluent(labelled(X,true)), [fluent(labelled(X,false))]) :- !.
domain_test_alternatives(fluent(labelled(X,false)), [fluent(labelled(X,true))]) :- !.
domain_test_alternatives(fluent(item_status(X,damaged)), [fluent(item_status(X,intact))]) :- !.
domain_test_alternatives(fluent(item_status(X,intact)), [fluent(item_status(X,damaged))]) :- !.

domain_test_alternatives(N, Return) :-
	N=fluent(_),
	select(N,
	[fluent(loc(X, office)), fluent(loc(X, library)), fluent(loc(X, workshop)), fluent(loc(X, kitchen)), fluent(in_hand(p1, X)), fluent(in_hand(p2, X)), fluent(in_hand(p3, X)), fluent(in_hand(rob1, X))],
	Return).
% Note that this can return some invalid values, but these functions are only used to swap in and out literals, which is always followed by testing for 'physicalConstraintsViolated'


domain_test_alternatives(attr(Literal), Alts) :-
	domain(attr(Literal)), attr_alts(List), select(Literal, List, Alts), !. % Assume it's only in one list.
domain_test_alternatives(fluent(Literal), Alts) :-
	domain(fluent(Literal)), fluent_alts(List), select(Literal, List, Alts), !. % Assume it's only in one list.
	


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

% 'serve':

% Positive affordance - action succeeds despite (damaged + non-engineer)
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(fluent(item_status(Obj, damaged))),
	not(currentState(attr(role_type(P, engineer)))),
	% Affordance: Labelled
	currentState(fluent(labelled(Obj, true))),
	%
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	% Note that this exception doesn't result in the causal law 'serve unlabelled object to salesperson makes it labelled' applying,
	% because it requires the object already be labelled.
	% However, in other more general cases I'll have to be careful.
	% Moving to an ASP-like distributed representation for causal laws should fix this.
	!.

% "An object should not be served if damaged, except to an engineer" [negative affordance]
applyActionToStateFinal(serve(_R, Obj, P)) :-
	currentState(fluent(item_status(Obj, damaged))),
	not(currentState(attr(role_type(P, engineer)))),
	!.
% Serving an object to a salesperson
applyActionToStateFinal(serve(R, Obj, P)) :-
	currentState(attr(role_type(P, sales))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(P, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	not(currentState(fluent(in_hand(P, _)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	assert(currentState(fluent(in_hand(P, Obj)))),
	retract(currentState(fluent(labelled(Obj,_)))),
	assert(currentState(fluent(labelled(Obj,true)))),
	!.
	
% 'pickup':

% "Can't pick up a heavy object with a weak arm."
applyActionToStateFinal(pickup(R, Obj)) :-
	currentState(attr(obj_weight(Obj, heavy))),
	currentState(attr(arm_type(R, electromagnetic))),
	!.
	
% Breaking a brittle object
applyActionToStateFinal(putdown(R, Obj)) :-
	currentState(attr(surface(Obj, brittle))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	retract(currentState(fluent(item_status(Obj,_)))),
	assert(currentState(fluent(item_status(Obj,damaged)))),
	!.

% 'affix_label':

% Positive affordance - action succeeds despite brittle
applyActionToStateFinal(affix_label(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(attr(surface(Obj, brittle))),
	% Affordance: Electromagnetic & Heavy
	currentState(attr(arm_type(R, electromagnetic))),
	currentState(attr(obj_weight(Obj, heavy))),
	%
	currentState(fluent(labelled(Obj, false))),
	retract(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
	!.

% "An object can only be labelled if it has a hard surface"
applyActionToStateFinal(affix_label(_R, Obj)) :-
	currentState(attr(surface(Obj, brittle))),
	!.

% "A damaged object should not be labelled by a pneumatic arm"
applyActionToStateFinal(affix_label(R, Obj)) :-
	currentState(fluent(item_status(Obj, damaged))),
	currentState(attr(arm_type(R, pneumatic))),
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

% Succeeded putdown
applyActionToStateFinal(putdown(R, Obj)) :-
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(in_hand(R, Obj))),
	assert(currentState(fluent(loc(Obj, Loc)))),
	retract(currentState(fluent(in_hand(R, Obj)))),
	!.

% Succeeded label affix
applyActionToStateFinal(affix_label(R, Obj)) :-
	currentState(attr(surface(Obj, hard))),
	currentState(fluent(loc(R, Loc))),
	currentState(fluent(loc(Obj, Loc))),
	currentState(fluent(labelled(Obj, false))),
	retract(currentState(fluent(labelled(Obj, false)))),
	assert(currentState(fluent(labelled(Obj, true)))),
	!.

applyActionToStateFinal(donothing) :- !.

applyActionToStateFinal(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	noiseChancePercent(Noise), % Noise is likely to blame, because it can set up impossible situations - ignore it
	((Noise > 0) -> true ; trace).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actionDescription(move(Robot, Destination)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(location(Destination))).
impossible_if(move(Robot, Destination), 10) :-
	currentState(fluent(loc(Robot, Destination))).

actionDescription(putdown(Robot, Object)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(item(Object))).
%impossible_if(putdown(_Robot, Object), 20) :-
	%currentState(attr(surface(Object, brittle))). %%%%%%%%%%%%%%%%%%%%%%% !!! 1
	% TODO fix this

impossible_if(putdown(Robot, Object), 21) :-
	not(currentState(fluent(in_hand(Robot, Object)))).

actionDescription(donothing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check commenting for whether particular different axioms are targeted or known

actionDescription(serve(Robot, Obj, Person)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(item(Obj))),
	currentState(attr(person(Person))).
impossible_if(serve(Robot, _Obj, Person), 30) :-
	not((	currentState(fluent(loc(Robot, Location))),
			currentState(fluent(loc(Person, Location))) )).
impossible_if(serve(_Robot, Obj, Person), 31) :-
	not(currentState(attr(role_type(Person, engineer)))),
	not(currentState(fluent(item_status(Obj, intact)))). %%%%%%%%%%%%%%%%%%%%%%% !!! 2
impossible_if(serve(Robot, Obj, _Person), 32) :-
	not(currentState(fluent(in_hand(Robot, Obj)))).
impossible_if(serve(_Robot, _Obj, Person), 33) :-
	currentState(fluent(in_hand(Person, _))).
%impossible_if(serve(_Robot, Obj, Person), 34) :-
	%( not(currentState(attr(role_type(Person, sales)))) ; currentState(fluent(labelled(Obj, true))) ). %%%%%%%%%%%%%%%%%%%%%%% !!! 3
	% TODO fix this
	
actionDescription(pickup(Robot, Object)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(item(Object))).
impossible_if(pickup(Robot, Object), 40) :-
	not((	currentState(fluent(loc(Robot, Loc))),
			currentState(fluent(loc(Object, Loc))) )).
impossible_if(pickup(Robot, Object), 41) :-
	currentState(attr(obj_weight(Object, heavy))),
	currentState(attr(arm_type(Robot, electromagnetic))). %%%%%%%%%%%%%%%%%%%%%%% !!! 4
impossible_if(pickup(_Robot, Object), 42) :-
	currentState(fluent(in_hand(_, Object))).
impossible_if(pickup(Robot, _Object), 43) :-
	currentState(fluent(in_hand(Robot, _))).

actionDescription(affix_label(Robot, Object)) :-
	currentState(attr(robot(Robot))),
	currentState(attr(item(Object))). % (currentState(attr(cup(Object))) ; currentState(attr(book(Object))) ; currentState(attr(printer(Object)))),
impossible_if(affix_label(Robot, Object), 50) :-
	not((	currentState(fluent(loc(Robot, Loc))),
			currentState(fluent(loc(Object, Loc))) )).
impossible_if(affix_label(_Robot, Object), 51) :-
	currentState(fluent(labelled(Object, true))).
impossible_if(affix_label(_Robot, Object), 52) :-
	currentState(attr(surface(Object, brittle))). %%%%%%%%%%%%%%%%%%%%%%% !!! 5
impossible_if(affix_label(Robot, Object), 53) :-
	currentState(fluent(item_status(Object, damaged))),
	currentState(attr(arm_type(Robot, pneumatic))). %%%%%%%%%%%%%%%%%%%%%%% !!! 6
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Observed unexpected state: Fluents depend on target action
setObservedUnexpectedState :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	unexpectedStateFluents(S),
	assertFluents(S).

% The state is reset at random for each sequence of episodes.
resetStateAtRandom :-
	assert(lastActionWas(none)),
	retractall(currentState(fluent(_))),
	retractall(step(_)),
	assert(step(1)),
	setupFluentsRandomly,
	!.

setupFluentsRandomly :-
	% 1. Do entity locations independently
	setupEntityL(rob1), setupEntityL(p1), setupEntityL(p2), setupEntityL(p3),
	% 2. Do fixed object locations
	assert(currentState(fluent(loc(tab1,workshop)))),
	assert(currentState(fluent(loc(shelf1,library)))),
	assert(currentState(fluent(loc(shelf2,kitchen)))),
	assert(currentState(fluent(loc(desk1,office)))),
	% 3. Any small object is labelled 1/2 of the time
	randomlyLabel,
	% 4. Objects are either intact or damaged
	setObjStatusesRandomly,
	% 5. Do non-fixed object locations dependently, because only one can be in a hand
	% Done last, as it tests state consistency:
	setObjLocationsRandomlyUntilValid([cup1,book1,prin1]),
	!.
	
randomlyLabel :-
	retractall(currentState(fluent(labelled(_,_)))),
	randomlyLabelObject([cup1, prin1, book1]).
randomlyLabelObject([]).
randomlyLabelObject([A|B]) :-
	random(R),
	(R < 0.5 -> Bool = true ; Bool = false),
	assert(currentState(fluent(labelled(A,Bool)))),
	randomlyLabelObject(B).


setupEntityL(Entity) :-
	List1 = [loc(Entity, office), loc(Entity, workshop), loc(Entity, kitchen), loc(Entity, library)],
	assertOneFluentAtRandom(List1).

	
setObjStatusesRandomly :-
	retractall(currentState(fluent(item_status(_,_)))),
	StatusAlts = [damaged, intact],
	random_member(S1, StatusAlts),
	random_member(S2, StatusAlts),
	random_member(S3, StatusAlts),
	assertFluents([item_status(cup1,S1), item_status(book1,S2), item_status(prin1,S3)]).
	
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
	tryalllocs([p1,p2,p3,rob1,cup1,book1,prin1],[p1,p2,p3,rob1],[workshop,library,kitchen,office],[],List1),
	append(List1, [loc(tab1,workshop), loc(shelf1,library), loc(shelf2,kitchen), loc(desk1,office)], List2),
	tryallstatuspermutations([cup1,book1,prin1], [], PermutationOfAllStatuses),
	append(List2, PermutationOfAllStatuses, List3),
	tryalllabelpermutations([cup1,book1,prin1], [], PermutationOfAllLabels),
	append(List3, PermutationOfAllLabels, List).

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

tryallstatuspermutations([], Return, Return).
tryallstatuspermutations([A|B], Current, PermutationOfAllLabels) :-
	member(Choice, [item_status(A,damaged), item_status(A,intact)]),
	append([Choice], Current, NewCurrent),
	tryallstatuspermutations(B, NewCurrent, PermutationOfAllLabels).
	
tryalllabelpermutations([], Return, Return).
tryalllabelpermutations([A|B], Current, PermutationOfAllLabels) :-
	member(Choice, [labelled(A,true), labelled(A,false)]),
	append([Choice], Current, NewCurrent),
	tryalllabelpermutations(B, NewCurrent, PermutationOfAllLabels).
	
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
			cup(cup1), object(cup1), 
			item(cup1), item(book1), item(prin1), 
			book(book1), object(book1), printer(prin1), object(prin1),
			desk(desk1), object(desk1), table(tab1), object(tab1),
			shelf(shelf1), object(shelf1), shelf(shelf2), object(shelf2),
			furniture(tab1), furniture(desk1), furniture(shelf1), furniture(shelf2) ],
	assertAtts(Stats),
	setupAffordancePropertiesRandomly.
	
setupAffordancePropertiesRandomly :- 
	ArmAlts = [pneumatic,electromagnetic],
	random_member(Arm, ArmAlts),
	assertAtts([arm_type(rob1,Arm)]),
	%
	RTAlts = [engineer, manager, sales],
	random_member(RA, RTAlts),
	random_member(RB, RTAlts),
	random_member(RC, RTAlts),
	assertAtts([role_type(p1,RA), role_type(p2,RB), role_type(p3,RC)]),
	%
	WAlts = [heavy, light],
	random_member(W1, WAlts),
	%random_member(W2, WAlts),
	random_member(W3, WAlts),
	random_member(W4, WAlts),
	random_member(W5, WAlts),
	random_member(W6, WAlts),
	random_member(W7, WAlts),
	random_member(W8, WAlts),
	assertAtts([obj_weight(cup1,W1), obj_weight(tab1,W3), obj_weight(shelf1,W4), obj_weight(shelf2,W5), obj_weight(desk1,W6), obj_weight(book1,W7), obj_weight(prin1,W8)]),
	SurAlts = [hard, brittle],
	random_member(Sur1, SurAlts),
	%random_member(Sur2, SurAlts),
	random_member(Sur3, SurAlts),
	random_member(Sur4, SurAlts),
	assertAtts([surface(cup1,Sur1), surface(book1,Sur3), surface(prin1,Sur4)])
	.

	

% I have a generic form of this elsewhere that I need to port over
	
change_att_value(_, attr(role_type(P,CurrentRole)), Return) :-
	Roles1 = [engineer, manager, sales],
	select(CurrentRole, Roles1, Roles2),
	random_member(NewVal, Roles2),
	Return = attr(role_type(P,NewVal)),
	!.
	
change_att_value(_, attr(obj_weight(N,X)), Return) :-
	Ns1 = [heavy, light],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(obj_weight(N,NewVal)),
	!.
change_att_value(_, attr(surface(N,X)), Return) :-
	Ns1 = [hard, brittle],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(surface(N,NewVal)),
	!.
change_att_value(_, attr(arm_type(N,X)), Return) :-
	Ns1 = [pneumatic, electromagnetic],
	select(X, Ns1, Ns2),
	random_member(NewVal, Ns2),
	Return = attr(arm_type(N,NewVal)),
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
%domain(attr(cup(cup2))).
domain(attr(book(book1))).
domain(attr(printer(prin1))).
domain(attr(shelf(shelf1))).
domain(attr(shelf(shelf2))).
domain(attr(desk(desk1))).
domain(attr(table(tab1))).
domain(attr(entity(E))) :- domain(attr(robot(E))) ; domain(attr(person(E))).
domain(attr(furniture(O))) :- domain(attr(shelf(O))) ; domain(attr(desk(O))) ; domain(attr(table(O))).
domain(attr(item(O))) :- domain(attr(cup(O))) ; domain(attr(book(O))) ; domain(attr(printer(O))).
domain(attr(object(O))) :- domain(attr(item(O))) ; domain(attr(furniture(O))).
domain(attr(role_type(P,R))) :- domain(attr(person(P))), member(R, [sales, engineer, manager]).
%%%domain(attr(room_type(R,T))) :- domain(attr(location(R))), member(T, [workshop, kitchen, office, library]).
domain(attr(obj_weight(O,S))) :- domain(attr(object(O))), member(S, [light, heavy]).
domain(attr(surface(O,S))) :- domain(attr(item(O))), member(S, [hard, brittle]). 
domain(attr(arm_type(R,S))) :- domain(attr(robot(R))), member(S, [pneumatic, electromagnetic]).

% Permissible fluents - (not actually needed??)
domain(fluent(loc(X,Y))) :- domain(attr(entity(X))), domain(attr(location(Y))).
domain(fluent(loc(X,Y))) :- domain(attr(object(X))), domain(attr(location(Y))).
domain(fluent(in_hand(E,O))) :- domain(attr(entity(E))), domain(attr(item(O))).
domain(fluent(item_status(O,S))) :- domain(attr(item(O))), member(S, [intact, damaged]).
domain(fluent(labelled(O,Bool))) :- domain(attr(item(O))), member(Bool, [true,false]).

% Permissible actions
domain(action(pickup(R,O))) :- domain(attr(robot(R))), domain(attr(item(O))).
domain(action(putdown(R,O))) :- domain(attr(robot(R))), domain(attr(item(O))).
domain(action(move(R,L))) :- domain(attr(robot(R))), domain(attr(location(L))).
domain(action(serve(R,O,P))) :- domain(attr(robot(R))), domain(attr(person(P))), domain(attr(item(O))).
domain(action(affix_label(R,O))) :- domain(attr(robot(R))), domain(attr(item(O))).


% STATE CONSTRAINTS

physicalConstraintsViolated :- currentState(fluent(loc(O,L1))), currentState(fluent(loc(O,L2))), L1 \= L2. % One thing in two places
physicalConstraintsViolated :- currentState(fluent(loc(O,_))), currentState(fluent(in_hand(_,O))). % Anything simultaneously in hand and at a place
physicalConstraintsViolated :- currentState(fluent(in_hand(H1,O))), currentState(fluent(in_hand(H2,O))), H1 \= H2. % Anything simultaneously in two different hands
physicalConstraintsViolated :- currentState(fluent(in_hand(H,O1))), currentState(fluent(in_hand(H,O2))), O1 \= O2. % Same entity has two things in hand
physicalConstraintsViolated :- (currentState(attr(robot(E))) ; currentState(attr(person(E)))), not(currentState(fluent(loc(E,_)))). % An entity not at a place
physicalConstraintsViolated :- currentState(attr(object(O))), not(currentState(fluent(loc(O,_)))), not(currentState(fluent(in_hand(_,O)))). % An object neither in_hand nor at a place
physicalConstraintsViolated :- currentState(fluent(labelled(O,B1))), currentState(fluent(labelled(O,B2))), B1 \= B2. % Labelled and not labelled
physicalConstraintsViolated :- currentState(fluent(item_status(O,B1))), currentState(fluent(item_status(O,B2))), B1 \= B2. % Damaged and intact
physicalConstraintsViolated :- currentState(attr(item(O))), not(currentState(fluent(labelled(O,_)))). % No boolean value for a small object being labelled






% Ground truth checker. Returns a number identifying the target axiom, or returns it back for "does not match any target axiom", or returns ignore_axiom

% 1. brittle object damaged when put down [CAUSAL LAW]
domainAxiomClassifier([ [attr(surface(prin1, brittle))], [] ], 1) :- domainGoalAction(putdown(rob1,prin1)), !.
domainAxiomClassifier([ [], [attr(surface(prin1, hard))] ], 1) :- domainGoalAction(putdown(rob1,prin1)), !.

% Special case - already damaged
domainAxiomClassifier([ [fluent(item_status(prin1, damaged))], [] ], ignore_axiom) :- domainGoalAction(putdown(rob1,prin1)), !.
domainAxiomClassifier([ [], [fluent(item_status(prin1, intact))] ], ignore_axiom) :- domainGoalAction(putdown(rob1,prin1)), !.

% 2. Damaged object, not served to engineer [EXECUTABILITY CONDITION]
domainAxiomClassifier([ [], [attr(role_type(p1,engineer)), fluent(item_status(cup1, intact))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(cup1, damaged))], [attr(role_type(p1,engineer))] ], 2) :- domainGoalAction(serve(rob1,cup1,p1)), !.

% 3. Unlabelled object served to a sales person becomes labelled [CAUSAL LAW]
domainAxiomClassifier([ [attr(role_type(p2, sales))], [] ], 3) :- domainGoalAction(serve(rob1,book1,p2)), !.
domainAxiomClassifier([ [], [attr(role_type(p2, engineer)),attr(role_type(p2, manager))] ], 3) :- domainGoalAction(serve(rob1,book1,p2)), !.

% Special case - already labelled
domainAxiomClassifier([ [fluent(labelled(book1, true))], [] ], ignore_axiom) :- domainGoalAction(serve(rob1,book1,p2)), !.
domainAxiomClassifier([ [], [fluent(labelled(book1, false))] ], ignore_axiom) :- domainGoalAction(serve(rob1,book1,p2)), !.

% 4. Heavy object cannot be picked up by an electromagnetic arm [NEGATIVE AFFORDANCE]
domainAxiomClassifier([ [],[attr(arm_type(rob1,pneumatic)),attr(obj_weight(prin1,light))] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)),attr(obj_weight(prin1,heavy))],[] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))],[attr(obj_weight(prin1,light))] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.
domainAxiomClassifier([ [attr(obj_weight(prin1,heavy))],[attr(arm_type(rob1,pneumatic))] ], 4) :- domainGoalAction(pickup(rob1,prin1)), !.

% 5. No hard surface - label [EXECUTABILITY CONDITION]
domainAxiomClassifier([ [attr(surface(prin1, brittle))], [] ], 5) :- domainGoalAction(affix_label(rob1,prin1)), !.
domainAxiomClassifier([ [], [attr(surface(prin1, hard))] ], 5) :- domainGoalAction(affix_label(rob1,prin1)), !.

% 6. Damaged - label with pneumatic arm [NEGATIVE AFFORDANCE]
domainAxiomClassifier([ [],[attr(arm_type(rob1,electromagnetic)),fluent(item_status(book1,intact))] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic)),fluent(item_status(book1,damaged))],[] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,pneumatic))],[fluent(item_status(book1,intact))] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.
domainAxiomClassifier([ [fluent(item_status(book1,damaged))],[attr(arm_type(rob1,electromagnetic))] ], 6) :- domainGoalAction(affix_label(rob1,book1)), !.

%%%%%%%%%%%%%%%%%%%%

% 7. Positive affordance - even when cup1 is brittle [5], under conditions [object is heavy, robot arm is electromagnetic], 'affix_label(rob1, cup1)' is possible
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)),attr(obj_weight(cup1,heavy))], [] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))], [attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(obj_weight(cup1,heavy))], [attr(arm_type(rob1,pneumatic))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,pneumatic)),attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
% Positive affordances including partial information from their associated executability condition
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(cup1,heavy)), attr(surface(cup1, brittle))], [] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(surface(cup1, brittle))], [attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(obj_weight(cup1,heavy)), attr(surface(cup1, brittle))], [attr(arm_type(rob1,pneumatic))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(surface(cup1, brittle))], [attr(arm_type(rob1,pneumatic)), attr(obj_weight(cup1,light))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic)), attr(obj_weight(cup1,heavy))], [attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(arm_type(rob1,electromagnetic))], [attr(obj_weight(cup1,light)), attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [attr(obj_weight(cup1,heavy))], [attr(arm_type(rob1,pneumatic)), attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.
domainAxiomClassifier([ [], [attr(arm_type(rob1,pneumatic)), attr(obj_weight(cup1,light)), attr(surface(cup1, hard))] ], 7) :- domainGoalAction(affix_label(rob1,cup1)), !.

% 8. Positive affordance - even when a damaged object is served to a non-engineer [2], under conditions [object is labelled], 'serve(rob1,book1,p1)' is possible
domainAxiomClassifier([ [fluent(labelled(book1, true))], [] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
% Positive affordances including partial information from their associated executability condition
domainAxiomClassifier([ [fluent(labelled(book1, true))], [attr(role_type(p1,engineer))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [attr(role_type(p1,engineer)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
%
domainAxiomClassifier([ [fluent(labelled(book1, true))], [fluent(item_status(book1, intact))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [fluent(item_status(book1, intact)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(labelled(book1, true))], [attr(role_type(p1,engineer)), fluent(item_status(book1, intact))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [], [attr(role_type(p1,engineer)), fluent(item_status(book1, intact)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
%
domainAxiomClassifier([ [fluent(item_status(book1, damaged)), fluent(labelled(book1, true))], [] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(book1, damaged))], [fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(book1, damaged)), fluent(labelled(book1, true))], [attr(role_type(p1,engineer))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.
domainAxiomClassifier([ [fluent(item_status(book1, damaged))], [attr(role_type(p1,engineer)), fluent(labelled(book1, false))] ], 8) :- domainGoalAction(serve(rob1,book1,p1)), !.



%%%%%%%%%%%%%%%%%%%%

% Catch case: Everything else
domainAxiomClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- !.

%%%%%%%%%%%%%%%%%%%%


	
% In this version of the domain, we are trying to learn a causal law:
/* "A brittle object becomes damaged when put down" */
cached :- 
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)),
	assert(allValidTests([
		attr(arm_type(rob1,electromagnetic)),attr(arm_type(rob1,pneumatic)),
		attr(obj_weight(prin1,heavy)),attr(obj_weight(prin1,light)),
		attr(surface(prin1,brittle)),attr(surface(prin1,hard)),
		fluent(in_hand(rob1,prin1)),
		fluent(item_status(prin1,damaged)),fluent(item_status(prin1,intact)),
		fluent(labelled(prin1,false)),fluent(labelled(prin1,true)),
		fluent(loc(rob1,kitchen)),fluent(loc(rob1,library)),fluent(loc(rob1,office)),fluent(loc(rob1,workshop)),
		fluent(loc(prin1,kitchen)),fluent(loc(prin1,library)),fluent(loc(prin1,office)),fluent(loc(prin1,workshop)),
		action(pickup(rob1,prin1)),action(putdown(rob1,prin1)),action(move(rob1,office)),action(move(rob1,workshop)),action(move(rob1,kitchen)),action(move(rob1,library)),action(affix_label(rob1,prin1))
		])),
	assert(num_possible_attribute_configs(8)),
	assert(usableActionList([
		action(pickup(rob1, prin1)), action(putdown(rob1, prin1)), action(move(rob1, office)), action(move(rob1, workshop)), action(move(rob1, kitchen)), action(move(rob1, library)), action(affix_label(rob1, prin1))])).
domainGoalAction(putdown(rob1,prin1)).
unexpectedResult([fluent(item_status(prin1,damaged))]).
unexpectedStateFluents([loc(p1,workshop),loc(p2,library),loc(p3,office),loc(rob1,workshop),in_hand(rob1,prin1),
	loc(book1,library),loc(cup1,office),loc(shelf1,library),loc(shelf2,kitchen),loc(desk1,office),loc(tab1,workshop),
	labelled(cup1,false),labelled(book1,false),labelled(prin1,false),
	item_status(cup1,intact),item_status(book1,intact),item_status(prin1,intact)
	]). % An actual state from which unexpected outcomes occurred. Others are possible.

