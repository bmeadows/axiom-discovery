
:- dynamic attemptsCount/1.

domain_specified_end :- attemptsCount(X), X >= 10.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_test_alternatives(attr(has_shape(B, cube)), [attr(has_shape(B, prism)), attr(has_shape(B, cuboid))]).
domain_test_alternatives(attr(has_shape(B, prism)), [attr(has_shape(B, cube)), attr(has_shape(B, cuboid))]).
domain_test_alternatives(attr(has_shape(B, cuboid)), [attr(has_shape(B, prism)), attr(has_shape(B, cube))]).
domain_test_alternatives(attr(has_size(B, small)), [attr(has_size(B, big)), attr(has_size(B, medium))]).
domain_test_alternatives(attr(has_size(B, big)), [attr(has_size(B, small)), attr(has_size(B, medium))]).
domain_test_alternatives(attr(has_size(B, medium)), [attr(has_size(B, small)), attr(has_size(B, big))]).
domain_test_alternatives(attr(has_color(B, red)), [attr(has_color(B, blue)), attr(has_color(B, green))]).
domain_test_alternatives(attr(has_color(B, blue)), [attr(has_color(B, red)), attr(has_color(B, green))]).
domain_test_alternatives(attr(has_color(B, green)), [attr(has_color(B, blue)), attr(has_color(B, red))]).

domain_test_alternatives(fluent(on(b1, b2)), [fluent(on(b1, b3)), fluent(on(b1, table1))]).
domain_test_alternatives(fluent(on(b1, b3)), [fluent(on(b1, b2)), fluent(on(b1, table1))]).
domain_test_alternatives(fluent(on(b1, table1)), [fluent(on(b1, b2)), fluent(on(b1, b3))]).

domain_test_alternatives(fluent(on(b2, b3)), [fluent(on(b2, b1)), fluent(on(b2, table1))]).
domain_test_alternatives(fluent(on(b2, b1)), [fluent(on(b2, b3)), fluent(on(b2, table1))]).
domain_test_alternatives(fluent(on(b2, table1)), [fluent(on(b2, b3)), fluent(on(b2, b1))]).

domain_test_alternatives(fluent(on(b3, b1)), [fluent(on(b3, b2)), fluent(on(b3, table1))]).
domain_test_alternatives(fluent(on(b3, b2)), [fluent(on(b3, b1)), fluent(on(b3, table1))]).
domain_test_alternatives(fluent(on(b3, table1)), [fluent(on(b3, b1)), fluent(on(b3, b2))]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainGoalAction(move(b2,b1)).

establishGoalState :-
	domainGoalAction(ACT),
	% Unexpected observations
	assert(goalState([
		fluent(on(b2,table1)),
		not(fluent(on(_,b1))),
		lastActionWas(ACT)
		])).

:- establishGoalState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

applyActionToState(A) :-
	attemptsCount(X),
	Y is X + 1,
	retractall(attemptsCount(_)),
	assert(attemptsCount(Y)),
	applyActionToStateBW(A),
	applyNoiseWhereAppropriate.
	
% failed move
applyActionToStateBW(move(Block1, Block2)) :-
	currentState(attr(block(Block2))), % Block2 is a block not table
	not(currentState(attr(has_shape(Block2, cube)))),   % The first permissible platform
	not(currentState(attr(has_shape(Block2, cuboid)))), % The second permissible platform
	retractall(currentState(fluent(on(Block1, _)))),
	assert(currentState(fluent(on(Block1, table1)))),
	!.
% failed move
applyActionToStateBW(move(Block1, Block2)) :-
	currentState(attr(block(Block2))), % Block2 is a block not table
	currentState(attr(has_size(Block1, big))),
	currentState(attr(has_size(Block2, small))),
	retractall(currentState(fluent(on(Block1, _)))),
	assert(currentState(fluent(on(Block1, table1)))),
	!.
	
% succeeded move
applyActionToStateBW(move(Block1, Dest)) :-
	retractall(currentState(fluent(on(Block1, _)))),
	assert(currentState(fluent(on(Block1, Dest)))),
	!.

applyActionToStateBW(donothing) :- !.

applyActionToStateBW(Something) :-
	writef('Note: Unexpected oracle failure.\n'),
	writef(Something), nl,
	noiseChancePercent(Noise), % Noise is likely to blame, because it can set up impossible situations - ignore it
	((Noise > 0) -> true ; trace).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validAction(donothing).

validAction(move(Block1, Block2)) :-
	currentState(attr(block(Block1))),
	currentState(attr(block(Block2))),
	not(currentState(fluent(on(_,Block1)))),
	not(currentState(fluent(on(_,Block2)))),
	Block1 \= Block2.

validAction(move(Block1, Tab)) :-
	currentState(attr(block(Block1))),
	currentState(attr(table(Tab))),
	not(currentState(fluent(on(_,Block1)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Observed failure state: Moving b2 onto b1 fails when all three blocks are on the table.
setObservedFailureState :-
	retractall(currentState(fluent(_))),
	retractall(attemptsCount(_)),
	assert(attemptsCount(0)),
	S = [ on(b1,table1), on(b2,table1), on(b3,table1) ],
	assertFluents(S).

% Returns all physical states, even if they break constraints
getTheoreticalStatePermutation(List) :-
	tryalllocs([b1,b2,b3],[b1,b2,b3,table1],[],List).

tryalllocs([],_List2,Return,Return).
tryalllocs([A|B],List2,Working,Return) :-
	member(X,List2),
	append(Working,[on(A,X)],New),
	tryalllocs(B,List2,New,Return).

resetStateAtRandom :-
	retractall(currentState(fluent(_))),
	retractall(attemptsCount(_)),
	assert(attemptsCount(0)),
	
	AllStates = [ 
		[ on(b1,table1), on(b2,table1), on(b3,table1) ],
		%
		[ on(b1,b2), on(b2,table1), on(b3,table1) ],
		[ on(b1,b3), on(b2,table1), on(b3,table1) ],
		%
		[ on(b1,table1), on(b2,b1), on(b3,table1) ],
		[ on(b1,table1), on(b2,b3), on(b3,table1) ],
		%
		[ on(b1,table1), on(b2,table1), on(b3,b1) ],
		[ on(b1,table1), on(b2,table1), on(b3,b2) ],
		%
		[ on(b1,b2), on(b2,b3), on(b3,table1) ],
		[ on(b1,b3), on(b3,b2), on(b2,table1) ],
		[ on(b2,b1), on(b1,b3), on(b3,table1) ],
		[ on(b2,b3), on(b3,b1), on(b1,table1) ],
		[ on(b3,b1), on(b1,b2), on(b2,table1) ],
		[ on(b3,b2), on(b2,b1), on(b1,table1) ]
	],
	random_member(S, AllStates), assertFluents(S).

setRandomInitialObjectConfig :-
	retractall(currentState(attr(_))),
	Colours = [red, green, blue],
	random_member(C1, Colours),
	random_member(C2, Colours),
	random_member(C3, Colours),
	
	Stats = [block(b1), block(b2), block(b3), table(table1), has_color(b1,C1), has_color(b2,C2), has_color(b3,C3)],
	assertAtts(Stats),
	
	Shapes = [prism, cuboid, cube],
	random_member(S1, Shapes),
	random_member(S2, Shapes),
	random_member(S3, Shapes),
	ShStats = [has_shape(b1,S1), has_shape(b2,S2), has_shape(b3,S3)],
	assertAtts(ShStats),
	%
	Sizes = [big, small, medium],
	random_member(I1, Sizes),
	random_member(I2, Sizes),
	random_member(I3, Sizes),
	SiStats = [has_size(b1,I1), has_size(b2,I2), has_size(b3,I3)],
	assertAtts(SiStats),
	!.
	
%

change_att_value(_, attr(has_color(Block,Value)), Return) :-
	Colours1 = [red, green, blue],
	select(Value, Colours1, Colours2),
	random_member(NewVal, Colours2),
	Return = attr(has_color(Block,NewVal)),
	!.

change_att_value(_, Old, attr(has_size(Block,NN))) :-
	Old = attr(has_size(Block,small)), !,
	random_member(NN, [big, medium]).
change_att_value(_, Old, attr(has_size(Block,NN))) :-
	Old = attr(has_size(Block,medium)), !,
	random_member(NN, [big, small]).
change_att_value(_, Old, attr(has_size(Block,NN))) :-
	Old = attr(has_size(Block,big)), !,
	random_member(NN, [small, medium]).

change_att_value(_, Old, attr(has_shape(Block,NN))) :-
	Old = attr(has_shape(Block,cuboid)), !,
	random_member(NN, [cube, prism]).
change_att_value(_, Old, attr(has_shape(Block,NN))) :-
	Old = attr(has_shape(Block,prism)), !,
	random_member(NN, [cube, cuboid]).
change_att_value(_, Old, attr(has_shape(Block,NN))) :-
	Old = attr(has_shape(Block,cube)), !,
	random_member(NN, [cuboid, prism]).

% For other cases, e.g., class membership may be written as object attribute because it's non-fluent
change_att_value(_ElidedList, AnythingElse, AnythingElse).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainChangeObjectAtts(List) :-
	random_member(X,List), % Pick one literal to change at random
	% Note you never need to add or subtract object attribute literals
	select(X, List, ElidedList),
	change_att_value(ElidedList, X, Y), % Call a domain function making a valid change to something other than the original value
	retractall(currentState(X)),
	(Y == [] -> true ; assert(currentState(Y))).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Returns a number uniquely identifying the target affordance, or returns it back for "does not match any target affordance"
% 1. Shape
domainAffordanceClassifier([[], [attr(has_shape(b1, cube)), attr(has_shape(b1, cuboid))]], 1) :- domainGoalAction(move(b2,b1)), !.
domainAffordanceClassifier([[attr(has_shape(b1, prism))], []], 1) :- domainGoalAction(move(b2,b1)), !.
%domainAffordanceClassifier(YesNo, 1) :- !.
% 2. Size
domainAffordanceClassifier([[attr(has_size(b1, small))], [attr(has_size(b2, medium)), attr(has_size(b2, small))]], 2) :- domainGoalAction(move(b2,b1)), !.
domainAffordanceClassifier([[attr(has_size(b1, small)), attr(has_size(b2, big))], []], 2) :- domainGoalAction(move(b2,b1)), !.
domainAffordanceClassifier([[attr(has_size(b2, big))], [attr(has_size(b1, big)), attr(has_size(b1, medium))]], 2) :- domainGoalAction(move(b2,b1)), !.
% Catch case: Everything else
domainAffordanceClassifier([YesLiterals,NoLiterals], [YesLiterals,NoLiterals]) :- domainGoalAction(move(b2,b1)), !.


% Domain description

domain(attr(block(b1))).
domain(attr(block(b2))).
domain(attr(block(b3))).
domain(attr(table(table1))).
domain(attr(location(X))) :- domain(attr(block(X))) ; domain(attr(table(X))).
domain(attr(has_color(X,Y))) :- domain(attr(block(X))), member(Y, [red, blue, green]).
domain(attr(has_shape(X,Y)))  :- domain(attr(block(X))), member(Y, [prism, cube, cuboid]).
domain(attr(has_size(X,Y)))   :- domain(attr(block(X))), member(Y, [big, medium, small]).
%
domain(action(move(B,L))) :- domain(attr(block(B))), domain(attr(location(L))).

% 13 physical configurations for 3 blocks
% 19,683 object property configurations for 3 blocks

physicalConstraintsViolated :- currentState(fluent(on(B1,B2))), currentState(fluent(on(B1,B3))), B2 \= B3. % One block in two places
physicalConstraintsViolated :- currentState(fluent(on(B1,B2))), currentState(fluent(on(B3,B2))), B1 \= B3, not(currentState(attr(table(B2)))). % Two blocks in same non-table place
physicalConstraintsViolated :- currentState(fluent(on(B1,B1))). % Block on itself
physicalConstraintsViolated :- currentState(fluent(on(T,_))), currentState(attr(table(T))). % Table on other location
physicalConstraintsViolated :- currentState(attr(block(B))), not(currentState(fluent(on(B,_)))). % Block nowhere
physicalConstraintsViolated :- currentState(fluent(on(B1,B2))), currentState(fluent(on(B2,B1))). % Loop: Block1 on Block2 on Block1
physicalConstraintsViolated :- currentState(fluent(on(B1,B2))), currentState(fluent(on(B2,B3))), currentState(fluent(on(B3,B1))). % Loop: Block1 on Block2 on Block3 on Block1
physicalConstraintsViolated :- currentState(fluent(on(B1,B2))), currentState(fluent(on(B2,B3))), currentState(fluent(on(B3,B4))), currentState(fluent(on(B4,B1))). % Loop: Block1 on Block2 on Block3 on Block4 on Block1




/* Blocks world 'move' action */
cached :- 
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)),
	assert(allValidTests( [attr(has_color(b1,blue)),attr(has_color(b1,green)),attr(has_color(b1,red)),attr(has_color(b2,blue)),attr(has_color(b2,green)),
		attr(has_color(b2,red)),attr(has_color(b3,blue)),attr(has_color(b3,green)),attr(has_color(b3,red)),attr(has_shape(b1,cube)),attr(has_shape(b1,cuboid)),
		attr(has_shape(b1,prism)),attr(has_shape(b2,cube)),attr(has_shape(b2,cuboid)),attr(has_shape(b2,prism)),attr(has_shape(b3,cube)),attr(has_shape(b3,cuboid)),
		attr(has_shape(b3,prism)),attr(has_size(b1,big)),attr(has_size(b1,medium)),attr(has_size(b1,small)),attr(has_size(b2,big)),attr(has_size(b2,medium)),
		attr(has_size(b2,small)),attr(has_size(b3,big)),attr(has_size(b3,medium)),attr(has_size(b3,small)),fluent(on(b1,b3)),fluent(on(b2,table1)),
		fluent(on(b1,table1)),fluent(on(b2,b3)),action(move(b1,b1)),action(move(b1,b2)),action(move(b1,b3)),action(move(b1,table1)),action(move(b2,b1)),
		action(move(b2,b2)),action(move(b2,b3)),action(move(b2,table1)),action(move(b3,b1)),action(move(b3,b2)),action(move(b3,b3)),action(move(b3,table1))] )),
	assert(num_possible_attribute_configs( 19683 )),
	assert(usableActionList( [action(move(b1,b1)),action(move(b1,b2)),action(move(b1,b3)),action(move(b1,table1)),action(move(b2,b1)),
		action(move(b2,b2)),action(move(b2,b3)),action(move(b2,table1)),action(move(b3,b1)),action(move(b3,b2)),action(move(b3,b3)),action(move(b3,table1))] )).



