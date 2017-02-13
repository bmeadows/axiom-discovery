
:- dynamic goalState/1, precalculate/0.


relevantTestLiteral(Lit) :-
	relevantTestLitAction(Lit).
relevantTestLiteral(Lit) :-
	relevantTestLitAttr(Lit).
relevantTestLitAction(Lit) :-
	Lit = action(T),
	domain(Lit),
	relevantToTarg(T).
relevantTestLitAttr(Lit) :-
	Lit = attr(L),
	domain(Lit),
	L =.. [_Predicate|Args],
	Args \= [_SingleElement],
	Args \= [],
	relevantToTarg(L).
relevantToTarg(L) :-
	L =.. [_Predicate|Args],
	targetActionArgs(List),
	member(A, Args),
	member(A, List).
/*
relevantToTargetAction(L) :-
	L =.. [_Predicate, Object, _Value],
	targetActionArgs(List),
	member(Object, List).
*/

	
relevantDomainTestAlternatives(OptionList) :- 
	domain(attr(L)),
	L =.. [Predicate, Object, _Value],
	targetActionArgs(List),
	member(Object, List),
	findall(	[Predicate,Object,Val],
				(domain(attr(L1)), L1 =.. [Predicate,Object,Val]),
				ValueTriples),
	sort(ValueTriples,OptionList).
	% Note this results in doubling up, so need to reduce to a true set elsewhere
	
reduceOpsToNums(ListOfLists, Return) :-
	reduceOpsToNumbers(ListOfLists, [], Return).
reduceOpsToNumbers([], Return, Return) :- !.
reduceOpsToNumbers([H|T], Current, Return) :- 
	length(H,X),
	append([X], Current, Next),
	reduceOpsToNumbers(T, Next, Return).

multiplyOut([], Result, Result) :- !.
multiplyOut([H|T], N, Result) :-
	Next is H * N,
	multiplyOut(T, Next, Result).

getCurrentRelevantConfigurationOfAttributes(RelevantConfigList) :-
	allValidTests(TSTS), % Includes actions and fluents, too
	findall(attr(Attr),
		(currentState(attr(Attr)), member(attr(Attr), TSTS)), % Note that TSTS *already* has the attr() wrappers (amongst others)
		AttrList),
	sort(AttrList,RelevantConfigList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 7 February 2017

applyNoiseWhereAppropriate :-
	noiseChancePercent(0),
	!.
applyNoiseWhereAppropriate :-
	noiseChancePercent(N),
	random_between(1,100,Rand1),
	% N percent chance to change some fluent in domain
	((Rand1 =< N) -> switchAFluent ; true),
	!.
applyNoiseWhereAppropriate :- trace.

switchAFluent :-
	findall(fluent(Fl), currentState(fluent(Fl)), FList),
	random_member(F, FList),
	domain_test_alternatives(F, AltList),
	retract(currentState(F)),
	random_member(NewFluent, AltList),
	assert(currentState(NewFluent)),
	(physicalConstraintsViolated -> switchAFluent ; true). % Very important - just replacing with something from domain_test_alternatives can still result in state constraint violations.
	% That in turn causes things like learning a constraint that incorporates a physical constraint violation, which makes filter checking hang forever!
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 8 January 2017

resetStateInPrincipledWay :-
	setObservedFailureState,
	random(R1),
	random(R2),
	(R1<0.9 -> makeSingleFluentChange ; true),
	(R2<0.1 -> makeSingleFluentChange ; true),
	!,
	(physicalConstraintsViolated -> resetStateInPrincipledWay ; true).
	
makeSingleFluentChange :- 
	% Select a random fluent currentState(fluent(f)), then call swapOut(fluent(f))
	findall(F, (currentState(F), F=fluent(_)), List),
	random_member(Fluent,List),
	swapOut(Fluent),
	!.

assertFluents([]).
assertFluents([A|B]) :-
	assert(currentState(fluent(A))),
	assertFluents(B).

assertAtts([]).
assertAtts([A|B]) :-
	assert(currentState(attr(A))),
	assertAtts(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

precalculate :-
	retractall(objrel(_)),
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)),
	setRandomInitialObjectConfig,
	findRelevantLiteralSuperset(Action, Lits),
	adjustForRelevance(Lits, ReturnedTests, ReturnedObjectSet),
	assert(allValidTests(ReturnedTests)),
	calculateConfigSpaceSize(ReturnedObjectSet),
	retractall(objrel(_)).
	
% For each valid physical states from which the target ground action is permissible,
% Find the relevant literals to that physicalstate/action pair.
findRelevantLiteralSuperset(Action, Lits) :-
	findall(	Literals, 
				(someDomainPhysicalStateWithApplicableAction(Action, Literals)),
				Lits).

someDomainPhysicalStateWithApplicableAction(Action, Literals) :-
	getTheoreticalStatePermutation(ListRepresentation),
	retractall(currentState(fluent(_))),
	assertFluents(ListRepresentation),
	not(physicalConstraintsViolated),
	currentlyApplicableWithRelevantLits(Action, Literals).
	
currentlyApplicableWithRelevantLits(Action, Literals) :-
	% 1. Check it's applicable
	validAction(Action),
	% 2. Return all literals relevant to the physicalconfig / action combination
	getRelevantLits(Literals).

adjustForRelevance(Lits, ReturnedTests, ReturnedObjectSet) :-
	flatten(Lits,Lits2),
	list_to_set(Lits2,Lits3),
	addAllObjectAttributesAndActions(Lits3, ReturnedTests, ReturnedObjectSet).
	
% Returns literals relevant to the action, based on current physical state
% ALSO asserts names of relevant objects in "objrel(X)".
getRelevantLits(Set2) :-
	targetActionArgs(Set1), % 1. For the set1 of objects named in the action predicate;
	findall( 	F,
				(currentState(F), F=fluent(Content), Content =.. [_Pred|ArgList], length(ArgList, N), N>0, last(ArgList, Last), select(Last, ArgList, Remnant), allAreIn(Remnant, Set1) ),
				Set2
			), % 2. Find the set2 of all currently-true-in-state fluents such that the fluent's arguments are all in set1;
	findall( 	LastConstant,
				(member(fluent(Content),Set2), Content =.. [_Predicate|ArgList], last(ArgList, LastConstant) ),
				Set3
			), % 3. Let set3 be the set of constants mapped onto by set2;
	assertEachAsObjRel(Set3).
	
assertEachAsObjRel([]) :- !.
assertEachAsObjRel([A|B]) :-
	objrel(A),
	!,
	assertEachAsObjRel(B).
assertEachAsObjRel([A|B]) :-
	assert(objrel(A)),
	!,
	assertEachAsObjRel(B).

allAreIn(SetA, SetB) :-
	not( (member(X, SetA), not(member(X, SetB))) ).

addAllObjectAttributesAndActions(A, Return, ReturnedObjectSet) :-	
	findall(X, objrel(X), ListObs),
	targetActionArgs(AAs),
	append(ListObs, AAs, NewList), % Add the arguments of target action
	sort(NewList, ReturnedObjectSet),

	addAllRelevantActions(ReturnedObjectSet, A, NewSet),

	% At this point, we just need to find valid BDT tests, so find all relevant instantiated object properties
	% (While we also use "allValidTests(Tests)" to find meaningfully different obj configs, no changes are necessary for that)
	% First, get all valid instantiated multi-argument object properties whose n-1 arguments are all in ReturnedObjectSet.
	findall(	attr(ObjectProperty),
				( domain(attr(ObjectProperty)), ObjectProperty =.. [_Pred|Args], length(Args, N), N>1, last(Args, Last), select(Last, Args, Remnant), allAreIn(Remnant, ReturnedObjectSet) ),
				Props1),
	sort(Props1,Props2),
	append(Props2, NewSet, Return).

addAllRelevantActions(ObjectSet, Initial, Final) :-
	findall( action(Act),
			(	
				domain(action(Act)),
				Act =.. [_Pred|Args],
				not( (member(Arg,Args), not(member(Arg,ObjectSet))) ) % No arg is outside the set of relevant domain objects
			),
			RelActions ),
	% Keep this list separately, so that you can use it not only to determine tests in the BDT, but also prevent the learner ever trying actions outside it
	assert(usableActionList(RelActions)),
	append(Initial,RelActions,Final).


% 1. We have determined which objects are relevant, 'ObjectSet'.
% 2. All properties of a relevant object are relevant.
% 3. For each property that CAN pertain to an object, it is guaranteed that the function will give SOME value.
% 4. We can call domain(attr(X)) and check number of arguments is >1.
% 5. So, for each predicate, count
% predicate([unique instantiated arguments all in ObjectSet], final argument)
% ...and then multiply out for all predicates

singleMapping(ObjectSet, Pred, Remnant, Number) :-
	domain(attr(ObjectProperty)),
	ObjectProperty =.. [Pred|Args],
	length(Args, N), N>1,
	last(Args, Last), select(Last, Args, Remnant),
	allAreIn(Remnant, ObjectSet),
	findall(	Last2,
				(domain(attr(ObjectProperty2)), ObjectProperty2 =.. [Pred|Args2], length(Args2, N2), N2>1, last(Args2, Last2), select(Last2, Args2, Remnant)),
				SetOfThingsMappedOnto),
	sort(SetOfThingsMappedOnto,SetOfThingsMappedOnto2),
	length(SetOfThingsMappedOnto2, Number).

	
calculateConfigSpaceSize(ObjectSet) :-
	findall([Pred, Remn, Number], singleMapping(ObjectSet, Pred, Remn, Number), Collects),
	sort(Collects, Numbers),
	findall(Number, member([_,_,Number],Numbers), Final),
	list_multiply(Final, ResultTotal),
	assert(num_possible_attribute_configs(ResultTotal)).
	
list_multiply([], 0) :- !.
list_multiply(List, ResultTotal) :-
	multiply_out_list(List, 1, ResultTotal).
multiply_out_list([], Final, Final).
multiply_out_list([A|B], Working, Final) :-
	New is A * Working,
	multiply_out_list(B, New, Final).

/* Use 'cached' option to precalculate a single time, for batch testing, and then skip the precalculation stage for each individual run. */
%:- precalculate.
:- cached.



	
%--------------------------------------------------------------------------------------------------------------------
	
% resetStateAtRandom is a completely random physical configuration.
% setObservedFailureState is a completely determined physical configuration.

%--------------------------------------------------------------------------------------------------------------------

