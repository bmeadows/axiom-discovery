:- dynamic 
currentState/1, goalState/1, goalreached/0, episode_count/1, loud/0, quiet/0, current_learning_rate_parameter/1, initial_num_of_episodes/1,
currentId/1, root/1, leaf/1, parent/2, test/2, predicted_q_value/2, split_count/1, no_split_count/1, lastActionWas/1, domain_specified_end/0,
end_by_goal_counter/1, end_by_domain_counter/1, clause1count/1, clause2count/1, clause3count/1, clause4count/1, clause5count/1.

%:- include(domain_blocksworld_actionhistories).
:- include(domain_robot).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% PARAMETERISATION %%%

%episode_count(20).
%episode_count(1000).
%episode_count(2000).
%episode_count(5000).
episode_count(10000).
%episode_count(20000).
%episode_count(50000).
%episode_count(100000).
%episode_count(400000).

initial_learning_rate_parameter(0.85).
current_learning_rate_parameter(0.85).
%initial_learning_rate_parameter(0.00001).
%current_learning_rate_parameter(0.00001).

explore_parameter(0.5).

reward_pos(10).
reward_neg(0.01).

split_count(0).
no_split_count(0).

end_by_domain_counter(0).
end_by_goal_counter(0).
	
currentId(0).

clause1count(0).
clause2count(0).
clause3count(0).
clause4count(0).
clause5count(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tree predicates

% root(id).
% child_y(id, id1).
% child_n(id, id2).
% parent(child, parent).
% leaf(id).
% test(id, test). - 'test' can be action(Action) or fluent(Contents)
% predicted_q_value(leaf_id, value).
% leaf_test_stats(leaf_id, test, yes, count, sumOfQ, sumOfSquaredQ). - 'test' can be action(Action) or fluent(Contents)
% leaf_test_stats(leaf_id, test, no, count, sumOfQ, sumOfSquaredQ). - 'test' can be action(Action) or fluent(Contents)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

applyActionAndReportOracleOutcome(Action, RewardValue) :-
	goalState(GOAL),
	applyActionToStateAndUpdateHistory(Action),
	(goalAchieved(GOAL) -> (assert(goalreached), reward_pos(X), RewardValue = X) ; (reward_neg(Y), RewardValue = Y)).

% 1. Every element of goal is achieved
% 2. No element of current state is not in goal, i.e., exact match - TODO - removed this; revisit
goalAchieved(List) :-
	achieved(List).
	%,
	%not(( currentState(X), X=fluent(_), not(member(X,List)) )).
	
achieved([]) :-
	!.
achieved([A|B]) :-
	(currentState(A) ; static(A) ; derived(A) ;
		(A = not(derived(N)), not(derived(N))) ;
		(A = not(fluent(Z)), not(currentState(fluent(Z))))  ;
		(A = lastActionWas(X), lastActionWas(X))  ),
	!,
	achieved(B).

applyActionToStateAndUpdateHistory(Action) :-
	retractall(lastActionWas(_)),
	assert(lastActionWas(Action)),
	applyActionToState(Action).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

episode :-
	goalreached,
	%
	end_by_goal_counter(X),
	retractall(end_by_goal_counter(_)),
	Y is X + 1,
	assert(end_by_goal_counter(Y)),
	!.
	
episode :-
	domain_specified_end,
	%
	end_by_domain_counter(X),
	retractall(end_by_domain_counter(_)),
	Y is X + 1,
	assert(end_by_domain_counter(Y)),
	!.
	
episode :-
	println('Next step in episode'),
	println('=== State...'),
	printState,
	
	% collect all elements of current state into list
	% only fluents, not statics or derived predicates
	findall(N, (currentState(N), N=fluent(_)), StateList0),
	sort(StateList0, StateList),
	
	println('Getting valid actions'),
	findall(X, validAction(X), ActionList), % Actions that are valid in the current state
	printd('Actions: '),
	println(ActionList),
	
	println('Selecting an action...'),
	explore_parameter(Exp),
	random(F),
	
	(
		(F < Exp)
	->
		% Explore
		(println('Getting random valid action'),
		random_member(Action, ActionList)
		)
	;
		% Policy
		(println('Getting best valid action based on Q estimates'),
		pickBestByBinaryTreeEstimate(ActionList, Action)) % pre-change, obviously, so it can look at 'current state' - doesn't need StateList
	),
	
	printd('Applying action:  '),
	println(Action),
	
	% apply action to generate next state
	applyActionAndReportOracleOutcome(Action, RewardValue),
	% We are now in new state. Find max stored q-value from possible actions from this new state
	findall(Y, validAction(Y), ActionList2),
	getHighestQValueForAnyActionFromCurrentState(ActionList2, FutureValue),
	
	% Note you are passing in 'StateList', i.e., a record of the state that the system was in 
	% when entering this episode, prior to applying the action.
	% The relevant example which we trickle down the tree cares about that state, the associated action,
	% and the outcome in terms of the highest-value action possible from the current, new, state.
	println('Updating Q-value'),
	trickleNewQValueExampleDownTree(StateList, Action, RewardValue, FutureValue),
	
    !,
	episode.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% TREE OPERATIONS %%%

trickleNewQValueExampleDownTree(StateList, Action, RewardValue, FutureValue) :-
	% Modify stored q-value for StateList:
	% Q(state, action) = current Q(state,action) +   gamma * (reward(state,action) + Max[Q(next state, all actions)] - current Q(state,action))
	root(ID),
	trickleDownTree(ID, StateList, Action, RewardValue, FutureValue).
	
splitNode(ID, Test) :-
	(leaf(ID) -> true ; (writef('split failed\n'), trace, fail)),
	assert(test(ID, Test)),
	retract(leaf(ID)),
	retract(predicted_q_value(ID, _Val)),
	createNode(ChildYes),
	createNode(ChildNo),
	assert(child_y(ID, ChildYes)),
	assert(child_n(ID,  ChildNo)),
	assert(parent(ChildYes,ID)),
	assert(parent(ChildNo, ID)),
	initialise_children_stats_and_q(ID, Test),
	% retract all tests at node ID as it's no longer a leaf.
	retractall(leaf_test_stats(ID, _, _, _, _, _)).

% We are passing an example down.
% ExampleStateList is a Prolog list of all fluents in the example's state (no longer current)
% Action is the example's action
% RewardValue is what the oracle says you get for the reward
% FutureValue is the best stored result from applying this action
trickleDownTree(Node, ExampleStateList, Action, RewardValue, FutureValue) :-
	not(leaf(Node)),
	!,
	test(Node, Test),
	(
	(member(Test, ExampleStateList) ; Test = action(Action)) % Test at node is positive?   % Note 'Action' is not wrapped in action()
	->
	(child_y(Node, NewNode))
	;
	(child_n(Node, NewNode))
	),
	trickleDownTree(NewNode, ExampleStateList, Action, RewardValue, FutureValue).
trickleDownTree(Node, ExampleStateList, Action, RewardValue, FutureValue) :-
	leaf(Node),
	!,
	updatePredictedQValue(Node, RewardValue, FutureValue),

	% TODO
	% Revisit
	% This interacts strongly with the "diminishing learning rate parameter" approach.
	% According to the Driessens paper, Q-value for an example is
	%   reward + (gamma)(max future Q)
	current_learning_rate_parameter(Gamma),
	ExampleQValue is RewardValue + (Gamma * (FutureValue)),
	
	updateAllLeafTestStats(Node, ExampleStateList, Action,  ExampleQValue).
	

updatePredictedQValue(LeafID, RewardValue, FutureValue) :-
	predicted_q_value(LeafID, CurrentValue),
	retractall(predicted_q_value(LeafID, _)),
	current_learning_rate_parameter(Gamma),
	Val is CurrentValue + (Gamma * (RewardValue + FutureValue - CurrentValue)),
	% Using a more advanced system for determining this
	printd('New Q-value: '),
	println(Val),
	asserta(predicted_q_value(LeafID, Val)).


% ExampleStateList is a Prolog list of all fluents in the example's state (no longer current ones)
% TestAction is the example's action
% ExampleQValue is calculated from RewardValue and FutureValue by whatever means
updateAllLeafTestStats(LeafID, ExampleStateList, TestAction,  ExampleQValue) :-
	% 1. See whether TestInThisStats is either in the ExampleStateList or == TestAction
	% 2. If yes, increment yes stats (after retracting original)
	% 3. Else, increment no stats (after retracting original)
	forall( leaf_test_stats(LeafID, TestInThisStats, yes, _Count, _SumOfQ, _SumOfSquaredQ), % 'Yes' arbitrarily
			((member(TestInThisStats,ExampleStateList) ; TestInThisStats = action(TestAction)) % Note 'TestAction' is not wrapped in action()
			->
			increment_leaf_stats(LeafID, TestInThisStats, yes, ExampleQValue)
			;
			increment_leaf_stats(LeafID, TestInThisStats, no, ExampleQValue)
			)
		).
	
increment_leaf_stats(LeafID, Test, YesOrNo, QV) :-
	leaf_test_stats(LeafID, Test, YesOrNo, Count, SumOfQ, SumOfSquaredQ),
	retractall(leaf_test_stats(LeafID, Test, YesOrNo, _, _, _)),
	Count2 is Count + 1,
	SumOfQ2 is SumOfQ + QV,
	SumOfSquaredQ2 is SumOfSquaredQ + (QV * QV),
	assert(leaf_test_stats(LeafID, Test, YesOrNo, Count2, SumOfQ2, SumOfSquaredQ2)).

initialise_children_stats_and_q(NodeID, NodeTest) :-
	child_y(NodeID, ChildYes),
	child_n(NodeID,  ChildNo),

	reward_neg(Neg),
	
	% Originally set predicted Q-value at a new leaf as the mean of the relevant examples.
	% We passed in the test that the node NodeID is splitting on as NodeTest.
	leaf_test_stats(ID, NodeTest, yes, CountYes, SumYes, _),
	((CountYes == 0)
	->
	MeanYes = Neg
	;
	MeanYes is SumYes / CountYes),
	
	leaf_test_stats(ID, NodeTest, no, CountNo, SumNo, _),
	((CountNo == 0)
	->
	MeanNo = Neg
	;
	MeanNo is SumNo / CountNo),
	
	assert(predicted_q_value(ChildYes, MeanYes)),
	assert(predicted_q_value(ChildNo,  MeanNo)),
	
	% Find all possible tests - all literals and *actions* not assigned as test in ancestors - add leaf_test_stats for each of them
	
	% 1. Find set of literals comprising all possible tests: grounded fluents and grounded actions;
	% 2. 'Parent' will have been set by the time this function is called, so rule out ancestor tests;
	% 3. For everything in the set, add the zeroed pair of leaf_test_stats literals.
		% leaf_test_stats(leaf_id, test, yes, count, sumOfQ, sumOfSquaredQ).
		% leaf_test_stats(leaf_id, test, no, count, sumOfQ, sumOfSquaredQ).
	makeTheTests(NodeID, NewTestList),
	setNewTests(NewTestList, ChildYes),
	setNewTests(NewTestList, ChildNo).

makeTheTests(NodeID, NewTestList) :-
	getAllPossibleTests(TestList),
	% 'test' can be action(Action) or fluent(Contents)
	% Note the following checks NodeID itself, too.
	getAncestorTests(NodeID, AncestorTests),
	subtract(TestList, AncestorTests, NewTestList).
	
setNewTests([], _ID).
setNewTests([A|B], ID) :-
	assert( leaf_test_stats(ID, A, yes, 0, 0, 0) ),
	assert( leaf_test_stats(ID, A, no,  0, 0, 0) ),
	setNewTests(B, ID).

	
%%%%%

calculateVarianceAtLeafNode(ID, Variance) :-
	% Any test will be good enough for this purpose
	leaf_test_stats(ID, SomeTest, yes, Count1, SumQ1, SumSquareQ1),
	leaf_test_stats(ID, SomeTest, no,  Count2, SumQ2, SumSquareQ2),
	!,
	TotalCount is Count1 + Count2,
	TotalSum is SumQ1 + SumQ2,
	TotalSquaredSum is SumSquareQ1 + SumSquareQ2,
	findVariance(TotalCount, TotalSum, TotalSquaredSum, Variance).

findVariance(Count, Sum, SumOfSquared, Variance) :-
	((Count == 0);(Sum == 0)) -> Variance = 1000000
	;
	(
	MeanQ is Sum/Count,
	SquaredMeanQ is MeanQ * MeanQ,
	MeanSquareSum is SumOfSquared/Count,
	Variance is abs(MeanSquareSum - SquaredMeanQ) % TODO note that taking the absolute value ISN'T prescribed by the (googleable) literature,
													% but otherwise this can end up negative!
	),
	((Variance < 0) -> (trace, writef('Variance failed?\n')) ; true).	
	
calculateYesNoVariancesForTest(ID, Test, VarianceY, VarianceN) :-
	leaf_test_stats(ID, Test, yes, Count1, SumQ1, SumSquareQ1),
	leaf_test_stats(ID, Test, no,  Count2, SumQ2, SumSquareQ2),
	!,
	findVariance(Count1, SumQ1, SumSquareQ1, VarianceY),
	findVariance(Count2, SumQ2, SumSquareQ2, VarianceN).

%%%%%

makeRoot :-
	retractall(root(_)),
	createNode(ID),
	assert(root(ID)),
	assert(predicted_q_value(ID, 0)),
	init_root_stats.
	
createNode(ID) :-
	makeNewId(ID),
	assert(leaf(ID)).
	
makeNewId(ID) :-
	currentId(ID),
	ID2 is ID + 1,
	retractall(currentId(_)),
	asserta(currentId(ID2)).

%	Find all leaves whose ancestor chain
%		(a) is a (partial) match for the current state, and
%		(b) has an action amongst their tests, and
%		(c)	that action is applicable in this state (i.e. is in ActionList).
%	Collect pairs <action, q-value at leaf>
%	Choose the best from the list and return that
pickBestByBinaryTreeEstimate(ActionList, BestAction) :-
	get_best_by_binary_tree_estimate(ActionList, BestAction, _).
	
get_best_by_binary_tree_estimate(ActionList, BestAction, BestValue) :-
	findall(	[Action, Value],
				matchingAncestorChain(ActionList, Action, Value),
				ListOfPairs ),
	( (ListOfPairs == []) -> (random_member(BestAction, ActionList), reward_neg(BestValue))
		;
	(
		member([_,BestValue], ListOfPairs),
		not( (member([_,Y], ListOfPairs), Y > BestValue) ),
		findall(Act2, member([Act2,BestValue], ListOfPairs), NewList),
		random_member(BestAction, NewList)
	)
	).

% (If list is empty, assume default low/negative value)
getHighestQValueForAnyActionFromCurrentState([], N) :- reward_neg(N), !.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	get_best_by_binary_tree_estimate(ActionList, _, BestValue).

%%%%%
	
matchingAncestorChain(ActionList, Action, Value) :-
	leaf(ID),
	getAncestorTests(ID, List),
	member(action(Action), List),
	member(Action, ActionList),
	allAreInCurrentStateOrActions(List),
	predicted_q_value(ID, Value).
	
% Note a node is its own ancestor
getAncestorTests(Node, List) :-
	recursiveGetAncestorTests(Node,[],List).

% root & leaf
recursiveGetAncestorTests(Node, _, []) :-
	root(Node),
	leaf(Node),
	!.
% root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	root(Node), % but not a leaf...
	(test(Node, T) ; (print('recursiveGetAncestorTests 1 failed: no test\n'), trace, fail)),
	test(Node, T),
	!,
	append(WorkingList,[T],ReturnList).
% !root & leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node,Node2),
	leaf(Node),
	!,
	recursiveGetAncestorTests(Node2, WorkingList, ReturnList).
% !root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node,Node2),
	(test(Node, T) ; (print('recursiveGetAncestorTests 2 failed: no test\n'), trace, fail)),
	!,
	append(WorkingList,[T],NewList),
	recursiveGetAncestorTests(Node2, NewList, ReturnList).

allAreInCurrentStateOrActions([]).
allAreInCurrentStateOrActions([A|B]) :-
	A = action(_),
	!,
	allAreInCurrentStateOrActions(B).
allAreInCurrentStateOrActions([A|B]) :-
	currentState(A),
	!,
	allAreInCurrentStateOrActions(B).
	
% Note there's, necessarily, no way to end up with two actions in the same path:
% once you have an action in the path from root to (current) leaf, then every 
% test for any other action will never partition the examples, so the leaf will 
% never split on that test.

init_root_stats :-
	root(RootID),
	makeTheTests(RootID, NewTestList),
	setNewTests(NewTestList, RootID).
	
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

% Consider extending the tree by splitting a leaf node into new leaves.
checkForTreeSplits :-
% Step 1. Calculate variance for each leaf node, and put in a list of ID-value pairs
	findall(	[LeafID, Variance],
				(leaf(LeafID), calculateVarianceAtLeafNode(LeafID, Variance)),
				List1),
% Step 2. Find all ID-Test-Proportion triples where...
	findall(	[NodeID, Test, MeanProportion],
				(	member([NodeID, ParentVariance], List1),
%		(a) parent node variance is greater than zero (preventing divide-by-zero and/or cases where there are probably too few examples)
					ParentVariance > 0,
%		(b) there is at least 2 in both yes and no groups
%		(c) neither child has a higher variance than the parent node
					leaf_test_stats(NodeID, Test, yes, Count1, _, _),
					leaf_test_stats(NodeID, Test, no,  Count2, _, _),
					calculateYesNoVariancesForTest(NodeID, Test, VarianceYes, VarianceNo),
					VarYesProportion is VarianceYes / ParentVariance,
					VarNoProportion  is VarianceNo  / ParentVariance,
					MeanProportion is (VarYesProportion + VarNoProportion)/2,
%		(d) report back the mean of the reductions in variance as 'MeanProportion'
%		(e) the reduction in MEAN variance BETWEEN children compared to the parent is at least 5%, i.e., the proportion is < 95%
					
					%Count1 > 1, Count2 > 1,
					%VarYesProportion =< 1.0,
					%VarNoProportion  =< 1.0,
					%VarYesProportion =< 1.1,
					%VarNoProportion  =< 1.1,
					%MeanProportion  =< 0.95
					%MeanProportion  =< 1.0
					
					% We now relax the tests, arbitrarily, with increasing numbers of example in a node
					% TODO revisit with something much cleaner
					(
						((Count1 >= 5, Count2 >= 5, (VarYesProportion =< 0.8 ; VarNoProportion  =< 0.8), MeanProportion  =< 0.4) -> inc_clause1 ; false)
						;
						((Count1 >= 15, Count2 >= 15, (VarYesProportion =< 0.85 ; VarNoProportion  =< 0.85), MeanProportion  =< 0.55) -> inc_clause2 ; false)
						;
						((Count1 >= 45, Count2 >= 45, (VarYesProportion =< 0.9 ; VarNoProportion  =< 0.9), MeanProportion  =< 0.7) -> inc_clause3 ; false)
						;
						((Count1 >= 120, Count2 >= 120, (VarYesProportion =< 0.95 ; VarNoProportion  =< 0.95), MeanProportion  =< 0.85) -> inc_clause4 ; false)
						;
						((Count1 >= 250, Count2 > 250, (VarYesProportion =< 0.999 ; VarNoProportion  =< 0.999), MeanProportion  =< 0.99) -> inc_clause5 ; false)
					)
					
				),
				List2),
% Step 3. Narrow down to just the list with the best (lowest) numeric value for reduction in variance
	findall(	Triple,
				(	member(Triple, List2),
					Triple = [_, _, Prop1],
					not((	member([_, _, Prop2], List2), Prop2 < Prop1 ))
				),
				NewList),
% Step 4. Select a random 'best test' from the list of valid best ones (could be empty if no node/test combination is currently permissible to split on)
	(
		(NewList == [])
		->
		% Record the failure
		(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount)))
		;
		(random_member(SplitterTriple, NewList),
		SplitterTriple = [ID,Test,_Count],
		(split_count(CurrentCount), NewCount is CurrentCount+1, retractall(split_count(_)), assert(split_count(NewCount))),
		splitNode(ID, Test)
		)
	),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_clause1 :- clause1count(X), Y is X + 1, retractall(clause1count(_)), assert(clause1count(Y)).
inc_clause2 :- clause2count(X), Y is X + 1, retractall(clause2count(_)), assert(clause2count(Y)).
inc_clause3 :- clause3count(X), Y is X + 1, retractall(clause3count(_)), assert(clause3count(Y)).
inc_clause4 :- clause4count(X), Y is X + 1, retractall(clause4count(_)), assert(clause4count(Y)).
inc_clause5 :- clause5count(X), Y is X + 1, retractall(clause5count(_)), assert(clause5count(Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go :-
	assert(loud),
	begin_rl.
	
go(loud) :-
	assert(loud),
	begin_rl.
	
go(quiet) :-
	assert(quiet),
	begin_rl.

begin_rl :-
	episode_count(N),
	assert(initial_num_of_episodes(N)),
	makeRoot,
	doOneEpisode.

doOneEpisode :-
	performOneLearningEpisode.

performOneLearningEpisode :-
	episode_count(X),
	X > 0,
	!,
	retractall(goalreached),
	println('Beginning episode'),
	printCountdown,
	retractall(lastActionWas(_)),
	resetStateAtRandom,
	episode,
	checkForTreeSplits,
	printTableAtEndOfEpisode,
	!,
	Y is X-1,
	retractall(episode_count(X)),
	assert(episode_count(Y)),
	updateLearningValue,
	performOneLearningEpisode.
	
performOneLearningEpisode :-
	!,
	writef('Learning done!\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

println(X) :-
	printd(X), printd('\n').

printd(X) :-
	loud,
	number(X),
	print(X).
	
printd(X) :-
	loud,
	not(number(X)),
	writef(X).

printd(_) :-
	quiet.

printState :-
	currentState(X),
	printd(' - '), println(X),
	fail.
printState.

printTableAtEndOfEpisode :-
	loud,
	!,
	writef('End of episode, learned values:\n'),
	printLearned.
printTableAtEndOfEpisode :-
%trace,
	episode_count(N),
	( (0 is N mod 5000) ; (N < 2) ),
	!,
	writef('End of episode, learned values:\n'),
	printLearned.
printTableAtEndOfEpisode :- !.

% Prints the tree of tests and predicted QValues
printLearned :-
	root(ID),
	printTree(ID, '*', 0),
	writef('Final counts:\n'),
	split_count(S),
	no_split_count(NS),
	writef('Number of times a leaf node split: '), print(S), nl,
	writef('Number of times no node was split: '), print(NS), nl,
	end_by_domain_counter(ED),
	end_by_goal_counter(EG),
	writef('Number of episode-sequences ended by domain: '), print(ED), nl,
	writef('Number of episode-sequences ended by goal success: '), print(EG), nl,
	clause1count(C1), clause2count(C2), clause3count(C3), clause4count(C4), clause5count(C5), 
	writef('Node splits by clauses: #1: '), print(C1), 
	writef('; #2: '), print(C2), 
	writef('; #3: '), print(C3), 
	writef('; #4: '), print(C4), 
	writef('; #5: '), print(C5), nl,
	nl.
	
printTree(ID, Symbol, Count) :-
	leaf(ID),
	!,
	printSpaces(Count),
	writef(Symbol), writef(' #'), print(ID), writef('  '),
	( (test(ID, T), print(T), writef('?')) ; writef('[no test]') ),
	!,
	writef(' : '),
	predicted_q_value(ID, Val),
	print(Val),
	once(
		(leaf_test_stats(ID, ArbitraryTest, yes, LCountY, _, _),
		 leaf_test_stats(ID, ArbitraryTest, no, LCountN, _, _),
		 LC is LCountY + LCountN,
		 writef(' ('),
		 print(LC),
		 writef(' examples) : Variance ')
		 )),
	calculateVarianceAtLeafNode(ID, Variance),
	print(Variance),
	nl.

printTree(ID, Symbol, Count) :-
	child_y(ID, IDY),
	child_n(ID, IDN),
	printSpaces(Count),
	writef(Symbol), writef(' #'), print(ID), writef('  '),
	( (test(ID, T), print(T), writef('?')) ; writef('[no test]') ),
	!,
	nl,
	New is Count + 1,
	printTree(IDY, 'Y', New),
	printTree(IDN, 'N', New).
	
printSpaces(0) :- !.
printSpaces(Count) :-
	writef('  '),
	N is Count - 1,
	printSpaces(N).

printCountdown :-
	loud,
	!,
	episode_count(N),
	writef('(Countdown '),
	print(N),
	writef(')'),
	nl.
printCountdown :-
	episode_count(N),
	( (0 is N mod 100) ; (N < 2) ),
	!,
	writef('(Countdown '),
	print(N),
	writef(')'),
	nl,
	% TODO - remove me...
	printLearned, nl.
printCountdown :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To guarantee convergence to the true Q-value, we slowly decrease the learning rate parameter 'gamma' (current_learning_rate_parameter) over time.
% Although in practice, it's often sufficient to simply use a small gamma.
% The method is: Each iteration/episode, current_learning_rate_parameter -= initial_learning_rate_parameter/(totalnumruns*2 +1).
% So eventually it reaches half.
updateLearningValue :-
	initial_num_of_episodes(A),
	%episode_count(B), % Could use this, but don't need to.
	initial_learning_rate_parameter(X),
	current_learning_rate_parameter(Y),
	Val1 is (A * 2) + 1, % Doubled for now so it narrows down to half the original
	Val2 is X / Val1,
	New is Y - Val2,
	retractall(current_learning_rate_parameter(_)),
	assert(current_learning_rate_parameter(New)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*

(notes elided)

Partial list of bugs fixed...

- Divide by zero, e.g. for zero variance cases.
- Upgraded print/writef for modern Prolog.
- Backtracking was unexpectedly occurring and causing bad values via assert/retract.
- Test stats for negative examples were being updated with the data from their corresponding positive examples.
- Lack of separate cases for 'non-root leaf' and 'non-root non-leaf' were leading to false data.
- Note that leaves end up with many examples attached, suggesting they 'should' be split, but in fact they're often all the exact same or similar example.
- Fixed a problem where negative variance was being calculated.
- Fixed a bug where derived predicates were being tested as fluents.
- Caught a problem where calculating the inverse of a mean was producing faulty initial Q-values.
- Investigated various ways of combining factors to work out which, if any, node to split. e.g., variance, change in variance, yes/no child differences, number of examples.
- Switched to a requirement that only one of the new leaves would experience a reduction in variance.
- Having the robot butler domain always start at 10AM allowed the system to quickly learn that serving an engineer would always fail,
	so exploitation would always run endless examples of moving to the engineer's location (if necessary) and serving them. Changed to spread of times.
- Adjusted robot butler domain so that exogenous events (people moving) correctly updates other outcomes of their location - to wit, previous equipment use 
	and appropriate contextual chance of new equipment use.

*/

