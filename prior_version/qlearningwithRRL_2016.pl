:- dynamic currentState/1, goalState/1, goalreached/0, episode_count/1, loud/0, quiet/0, current_learning_rate_parameter/1, initial_num_of_episodes/1, lastActionWas/1, 
currentNodeId/1, root/1, leaf/1, parent/2, child_y/2, child_n/2, test/2, predicted_q_value/3, leaf_stored_example/5, split_count/1, no_split_count/1, qValueLearned/5, data_output_file/1, 
domain_specified_end/0, end_by_goal_counter/1, end_by_domain_counter/1, clause1count/1, clause2count/1, clause3count/1, total_config_count/1, learned_config/1, episode_high_val/3,
examplepathrecord/4, semifinalexample/5, finalexample/4, candidate_axiom/6, final_axiom/6, initial_learning_rate_parameter/1, last_split_at/1,
sumQCollector/1, countCollector/1, random_sampling_count/1, number_of_random_sample_draws_to_make/1, leaf_generalised/1, affectedLeavesThisConfig/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Suggested syntax to run this program from SWI-Prolog:
/*
	protocol('test-results.txt'), go, noprotocol.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Parameterisation and running %%%

% Include exactly one domain.

%:- include(domain_blocksworld_noloss).
:- include(domain_robot_noloss).

% Define a maximum percentage of the space to explore.
% 10 or 20 percent seems reasonable.
percent_of_static_config_space_to_search(10).

% Define a multiplier determining how much support to draw when constructing axioms.
% Experimentation suggests the value does not matter much in outcomes unless very small,
% and the bulk of processing time is not spent here regardless.
% 100 seems sufficiently large, for safety.
sample_multiplier(100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Other, minor, parameters %%%

% Basic Q-learning not currently supported.
rrl(true).

% Learning rate does not currently change over time.
initial_learning_rate_parameter(0.1).
current_learning_rate_parameter(0.1).

% Action selection is currently epsilon-greedy, i.e., a fixed chance to select a random action instead of the best one.
% This chance is the 'explore' chance.
% 10 or 20 percent seems reasonable.
explore_parameter(0.2). % medium

% Positive and negative reward values.
reward_pos(10).
reward_neg(0.0).

% Change this to 'true' to record all the data files.
% Untested - may be currently broken.
record_full_data_traces(false).

% Artificially constrain candidate axioms to a number of negated clauses and the same number of non-negated clauses.
% Setting to two means the most complex axiom will look like
% V :- w, x, not(y), not(z).
pos_or_neg_clause_limit_per_axiom(2).

% Parsimony is a heuristic not currently used for final axiom selection.
parsimony_literal_penalty(0.02).

% 'Closed world' option determines whether
% (a) Each in the 'negative' section of a candidate axiom must also be in the 'negative' section of an example, or 
% (b) Each in the 'negative' section of a candidate axiom is not in the 'positive' section of an example.
closed_world(true).

% If convergence fails for some reason, cap number of iterations the system can try.
% 200 seems sufficiently high as a catch case.
perConfigEpCap(200).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% System internal counters, all initialised to zero.
split_count(0).
no_split_count(0).
end_by_domain_counter(0).
end_by_goal_counter(0).
currentNodeId(0).
clause1count(0).
clause2count(0).
clause3count(0).
random_sampling_count(0).
total_config_count(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% === Important internal data structures ===

% Tree predicates

% root(id).
% child_y(id, id1).
% child_n(id, id2).
% parent(child, parent).
% leaf(id).
% test(id, test). - 'test' can be action(Action) or fluent(Contents) or static(Value)
% predicted_q_value(leaf_id, value, actioncount).
% leaf_stored_example(leaf_id, remainderdescriptionlist, count, sumOfQ, sumOfSquaredQ).

% Configuration predicates

% learned_config(SORTED_LIST_OF_STATICS).
% num_possible_static_configs(n).
% total_config_count(n).
% percent_of_static_config_space_to_search(n).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

applyActionAndReportOracleOutcome(Action, RewardValue) :-
	goalState(GOAL),
	applyActionToStateAndUpdateHistory(Action),
	(goalAchieved(GOAL) -> (assert(goalreached), reward_pos(X), RewardValue = X) ; (reward_neg(Y), RewardValue = Y)).

% Every element of goal is achieved
goalAchieved(List) :-
	achieved(List).
	
achieved([]) :-
	!.
achieved([A|B]) :-
	(currentState(A) ; derived(A) ;
		(A = not(derived(N)), not(derived(N))) ;
		(A = not(fluent(Z)), not(currentState(fluent(Z))))  ;
		(A = lastActionWas(X), lastActionWas(X))  ),
	!,
	achieved(B).

applyActionToStateAndUpdateHistory(Action) :-
	retractall(lastActionWas(_)),
	assert(lastActionWas(Action)),
	applyActionToState(Action),
	!.

applyActionToStateAndUpdateHistory(Action) :-
	writef('Action application has failed. Problem: '),
	writef(Action),
	nl,
	trace.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go :-
	go(quiet).
	
go(loud) :-
	assert(loud),
	begin_rl.
	
go(quiet) :-
	assert(quiet),
	begin_rl.

begin_rl :-
	makeNewRoot,
	learnOneStaticConfiguration.
	
learnOneStaticConfiguration :-
	num_possible_static_configs(X),
	total_config_count(C),
	percent_of_static_config_space_to_search(P),
	C >= (X * P)/100,
	!,
	writef('Necessary number of static configurations learned. Beginning generalisation step.\n'),
	doGeneralisation.

learnOneStaticConfiguration :-
	% Each MDP gets its own stream with a similar name.
	Str = 'data-', get_time(TimeStamp), atom_concat(Str, TimeStamp, Stream), 
	retractall(data_output_file(_)),
	assert(data_output_file(Stream)), 
	% Use domain-defined function to randomize initial static configuration
	setRandomInitialStaticConfig,
	makePrincipledChangeToStaticConfigurationIfNecessary,
	doAllEpisodes,
	total_config_count(A), B is A +1,
	retractall(total_config_count(_)),
	assert(total_config_count(B)),
	!,
	learnOneStaticConfiguration.
	
doAllEpisodes :-
	retractall(episode_count(_)),
	assert(episode_count(0)),
	retractall(episode_high_val(_,_,_)),
	retractall(affectedLeavesThisConfig(_List)),
	assert(affectedLeavesThisConfig([])),
	performEpisodesForOneConfig.
	
% Q-values converged for this config
performEpisodesForOneConfig :-
	episode_count(N),
	N > 20,
	q_values_have_converged,
	!,
	writef('Q-values converged, episode #'),
	print(N), nl,
	checkForStaticConfigurationChangeAndContinue.
	
% Q-values have failed to converge for this config
performEpisodesForOneConfig :-
	episode_count(N),
	perConfigEpCap(X),
	N > X,
	!,
	writef('Q-values did not converge: over episode limit of '), print(N), nl,
	checkForStaticConfigurationChangeAndContinue.

% Q-values have not yet converged for this config: continue.
performEpisodesForOneConfig :-
	episode_count(X),
	!,
	retractall(goalreached),
	retractall(lastActionWas(_)),
	resetStateAtRandom,
	do_steps_in_episode, % System's effort is here
	checkForTreeSplits,
	printTableAtEndOfEpisode,
	!,
	Y is X+1,
	retractall(episode_count(X)),
	assert(episode_count(Y)),
	% Here is where learning value would be updated in a traditional system
	% updateLearningValue,
	storeRLStatisticsEachEpisode,
	performEpisodesForOneConfig.
	
% Either keeps going with a new static configuration, or stops.
checkForStaticConfigurationChangeAndContinue :-
	markCurrentStaticConfigurationLearned,
	checkStaticConfigChange.

checkStaticConfigChange :-
	total_config_count(C),
	percent_of_static_config_space_to_search(X),
	C > X,
	!.
checkStaticConfigChange :-
	makePrincipledChangeToStaticConfiguration,
	!,
	total_config_count(A), B is A +1,
	retractall(total_config_count(_)),
	assert(total_config_count(B)),
	doAllEpisodes.

% Don't alter a randomized start static configuration, sight unseen.
% Check that it's a duplicate.
makePrincipledChangeToStaticConfigurationIfNecessary :-
	checkForFurtherChangeToConfig.
	
makePrincipledChangeToStaticConfiguration :-
	randomWalkCurrentStaticConfig,
	!,
	checkForFurtherChangeToConfig.

checkForFurtherChangeToConfig :-
	getCurrentStaticConfig(X),
	learned_config(X),
	!,
	makePrincipledChangeToStaticConfiguration.
checkForFurtherChangeToConfig :-
	getCurrentStaticConfig(X),
	not(learned_config(X)),
	!.
	
getCurrentStaticConfig(X) :-
	findall(	N,
				(currentState(N), N=static(_)),
				List1),
	sort(List1, X).

% Catch case: Somehow, we have hit the max number of configs and are still going
randomWalkCurrentStaticConfig :-
	num_possible_static_configs(Limit),
	total_config_count(N),
	N >= Limit, % safety measure
	trace,
	!.

randomWalkCurrentStaticConfig :-
	getCurrentStaticConfig(List),
	domainChangeStatics(List).
	
markCurrentStaticConfigurationLearned :-
	getCurrentStaticConfig(List),
	assert(learned_config(List)).
	
% Assume "convergence" means the best learned value doesn't change by more than 1% for 10 episodes in a row.
% Track and reset as necessary (e.g. when a node in the tree is split).
q_values_have_converged :-
	episode_count(EC),
	last_split_at(Episode),
	Diff is EC - Episode,
	Diff > 20,
	findall( LeafID,
				(episode_high_val(EC, LeafID, HighVal),
				not(( episode_high_val(EC, _, HigherVal), HigherVal > HighVal ))),
				LeafIDs),
	!,
	check_last_X_differences(EC, LeafIDs, 20).

check_last_X_differences(_, _, 0) :- !.
check_last_X_differences(N, LeafIDs, M) :-
	C1 is N-M-1,
	C2 is N-M,
	checkDiffsForAllLeaves(C1, C2, LeafIDs),	
	O is M-1,
	check_last_X_differences(N, LeafIDs, O).

checkDiffsForAllLeaves(_,_,[]) :- !.
checkDiffsForAllLeaves(C1, C2, [ID|Tail]) :-
	% These will just fail safely if there aren't records for this particular ID.
	% (The rule checking for Q-value convergence fails safely)
	episode_high_val(C1, ID, OlderVal),
	episode_high_val(C2, ID, NewerVal),
	% Arbitrarily discard when the highest Q-value is zero, to prevent divide-by-zero
	( ((OlderVal == 0.0) ; (OlderVal == 0)) -> fail ; true),
	( ((NewerVal == 0.0) ; (NewerVal == 0)) -> fail ; true),
	Diff1 is (NewerVal/OlderVal),
	Diff2 is (OlderVal/NewerVal),
	Diff1 < 1.01, % Less than 1% change required, for 20 generations
	Diff2 < 1.01, % Less than 1% change required, for 20 generations
	!,
	checkDiffsForAllLeaves(C1, C2, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The second stage is generalisation. This proceeds by generating and revising proto-candidate axioms and candidate axioms in various ways,
% and currently, the proto-candidates are renamed many times (i.e., stored in different literals). This is confusing and bears revisiting.

doGeneralisation :-
	
	findall([A1], leaf(A1), List0), length(List0,Size0),
	writef('===> Number of leaves in the BDT: '), print(Size0), nl,
	
	writef('Extracting examples...'), nl,
	makeGeneralisedExamples,
	findall([ID,Statics, S, C], examplepathrecord(ID, Statics, S, C), List1), length(List1,Size1),
	writef('===> Number of \'examplepathrecord\' records including duplicates; nonactions removed: '), print(Size1), nl,
		
	writef('Constructing full set of generalised examples...'), nl,
	constructCandidateAxioms,
	findall([CC,DD,EE,FF,GG], semifinalexample(CC, DD, EE, FF, GG), List2), length(List2,Size2),
	writef('===> Number of \'semifinalexample\' records produced by finding subsets of each example: '), print(Size2), nl,

	writef('Making candidate tree\n'),
	makeCandidateTree,
	
	findall([A2A,B2B,C2C,D2D,E2E,F2F], candidate_axiom(A2A, B2B, C2C, D2D, E2E, F2F), List3), length(List3,Size3),
	writef('===> Number of candidate axioms: '), print(Size3), nl,
	
	writef('\nCandidate axioms, pre parsimony:\n'),
	%listing(candidate_axiom/6),
	
	writef('Extracting top candidate axioms.\n'),
	adjustAxiomsForParsimony,
	writef('Sorting top candidate axioms.\n'),
	sortFinalAxioms,
	%listing(final_axiom/6),
	
	writef('Finalising top candidate axioms, considering generality (universal filter).\n'),
	finalAxiomQualityImprovement,
	extractAndReportTopAxioms(0),
	
	writef('Checking top (max ten) candidates with oracle.\n'),
	checkTopCandidatesWithOracle,
	
	writef('Lifting final candidates\n'),
	liftCandidates,
	
	extractAndReportTopAxioms('final'),
	!.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reduce all leaves into "path records" of the tests from them to root. Keep only statics and actions. Delete leaves in their wake.
% This (amongst many other things) will need to change if we learn knowledge structures based on anything other than static attributes.
makeGeneralisedExamples :-
	leaf(ID),
	not(leaf_generalised(ID)),
	getStateActionInfoOnPath(ID, StateNeg, StatePos),
	makeExamplePathRecords(ID, StateNeg, StatePos),
	assert(leaf_generalised(ID)),
	!,
	makeGeneralisedExamples.
makeGeneralisedExamples :-
	not( (leaf(ID), not(leaf_generalised(ID))) ),
	!.

makeExamplePathRecords(ID, StateNeg, StatePos) :-
	findall(X1, (member(static(X1), StatePos)), StaticsPos1),
	findall(X2, (member(static(X2), StateNeg)), StaticsNeg1),
	sort(StaticsPos1, StaticsPos),
	sort(StaticsNeg1, StaticsNeg),
	Statics = [StaticsPos,StaticsNeg],
	domainGoalAction(DGA),
	(
		% Case 1: a positive action is mentioned in StatePos and it matches the one we care about (possibly this never happens)
		% Case 2: look for the action we care about specified within the actual examples at the leaf
		member(action(DGA), StatePos)
	->
		addMatchingSubsetAsPathRecord(ID, Statics)
	;
		addMatchingSubsetAsPathRecord(ID, Statics, DGA)
	).

% Pick one example at this leaf at random
addMatchingSubsetAsPathRecord(ID, StaticsPosAndNeg) :-
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, _, _, _, _), LeafExample), LeafExamples),
	addANewPathRecord(ID, StaticsPosAndNeg, LeafExamples).

% Pick one example at this leaf at random, as long as it has the goal action DGA in its remainder
addMatchingSubsetAsPathRecord(ID, StaticsPosAndNeg, DGA) :-
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, ExRemainder, _, _, _), LeafExample, member(action(DGA), ExRemainder)), LeafExamples),
	addANewPathRecord(ID, StaticsPosAndNeg, LeafExamples).
	
addANewPathRecord(_, _, []) :- !.
addANewPathRecord(_, [[],[]], _) :- !.
addANewPathRecord(ID, [StaticsYes,StaticsNo], LeafExamples) :-
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(0.0)),
	assert(countCollector(0)),
	amassPathRecord(ID, LeafExamples),
	sumQCollector(SumQ),
	countCollector(Count),
	assert(examplepathrecord(ID, [StaticsYes,StaticsNo], SumQ, Count)),
	!.
addANewPathRecord(ID, [StaticsYes,StaticsNo], LeafExamples) :- print([ID, [StaticsYes,StaticsNo], LeafExamples]), trace.
	
amassPathRecord(ID, LeafExamples) :-
	member(leaf_stored_example(ID, _ExRemainder, InternalCount, InternalSum, _), LeafExamples),
	sumQCollector(SumQ), SumQNew is SumQ + (InternalSum / InternalCount),
	countCollector(Count), CountNew is Count + 1,
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(SumQNew)),
	assert(countCollector(CountNew)),
	fail.
amassPathRecord(_,_) :- !.
	
% Benefit of RRL is being able to cluster 'irrelevant' (non-determining) attributes (such as colours in the blocks world) at the leaf.
% But these also have to be available as counterexamples to low-quality candidate rules that only emerged by chance.
% Thus our approach, where only internal nodes are used to construct the set of possible axioms but then all the information is available to support or eliminate them.

% There is still work to be done in this regard because the system doesn't know what attribute-value pairs are.
% e.g., if a leaf specifies colour(block1,red), this doesn't provide any feedback to the candidate axiom "not(colour(block1,blue))".

subsetp([], []).
subsetp([E|Tail], [E|NTail]):- subsetp(Tail, NTail).
subsetp([_|Tail], NTail):- subsetp(Tail, NTail).
  
constructCandidateAxioms :-
	not(examplepathrecord(_, _, _, _)),
	!.

constructCandidateAxioms :-
	% 1. Pick any path record
	examplepathrecord(ID, [Pos,Neg], Sum, Count),
	% 2. Infer its candidate-subsets
	pos_or_neg_clause_limit_per_axiom(Lim),
	findall(SubPos, (subsetp(Pos, SubPos), length(SubPos, SP), SP =< Lim), ListOfPosSubsets),
	findall(SubNeg, (subsetp(Neg, SubNeg), length(SubNeg, SN), SN =< Lim), ListOfNegSubsets),
	findall(L, (member(A,ListOfPosSubsets), member(B,ListOfNegSubsets), L = [A,B], L \== [[],[]]), List),
	!,
	% 3. For each, create or find the candidate and increment its two values appropriately
	updateCandidateAxioms(List, Sum, Count, ID),
	% 4. Delete the path record
	retractall(examplepathrecord(ID, [Pos,Neg], Sum, Count)),
	% 5. Repeat.
	!,
	constructCandidateAxioms.
	
% Recursive base
updateCandidateAxioms([], _S, _C, _ID) :- !.

updateCandidateAxioms([A|B], Sum, Count, ID) :-
	A = [PosList,NegList],
	semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList),
	!,
	Q2 is QV + Sum,
	Count2 is OldCount + Count,
	TempMean is Sum/Count,
	((TempMean < Worst) -> Worst2 = TempMean ; Worst2 = Worst),
	(member(ID, OldIDList) -> NewIDList = OldIDList ; append(OldIDList, [ID], NewIDList)),
	retractall(semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList)),
	assert(semifinalexample([PosList,NegList], Q2, Worst2, Count2, NewIDList)),
	updateCandidateAxioms(B, Sum, Count, ID).

% Make new candidate	
updateCandidateAxioms([A|B], Sum, Count, ID) :-
	A = [PosList,NegList],
	not(semifinalexample([PosList,NegList], _, _, _, _)),
	!,
	TempMean is Sum/Count,
	assert(semifinalexample([PosList,NegList], Sum, TempMean, Count, [ID])),
	updateCandidateAxioms(B, Sum, Count, ID).

% Shouldn't be possible
updateCandidateAxioms(_, _, _, _) :- trace.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getStateActionInfoOnPath(LeafID, SortedStateNeg, SortedStatePos) :-
	recursiveGetStateActionInfoOnPath(LeafID, [], SortedStateNeg, [], SortedStatePos).
	
% Base case: hit the root
recursiveGetStateActionInfoOnPath(Node, CurrentStateNeg, ReturnSortedStateNeg, CurrentStatePos, ReturnSortedStatePos) :-
	root(Node),
	sort(CurrentStateNeg, ReturnSortedStateNeg),
	sort(CurrentStatePos, ReturnSortedStatePos),
	!.

% Non-root: leaf or intermediate node (note leaf+root is covered above)
% Thus is a positive or negative example of parent test (action/fluent/static)
recursiveGetStateActionInfoOnPath(Node, CurrentStateNeg, ReturnSortedStateNeg, CurrentStatePos, ReturnSortedStatePos) :-
	not(root(Node)),
	parent(Node, Node2),
	(test(Node2, Test) ; (print('recursiveGetStateActionInfoOnPath failed: no test\n'), trace, fail)),
	!,
	(
		child_y(Node2, Node)
		->
		(append(CurrentStatePos,[Test],CurrentStatePos2), CurrentStateNeg2 = CurrentStateNeg)
		;
		(append(CurrentStateNeg,[Test],CurrentStateNeg2), CurrentStatePos2 = CurrentStatePos) % assume child_n
	),
	!,
	recursiveGetStateActionInfoOnPath(Node2, CurrentStateNeg2, ReturnSortedStateNeg, CurrentStatePos2, ReturnSortedStatePos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeCandidateTree :-
	writef('Finalising all semifinalexample candidates\n'),
	reduceToHighQualityCandidates,
	findall([A,B,C,D], finalexample(A, B, C, D), List3), length(List3,Size3),
	writef('===> Number of reduced \'finalexample\' records remaining: '), print(Size3), nl,
	establishSupportForCandidates,
	writef('===> Now supported with random examples.'), nl,
	finaliseCandidates.

reduceToHighQualityCandidates :-
	not(semifinalexample(_,_,_,_,_)),
	!.

reduceToHighQualityCandidates :-
	semifinalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingLeavesList),
	% Arithmetic mean
	TempMean is QValueSum/ExampleCount,
	retractall(semifinalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingLeavesList)),
	reward_pos(N),
	Min is N / 2.0,
	((TempMean > Min) -> assert(finalexample([Yes,No], QValueSum, WorstQValue, ExampleCount)) ; true),
	!,
	reduceToHighQualityCandidates.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Do a sweep through, adding the values of random examples to the appropriate candidates

establishSupportForCandidates :-
	% number_of_random_sample_draws_to_make should be set to something based on
	% (a) the number of attributes not used at nodes in the BDT, and/or average path length (because an attribute can appear in multiple branches); and
	% (b) some multiplier arrived at experimentally.
	getAllPossibleTests(DomainTestsList),
	length(DomainTestsList, DTL),
	% Count number of unique internal nodes UIN
	findall(	Test,
				test(_SomeNode,Test),
				TestsUsed
			),
	sort(TestsUsed, UniqueTestsUsed),
	length(UniqueTestsUsed, UIN),
	N is (DTL - UIN) +1,
	% Get average path length P (note a node is its own ancestor)
	findall(	ALength,
				( leaf(ID), getAncestorTests(ID, Ancestors), length(Ancestors, ALength) ),  % 'getAncestorTests' doesn't distinguish between positive and negative responses, but that is fine for this context
				PathLengths
			),
	sum_list(PathLengths, SL),
	length(PathLengths, LL),
	P is SL / LL,
	% Take mean
	Average is (N + P) / 2,
	sample_multiplier(SM),
	Total is Average * SM,
	assert(number_of_random_sample_draws_to_make(Total)),
	writef('Sample multiplier: '), print(SM), nl,
	writef('Path length: '), print(P), 
	writef('; unused attribute tests: '), print(N), 
	writef('; average: '), print(Average), nl,
	writef('Number of random samples to draw: '), print(Total), nl, 
	% Now perform the support step, now that you know how many samples to draw
	!,
	findall(X, (X = leaf_stored_example(_, _, _, _, _), X), AllExamplesList),
	length(AllExamplesList, AELSize),
	writef('Total set of samples we can draw from: '), print(AELSize), nl,
	supportWithRandomExamples(AllExamplesList).

% Base case: Done
supportWithRandomExamples(_) :-
	number_of_random_sample_draws_to_make(X),
	random_sampling_count(Y),
	Y >= X, % Can't use equality. Even before the multiplier, it's not going to be an integer; it's produced by an average.
	writef('\nFinished support step: Drew '),
	print(Y),
	writef(' random examples.\n'),
	!.
% Base case: Ran out of possible samples early
supportWithRandomExamples([]) :-
	writef('\n*****\nFinished support step: Ran out of possible samples early!\n*****\n'),
	!.
% Recursive case: Support with a new sample
supportWithRandomExamples(ExampleList) :-
	drawNewRandomSampleFromBDT(ExampleList, NewExampleList),
	addToSampleCount(1),
	!,
	supportWithRandomExamples(NewExampleList).

addToSampleCount(Integer) :-
	random_sampling_count(RSC),
	RSC2 is RSC + Integer,
	retractall(random_sampling_count(RSC)),
	assert(random_sampling_count(RSC2)).

% 1. Get a random example from a random leaf.
% 2. Find the full example by adding (a) its locally stored statics (that match domain-defined testable ones) to (b) its path to root.
% 3. Find each subset of the full example that matches a candidate.
% 4. Add the example's value to that candidate's, and increment the candidate's count.
% 5. Delete the example so it can't be drawn again.
drawNewRandomSampleFromBDT(ExampleList, ElidedExampleList) :-
	random_member(RandomExample, ExampleList),
	select(RandomExample, ExampleList, ElidedExampleList),
	%
	RandomExample = leaf_stored_example(LeafID, RemainderList, Count, SumOfQ, _SumOfSquaredQ),
	!,
	AddedValue is SumOfQ / Count,
	getStateActionInfoOnPath(LeafID, NegativeAncestors, PositiveAncestors),
	%
	append(RemainderList, PositiveAncestors, PositiveExampleAttributes1),
	NegativeExampleAttributes1 = NegativeAncestors,
	domainGoalAction(DGA),
	(not(member(action(DGA), PositiveExampleAttributes1))
	->
	( addToSampleCount(-1) ) % If it doesn't contain the action you care about, recurse after decrementing the count (because it will be incremented elsewhere)
	;
	(
		reduceToStaticsAndRemoveWrappers(PositiveExampleAttributes1, PositiveExampleAttributes),
		reduceToStaticsAndRemoveWrappers(NegativeExampleAttributes1, NegativeExampleAttributes),
		%
		findall(	Candidate,
					someSubsetMatchesSomeCandidate(PositiveExampleAttributes, NegativeExampleAttributes, Candidate),
					CandidatesToAdjust0
				),
		sort(CandidatesToAdjust0, CandidatesToAdjust),
		addToAllCandidates(CandidatesToAdjust, AddedValue)
	)).

% Use because 'finalexample' candidates already have the 'static' wrappers removed
reduceToStaticsAndRemoveWrappers(List1, List2) :-
	findall(X, member(static(X), List1), List2).

someSubsetMatchesSomeCandidate(PositiveExampleAttributes, NegativeExampleAttributes, Candidate) :-
	closed_world(false),
	!,
	% Each in the 'negative' section of the candidate axiom must also be in the 'negative' section of the example
	finalexample([AnyPositiveSubset,AnyNegativeSubset], QValueSum, WorstQValue, ExampleCount),
	subsetp(PositiveExampleAttributes, AnyPositiveSubset),
	subsetp(NegativeExampleAttributes, AnyNegativeSubset),
	Candidate = finalexample([AnyPositiveSubset,AnyNegativeSubset], QValueSum, WorstQValue, ExampleCount).
	
someSubsetMatchesSomeCandidate(PositiveExampleAttributes, _NegExampleAtts, Candidate) :-
	closed_world(true),
	!,
	% It is enough that each in the 'negative' section of the candidate axiom is NOT in the positive section of the example
	% This is important because otherwise we can only find things that have appeared as negative tests
	Candidate = finalexample([AnyPositiveSubset,CandidateNegativeLiterals], QValueSum, WorstQValue, ExampleCount),
	% It exists
	finalexample([AnyPositiveSubset,CandidateNegativeLiterals], QValueSum, WorstQValue, ExampleCount),
	subsetp(PositiveExampleAttributes, AnyPositiveSubset),
	not( (member(N,CandidateNegativeLiterals), member(N,PositiveExampleAttributes) ) ).
	
addToAllCandidates([], _) :- !.
addToAllCandidates([A|B], AddedValue) :-
	A = finalexample(Content, QValueSum, WorstQValue, ExampleCount),
	NewQ is QValueSum + AddedValue,
	((AddedValue < WorstQValue) -> NewWorst = AddedValue ; NewWorst = WorstQValue),
	NewCount is ExampleCount + 1,
	retractall(finalexample(Content, QValueSum, WorstQValue, ExampleCount)),
	assert(finalexample(Content, NewQ, NewWorst, NewCount)),
	!,
	addToAllCandidates(B, AddedValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Finally, remove cases that are still supported by only one or two examples
finaliseCandidates :-
	finalexample(_, _, _, 1),
	retractall(finalexample(_, _, _, 1)),
	!,
	finaliseCandidates.
finaliseCandidates :-
	finalexample(_, _, _, 2),
	retractall(finalexample(_, _, _, 2)),
	!,
	finaliseCandidates.
% Base case
finaliseCandidates :- !, changeExamplesToCandidates.

changeExamplesToCandidates :-
	finalexample([StaticsYes,StaticsNo], Q, Worst, Count),
	retractall(finalexample([StaticsYes,StaticsNo], Q, Worst, Count)),
	domainGoalAction(Act),
	assert(candidate_axiom(raw, not_occurs(Act), [StaticsYes,StaticsNo], Q, Worst, Count)),
	!,
	changeExamplesToCandidates.
changeExamplesToCandidates.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

liftCandidates :-
	final_axiom(Rank, not_occurs(Act), [Yes,No], AdjustedMean, Worst, Count),
	nonvar([Yes,No]),
	!,
	retractall(final_axiom(Rank, not_occurs(Act), [Yes,No], AdjustedMean, Worst, Count)),
	% 1. Get Arity = arity of Act
	functor(Act, _Name, Arity),
	% 2. For each of Arity in turn, replace in Act and throughout Yes and No with members of list
	% [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]
	AlphabetVariableList = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z],
	replaceByArity(Arity, Act, Yes, No, AlphabetVariableList),
	assert(final_axiom(Rank, fails(Act), [Yes,No], AdjustedMean, Worst, Count)),
	!,
	% 3. Recursive call
	liftCandidates.
liftCandidates.

replaceByArity(0, _Act, _StaticsYes, _StaticsNo, _) :- !.
replaceByArity(Arity, Act, StaticsYes, StaticsNo, AlphabetVariableList) :-
	Arity > 0,
	!,
	nth1(Arity, AlphabetVariableList, NewVariable), % Get nth starting at 1
	(arg(Arity, Act, AtomicValue)),
	setarg(Arity, Act, NewVariable),
	replaceAtomWithVar(AtomicValue, NewVariable, StaticsYes),
	replaceAtomWithVar(AtomicValue, NewVariable, StaticsNo),
	NewArity is Arity - 1,
	!,
	replaceByArity(NewArity, Act, StaticsYes, StaticsNo, AlphabetVariableList).

replaceAtomWithVar(_AtomicValue, _NewVariable, []) :- !.
replaceAtomWithVar(AtomicValue, NewVariable, [Head|Tail]) :-
	not(compound(Head)),
	!,
	replaceAtomWithVar(AtomicValue, NewVariable, Tail).
replaceAtomWithVar(AtomicValue, NewVariable, [Head|Tail]) :-
	compound(Head),
	(  (arg(FoundArity, Head, Argument), Argument == AtomicValue)
	->
	setarg(FoundArity, Head, NewVariable)
	;
	true),
	!,
	replaceAtomWithVar(AtomicValue, NewVariable, Tail).
	
% The following has been hamstrung so as to go unused
adjustAxiomsForParsimony :-
	not(candidate_axiom(raw,_,[_,_],_,_,_)),
	!.
adjustAxiomsForParsimony :-
	candidate_axiom(raw,Act,[Yes,No],Q,Worst,Count),
	Mean is Q/Count,
	%length(Yes, L1),
	%length(No, L2),
	%NumLiterals is L1 + L2,
	%parsimony_literal_penalty(P),
	%Penalty is NumLiterals * P,
	Penalty = 0,
	NewMean is Mean - Penalty, % Reduce mean value very slightly based on number of literals - enough to break ties but probably not much else
	NewWorst is Worst - Penalty,
	retractall(candidate_axiom(raw,Act,[Yes,No],Q,Worst,Count)),
	assert(candidate_axiom(NewMean,Act,[Yes,No],Q,NewWorst,Count)),
	adjustAxiomsForParsimony.
	%=> candidate_axiom(AdjustedMean,Act,[Yes,No],Q,Worst,Count),
	
% (a) Only take absolute top tier (in terms of amassed value)
% (b) Rank by total number of supporting examples, high to low
sortFinalAxioms :-
	% get best value, pass it in
	candidate_axiom(AdjustedMean,_,_,_,_,_),
	not( (candidate_axiom(AdjustedMean2,_,_,_,_,_), AdjustedMean2 > AdjustedMean) ),
	!,
	rankFinalAxioms(AdjustedMean).

rankFinalAxioms(RequiredMean) :-
	not(candidate_axiom(RequiredMean,_,[_,_],_,_,_)),
	!.
rankFinalAxioms(RequiredMean) :-
	candidate_axiom(RequiredMean,Act,[Yes,No],Q,Worst,Count),
	not( (candidate_axiom(RequiredMean,_,_,_Q2,_,Count2), Count2 > Count) ),
	!,
	((final_axiom(Number,_,_,_,_,_), not( (final_axiom(Number2,_,_,_,_,_), Number2 > Number) ), NewRank is Number + 1)
	;
	NewRank = 1),
	% Essentially, change the rank each time, starting from best (highest mean), i.e., smallest number rank	
	retract(candidate_axiom(RequiredMean,Act,[Yes,No],Q,Worst,Count)),
	asserta(final_axiom(NewRank,Act,[Yes,No],RequiredMean,Worst,Count)),
	!,
	rankFinalAxioms(RequiredMean).

% % % % % % % % % %

finalAxiomQualityImprovement :-
	checkCandidateAxiom(1),
	reRankAxioms(0).

% 1. Run out of axioms to check.
checkCandidateAxiom(N) :-
	not( final_axiom(N,_Act,_,_Mean,_Worst,_Count) ),
	!.
% 2. Has more than one literal in common with an already-confirmed axiom.
/*    *** Cases like this would be used for heuristic filtering. ***
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_),
	M < N,
	intersection(Yes1, Yes2, Yes),
	intersection(No1, No2, No),
	append(Yes, No, All),
	length(All, Len),
	Len > 1,
	!,
	retractall(final_axiom(N,_,[Yes1,No1],_,_,_)),
	Next is N+1,
	checkCandidateAxiom(Next).
*/
% 3. N is a more specific form of an already-confirmed axiom.
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_),
	M < N,
	subset(Yes2, Yes1), % Yes2 is subset of Yes1
	subset(No2, No1),
	!,
	retractall(final_axiom(N,_,[Yes1,No1],_,_,_)),
	Next is N+1,
	checkCandidateAxiom(Next).
% 4. N is a more general form of an already-confirmed axiom. (unlikely, but perhaps they could have the exact same amount of support, so technically possible)
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_),
	M < N,
	subset(Yes1, Yes2), % Yes1 is subset of Yes2
	subset(No1, No2),
	!,
	retractall(final_axiom(M,_,[Yes2,No2],_,_,_)),
	checkCandidateAxiom(N). % Have to check the same one again, in case of problems
% 5. Base case
checkCandidateAxiom(N) :-
	!,
	Next is N+1,
	checkCandidateAxiom(Next).
	
reRankAxioms(CurrentLargestRank) :-
	% 1. Find the candidate with the rank > CurrentLargestRank but < all other extant ranks
	final_axiom(N,Act,B,Mean,Worst,C),
	N > CurrentLargestRank,
	not(( 
		final_axiom(M,_,_,_,_,_),
		M > CurrentLargestRank,
		M < N
	)),
	% 2. Retract it and set it to rank CurrentLargestRank+1
	retract(final_axiom(N,Act,B,Mean,Worst,C)),
	NewRank is CurrentLargestRank+1,
	% Discard any over the top ten!
	% This should not be hardcoded, and itself is a form of heuristic
	((NewRank < 11) -> assert(final_axiom(NewRank,Act,B,Mean,Worst,C)) ; true),
	% 3. Recurse
	!,
	reRankAxioms(NewRank).
% Base case
reRankAxioms(_) :- !.

checkTopCandidatesWithOracle :-
	% For each remaining candidate C in turn:
	% 1. Start with domain-defined physical state
	% 2. Test random static configurations until you get a static config S where the action succeeds
	% 3. Now:
	%		(a) Modulate S to S' by the minimum necessary changes such that the partial configuration described by C holds in S'
	% 		(b) Poll the oracle as to whether the action still succeeds in S'
	% 		(c) If it does, discard the candidate axiom C
	
	% For purposes of experiments, have hardcoded five passes, and printing results between each.
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(1),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(2),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(3),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(4),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(5).
	
oracleFilterCandidates(N) :-
	not(final_axiom(N,_Act,_B,_Mean,_Worst,_C)),
	!.
oracleFilterCandidates(N) :-
	writef('Oracle filter:\n  Checking candidate #'), print(N), nl,
	setVirtuousStateRandomly,
	!,
			%storeCurrentStateAndConfig(STLIST1), writef('>>> '), print(STLIST1), nl,
	final_axiom(N,Act,[True,False],Mean,Worst,C),
	adjustVirtuousState(True,False),
			%storeCurrentStateAndConfig(STLIST2), writef('>>> '), print(STLIST2), nl,
	%trace,
	(not(actionFailsInDomain)
		->
		(writef('  Pruning candidate #'), print(N), nl,
		retractall(final_axiom(N,Act,[True,False],Mean,Worst,C)))
		;
		true
	),
	M is N +1,
	!,
	oracleFilterCandidates(M).
	
setVirtuousStateRandomly :-
	resetStateAtRandom,
	setRandomInitialStaticConfig,
	storeCurrentStateAndConfig(List),
	!,
	(actionFailsInDomain -> setVirtuousStateRandomly ; restoreStateAndConfig(List)).
	% Some things here work around destructive operations (i.e., permanent changes)
	
actionFailsInDomain :-
	domainGoalAction(Action),
	not(validAction(Action)), % Physical configuration is wrong for it to work
	!.
actionFailsInDomain :-
	domainGoalAction(Action),
	applyActionToStateAndUpdateHistory(Action), 
	goalState(GOAL), % Remember this defines an unexpected FAILURE.
	goalAchieved(GOAL), % That's why this is positive.
	!.

storeCurrentStateAndConfig(List) :-
	findall(N, currentState(N), List).

restoreStateAndConfig(List) :-
	retractall(currentState(_)),
	restoreFromList(List).
	
restoreFromList([]) :- !.
restoreFromList([A|B]) :- 
	assert(currentState(A)),
	restoreFromList(B).
	
adjustVirtuousState(True,False) :-
	% nothing from False is true in the state, everything from True is true in the state
	not( (member(F,False), currentState(static(F))) ),
	not( (member(T,True), not(currentState(static(T)))) ),
	!.
adjustVirtuousState(True,False) :-
	% 1. Remove anything in False from the state.
	swapAllOutFromState(False), % Replaces with some attribute
	% 2. Add anything in True to the state, removing anything the domain defines as contradicting it.
	swapAllInToState(True), % Replaces existing attribute
	!,
	adjustVirtuousState(True,False).
	
swapAllOutFromState([]) :- !.
swapAllOutFromState([A|B]) :-
	swapOut(A),
	swapAllOutFromState(B).

swapAllInToState([]) :- !.
swapAllInToState([A|B]) :-
	swapIn(A),
	swapAllInToState(B).	

swapOut(StaticLiteral) :-
	not( currentState(static(StaticLiteral)) ),
	!.
swapOut(StaticLiteral) :-
	currentState(static(StaticLiteral)),
	retractall(currentState(static(StaticLiteral))),
	domain_attribute_alternatives(StaticLiteral, AltList),
	random_member(Alt, AltList),
	assert(currentState(static(Alt))).
swapIn(StaticLiteral) :-
	currentState(static(StaticLiteral)),
	!.
swapIn(StaticLiteral) :-
	domain_attribute_alternatives(StaticLiteral, AltList),
	deleteAllStatics(AltList),
	assert(currentState(static(StaticLiteral))).
	
deleteAllStatics([]) :- !.
deleteAllStatics([A|B]) :-
	retractall(currentState(static(A))),
	deleteAllStatics(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractAndReportTopAxioms(NToReport) :-
	writef('\n------------------------\n'),
	writef('Number of oracle passes: '), print(NToReport), nl,
	printAxiomListRecursive(1),
	writef('\n------------------------\n').

printAxiomListRecursive(Rank) :-
	Rank > 99,
	!.
printAxiomListRecursive(Rank) :-
	not(final_axiom(Rank, _Act, _YN, _QMean, _Worst, _Count)),
	!.
printAxiomListRecursive(Rank) :-
	final_axiom(Rank, Act, [Yes,No], QMean, Worst, Count),
	print(Rank), writef('. '),
	((Rank < 10) -> writef(' ') ; true), % Zero pad
	printAxiomNicelyWithoutNewline(Act, Yes, No),
	writef('  [mean '), print(QMean), writef('], [worst '), print(Worst), writef('], [support: '), print(Count), writef(' examples]'), nl,
	NewRank is Rank + 1,
	printAxiomListRecursive(NewRank).

printAxiomNicelyWithoutNewline(Act, Yes, No) :-
	print(Act),
	writef(' :- '),
	writeAllInList(Yes),
	writeAllInListNegated(No),
	writef('end.').

writeAllInList([]) :- !.
writeAllInList([A|B]) :-
	!,
	print(A), writef(', '), writeAllInList(B).

writeAllInListNegated([]) :- !.
writeAllInListNegated([A|B]) :-
	!,
	writef('not('), print(A), writef('), '), writeAllInListNegated(B).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_steps_in_episode :-
	one_step.

one_step :-
	goalreached,
	%
	end_by_goal_counter(X),
	retractall(end_by_goal_counter(_)),
	Y is X + 1,
	assert(end_by_goal_counter(Y)),
	!.
	
one_step :-
	domain_specified_end,
	%
	end_by_domain_counter(X),
	retractall(end_by_domain_counter(_)),
	Y is X + 1,
	assert(end_by_domain_counter(Y)),
	!.
	
one_step :-
	rrl(true),
	!,
	% Collect all elements of current state into list: only fluents, not statics or derived predicates
	findall(N, (currentState(N)), StateListUnsorted),
	sort(StateListUnsorted, StateList),
	println('Getting valid actions'),
	findall(X, validAction(X), ActionList), % Actions that are valid in the current state
	%
	println('Selecting an action...'),
	explore_parameter(Exp),
	random(F),
	%
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
	%
	printd('Applying action:  '), println(Action),
	% Apply action to generate next state
	applyActionAndReportOracleOutcome(Action, RewardValue),
	% We are now in new state. Find max stored q-value from possible actions from this new state
	findall(Y, validAction(Y), ActionList2),
	%(ActionList2 == [] -> trace ; true),
	getHighestQValueForAnyActionFromCurrentState(ActionList2, FutureValue),
	% Note you are passing in 'StateList', i.e., a record of the state that the system was in 
	% when entering this step, prior to applying the action.
	println('Updating Q-value'),
	append(StateList, [action(Action)], StateDescriptionWithAction),
	trickleNewQValueExampleDownTree(StateDescriptionWithAction, RewardValue, FutureValue),
	!,
	one_step.

% --- DEPRECATED, UNSUPPORTED ---
% Variant: Q-learning only
one_step :-
	rrl(false),
	!,
	% Collect all elements of current state into list: only fluents, not statics or derived predicates
	findall(N, (currentState(N)), StateListUnsorted),
	sort(StateListUnsorted, StateList),
	findall(X, validAction(X), ActionList), % Actions that are valid in the current state
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
		pickBestByTable(StateList, ActionList, Action))
	),
	% Apply action to generate next state
	applyActionAndReportOracleOutcome(Action, RewardValue),
	% We are now in new state. Find max stored q-value from possible actions from this new state
	findall(Y, validAction(Y), ActionList2),
	getHighestQValueForAnyActionFromCurrentState(ActionList2, FutureValue),
	% Note you are passing in 'StateList', i.e., a record of the state that the system was in 
	% when entering this step, prior to applying the action.
	% The relevant example which we trickle down the tree cares about that state, the associated action,
	% and the outcome in terms of the highest-value action possible from the current, new, state.
	println('Updating Q-value'),
	updateStoredExampleWithQV(StateList, Action, RewardValue, FutureValue),
    !,
	one_step.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pickBestByTable(StateList, ActionList, Action) :-
	findall([Act, Val], (qValueLearned(StateList, Act, Val, _Count), member(Act, ActionList)), ListOfPairs),
	( (ListOfPairs == []) -> random_member(Action, ActionList) ;
	(
		member([_,X], ListOfPairs),
		not( (member([_,Y], ListOfPairs), Y > X) ),
		findall(Act2, member([Act2,X], ListOfPairs), NewList),
		random_member(Action, NewList)
	)
	).

% Works whether or not a current example exists.
updateStoredExampleWithQV(StateList, Action, RewardValue, FutureValue) :-
	(qValueLearned(StateList, Action, CurrentQV, Count)
	->
	true
	;
	(CurrentQV = 0, Count = 0)
	),
	!,
	retractall(qValueLearned(StateList, Action, _, _)), % Remove existing score if applicable
	current_learning_rate_parameter(Gamma),	
	Count2 is Count + 1,
	Alpha is (1/Count2),
	NewQValue is CurrentQV + (Alpha * (RewardValue + (Gamma * FutureValue) - CurrentQV)),
	assert(qValueLearned(StateList, Action, NewQValue, Count2)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% TREE OPERATIONS %%%

splitNode(ID, Test) :-
	(leaf(ID) -> true ; (writef('split failed\n'), trace, fail)),
	(split_count(CurrentCount), NewCount is CurrentCount+1, retractall(split_count(_)), assert(split_count(NewCount))),
	episode_count(EpCount),
	retractall(last_split_at(_)),
	assert(last_split_at(EpCount)),
	assert(test(ID, Test)),
	retract(leaf(ID)),
	retract(predicted_q_value(ID, _Val, _Count)),
	createNode(ChildYes),
	createNode(ChildNo),
	assert(child_y(ID, ChildYes)),
	assert(child_n(ID,  ChildNo)),
	assert(parent(ChildYes, ID)),
	assert(parent(ChildNo,  ID)),
	initialise_children_stats_and_q(ID, Test),
	!.
	
trickleNewQValueExampleDownTree(StateDescriptionWithAction, RewardValue, FutureValue) :-
	% Modify stored Q-value for StateList:
	% Q(state, action) = current Q(state,action) +   gamma * (reward(state,action) + Max[Q(next state, all actions)] - current Q(state,action))
	root(ID),
	trickleDownTree(ID, StateDescriptionWithAction, RewardValue, FutureValue).
	
% ExampleStateList is a list of all fluents in the example's state (no longer current)
% Action is the example's action
% RewardValue is what the oracle says you get for the reward
% FutureValue is the best stored result from applying this action
trickleDownTree(Node, StateDescriptionWithAction, RewardValue, FutureValue) :-
	not(leaf(Node)),
	!,
	test(Node, Test),
	(
	(member(Test, StateDescriptionWithAction) ; (Test = action(Action), member(Action,StateDescriptionWithAction), writef('?1\n')))
	->
	(child_y(Node, NewNode), select(Test, StateDescriptionWithAction, NewStateDescriptionWithAction)) % Remove positive tests as you trickle down so end up with only remainder
	;
	(child_n(Node, NewNode), NewStateDescriptionWithAction = StateDescriptionWithAction)
	),
	!,
	trickleDownTree(NewNode, NewStateDescriptionWithAction, RewardValue, FutureValue).
trickleDownTree(Node, StateDescriptionWithAction, RewardValue, FutureValue) :-
	leaf(Node),
	!,
	updatePredictedQValue(Node, RewardValue, FutureValue),

	% According to Driessens et al, Q-value for an example is
	%   reward + (gamma)(max future Q)
	current_learning_rate_parameter(Gamma),
	ExampleQValue is RewardValue + (Gamma * (FutureValue)),
	
	% REMOVED - resulted in axioms valued at higher-than-maximum-reward
	%updateExamplesStoredAtLeaf(Node, StateDescriptionWithAction, ExampleQValue).
	updateExamplesStoredAtLeaf(Node, StateDescriptionWithAction, RewardValue),
	
	% Record that you adjusted this leaf; you only want to look at convergence/splitting for changed leaves
	touchLeaf(Node).

touchLeaf(ID) :-
	affectedLeavesThisConfig(List),
	member(ID, List),
	!.
touchLeaf(ID) :-
	affectedLeavesThisConfig(List),
	retractall(affectedLeavesThisConfig(List)),
	append(List, [ID], New),
	assert(affectedLeavesThisConfig(New)),
	!.

updatePredictedQValue(LeafID, RewardValue, FutureValue) :-
	predicted_q_value(LeafID, CurrentQV, Count1),
	retractall(predicted_q_value(LeafID, _, _)),
	current_learning_rate_parameter(Gamma),
	Count2 is Count1 + 1,
	Alpha is (1/Count2),
	NewQValue is CurrentQV + (Alpha * (RewardValue + (Gamma * FutureValue) - CurrentQV)),
	%Val is CurrentValue + (Gamma * (RewardValue + FutureValue - CurrentValue)),
	% Now using a more advanced system for determining this
	asserta(predicted_q_value(LeafID, NewQValue, Count2)).

% ExampleStateList is a Prolog list of all fluents in the example's state (no longer current ones)
% TestAction is the example's action
% ExampleQValue is calculated from RewardValue and FutureValue by whatever means
updateExamplesStoredAtLeaf(LeafID, RemainderListWithAction, QV) :-
% Case 1: example already exists, stored at leaf
	sort(RemainderListWithAction,Remainder),
	leaf_stored_example(LeafID, Remainder, Count, SumOfQ, SumOfSquaredQ),
	!,
	Count2 is Count + 1,
	SumOfQ2 is SumOfQ + QV,
	SumOfSquaredQ2 is SumOfSquaredQ + (QV * QV),
	retractall(leaf_stored_example(LeafID, Remainder, Count, SumOfQ, SumOfSquaredQ)),
	assert(leaf_stored_example(LeafID, Remainder, Count2, SumOfQ2, SumOfSquaredQ2)),
	!.
updateExamplesStoredAtLeaf(LeafID, RemainderListWithAction, QV) :-
% Case 2: create new example stored at leaf
	sort(RemainderListWithAction,Remainder),
	Sq is QV * QV,
	assert(leaf_stored_example(LeafID, Remainder, 1, QV, Sq)),
	!.

initialise_children_stats_and_q(NodeID, NodeTest) :-
	child_y(NodeID, ChildYes),
	child_n(NodeID,  ChildNo),
	% Originally set predicted Q-value at a new leaf as the mean of those of the relevant examples.
	reward_neg(Neg),
	% 1. Look at examples responding 'yes' to new test, i.e., it's in their remainder. Count and take Q-values.
	findall( QV,
			(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ), member(NodeTest, ExampleContent), QV is SumOfQ/Count),
			ListYes),
	length(ListYes, NumYes),
	sum_list(ListYes, SumQVYes),
	(	(NumYes == 0) -> 
		MeanYes = Neg
		;
		MeanYes is SumQVYes/NumYes
		),
	% 2. Look at 'no' examples likewise - just look for negation
	findall( QV,
			(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ), not(member(NodeTest, ExampleContent)), QV is SumOfQ/Count),
			ListNo),
	length(ListNo, NumNo),
	sum_list(ListNo, SumQVNo),
	(	(NumNo == 0) -> 
		MeanNo = Neg
		;
		MeanNo is SumQVNo/NumNo
		),
	% 3. Calculate 'yes' and 'no' children's initial Q-values from those
	assert(predicted_q_value(ChildYes, MeanYes, NumYes)),
	assert(predicted_q_value(ChildNo,  MeanNo,  NumNo)),
	% 4. Move examples down to 'yes' and 'no' children appropriately, deleting them at the former leaf
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest),
	!.
	
moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest) :-
	leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ),
	member(NodeTest, ExampleContent),
	!,
	retractall(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ)),
	select(NodeTest, ExampleContent, Remainder),
	asserta(leaf_stored_example(ChildYes, Remainder, Count, SumOfQ, SumOfSquaredQ)),
	!,
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest).
moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest) :-
	leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ),
	not(member(NodeTest, ExampleContent)),
	!,
	retractall(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ)),
	asserta(leaf_stored_example(ChildNo, ExampleContent, Count, SumOfQ, SumOfSquaredQ)),
	!,
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest).
moveExamplesDown(_NodeID, _ChildYes, _ChildNo, _NodeTest) :- !.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculateVarianceAtLeafNode(ID, Variance) :-
	% Look at all examples at leaf
	findall(Count, leaf_stored_example(ID, _, Count, _, _), CountList),
	findall(SumQ, leaf_stored_example(ID, _, _, SumQ, _), SumQList),
	findall(SumSquareQ, leaf_stored_example(ID, _, _, _, SumSquareQ), SumSquareQList),
	sum_list(CountList, Count),
	sum_list(SumQList, SumQ),
	sum_list(SumSquareQList, SumSquareQ),
	!,
	findVariance(Count, SumQ, SumSquareQ, Variance).

findVariance(Count, Sum, SumOfSquared, Variance) :-
	(((Count == 0);(Sum == 0)) -> Variance = 100000
	;
	(
	MeanQ is Sum/Count,
	SquaredMeanQ is MeanQ * MeanQ,
	MeanSquareSum is SumOfSquared/Count,
	Variance is abs(MeanSquareSum - SquaredMeanQ) % Otherwise this can be negative
	)),
	((Variance < 0) -> (trace, writef('Variance failed?\n')) ; true).
	
calculateYesNoVariancesForTest(LeafID, ProspectiveTest, VarianceY, VarianceN) :-
	findall(	[CountYes, QYes, QSqYes],
				(leaf_stored_example(LeafID, ContentYes, CountYes, QYes, QSqYes), member(ProspectiveTest,ContentYes)),
				ListYes ),
	findall(	[CountNo, QNo, QSqNo],
				(leaf_stored_example(LeafID, ContentNo, CountNo, QNo, QSqNo), not(member(ProspectiveTest,ContentNo))),
				ListNo ),
	findall(A, member([A, _, _], ListYes), Count1), sum_list(Count1, CountY),
	findall(B, member([_, B, _], ListYes), SumQ1), 	sum_list(SumQ1, SumQY),
	findall(C, member([_, _, C], ListYes), SumSqQ1), sum_list(SumSqQ1, SumSquareQY),
	findall(D, member([D, _, _], ListNo), Count2), 	sum_list(Count2, CountN),
	findall(E, member([_, E, _], ListNo), SumQ2), 	sum_list(SumQ2, SumQN),
	findall(F, member([_, _, F], ListNo), SumSqQ2), sum_list(SumSqQ2, SumSquareQN),
	!,
	findVariance(CountY, SumQY, SumSquareQY, VarianceY),
	findVariance(CountN, SumQN, SumSquareQN, VarianceN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeNewRoot :-
	retractall(root(_)),
	createNode(ID),
	assert(root(ID)),
	assert(predicted_q_value(ID, 0, 0)).
	
createNode(ID) :-
	makeNewId(ID),
	assert(leaf(ID)).
	
makeNewId(ID) :-
	currentNodeId(ID),
	ID2 is ID + 1,
	retractall(currentNodeId(_)),
	asserta(currentNodeId(ID2)).

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

% (If list is empty, assume default minimal value)
getHighestQValueForAnyActionFromCurrentState(_, [], N) :- reward_neg(N), !.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	rrl(true),
	!,
	get_best_by_binary_tree_estimate(ActionList, _, BestValue).

% --- DEPRECATED, UNSUPPORTED ---
% Variant without RRL/trees, just Q-learning.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	rrl(false),
	!,
	% state has changed; get new state
	% collect all elements of current state into list
	% only fluents, not statics or derived predicates
	findall(N, (currentState(N)), StateListUnsorted),
	sort(StateListUnsorted, StateInListForm),
	% All actions in actionlist are valid, but not all might have stored values!
	% So if none do, return -1 or default
	findall(Value, (qValueLearned(StateInListForm, InstantiatedAction, Value, _Count), member(InstantiatedAction, ActionList)), ListOfVals),
	%bagof???
	(ListOfVals == []
	->
	(reward_neg(N), BestValue = N)
	;
	max_list(ListOfVals,BestValue)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
matchingAncestorChain(ActionList, Action, Value) :-
	leaf(ID),
	getAncestorTests(ID, List),
	member(action(Action), List),
	member(Action, ActionList),
	allAreInCurrentStateOrActions(List),
	predicted_q_value(ID, Value, _).
	
% Note a node is its own ancestor
getAncestorTests(Node, List) :-
	recursiveGetAncestorTests(Node,[],List).

% +root & +leaf
recursiveGetAncestorTests(Node, _, []) :-
	root(Node),
	leaf(Node),
	!.
% +root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	root(Node), % but not a leaf...
	(test(Node, T) ; (print('recursiveGetAncestorTests 1 failed: no test\n'), trace, fail)),
	test(Node, T),
	!,
	append(WorkingList,[T],ReturnList).
% !root & +leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node, Node2),
	leaf(Node),
	!,
	recursiveGetAncestorTests(Node2, WorkingList, ReturnList).
% !root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node, Node2),
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
% once there is an action in the path from root to (current) leaf, then every test for any other action will never partition the examples, 
% so the leaf will never split on that test.
	
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkForTreeSplits :-
	affectedLeavesThisConfig(ListToTest),
	(is_set(ListToTest) -> true ; trace),
	!,
	checkListForTreeSplits(ListToTest),
	!.
	
checkListForTreeSplits([]) :-
	!.
checkListForTreeSplits([LeafID|Tail]) :-
	% If no variance, do nothing.
	calculateVarianceAtLeafNode(LeafID, TotalVariance),
	TotalVariance =< 0.0,
	!,
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	checkListForTreeSplits(Tail).
checkListForTreeSplits([LeafID|Tail]) :-
	% 1. Calculate current total variance for leaf node
	calculateVarianceAtLeafNode(LeafID, TotalVariance),
	% 2. Get list of unique possible tests:
	% - Collate everything from partial states in examples at the leaf
	% - Sort this list
	findall(	Literal,
				(leaf_stored_example(LeafID, Content, _, _, _), member(Literal, Content)), % Doesn't get any test that they all say 'no' to, but that's fine
				List1),
	sort(List1, List2),
	findall(	[ProspectiveTest, MeanProportion, VarYesProportion, VarNoProportion],
				(
					member(ProspectiveTest, List2),
					calculateYesNoVariancesForTest(LeafID, ProspectiveTest, VarianceYes, VarianceNo),
					% We already know TotalVariance > 0 otherwise the previous clause would have kicked in
					VarYesProportion is VarianceYes / TotalVariance,
					VarNoProportion  is VarianceNo  / TotalVariance,
					MeanProportion is (VarYesProportion + VarNoProportion)/2
				),
				ListOfTestPossibilities
				),
	% First, pick the lowest-variance test where there is some sufficient reduction in variance
	member(SelectedTest, ListOfTestPossibilities),
	SelectedTest = [Test, MeanProportion, VarYesProportion, VarNoProportion],
	once((VarYesProportion =< 0.9 ; VarNoProportion  =< 0.9)),
	MeanProportion  =< 0.99,
	not( (member(OtherTest, ListOfTestPossibilities), OtherTest \== SelectedTest, OtherTest = [_, OtherMean, _, _], OtherMean < MeanProportion) ),
	splitNode(LeafID, Test), % Enacts split
	!,
	checkListForTreeSplits(Tail).

% Base - not enough variance improvement to split at all
checkListForTreeSplits([_LeafID|Tail]) :-
	!,
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	checkListForTreeSplits(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_output_stream(Stream) :-
	data_output_file(A),
	%current_mdp_index(N),
	%atom_concat(A, N, B),
	atom_concat(A, '.txt', Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simplest case: Report the magnitude of the highest Q-value in the tree.

storeRLStatisticsEachEpisode :-
	establish_convergence(Val),
	storeRLStats(Val).

% --- DEPRECATED, UNSUPPORTED ---
% Not storing RL stats for now
establish_convergence(-9001) :-
	% 1. Get list of leaf nodes touched this config
	affectedLeavesThisConfig(List),
	% 2. Store current predicted Q-value for each of those nodes in episode_high_val
	episode_count(EC),
	storeEpisodicValues(EC, List).
	
storeEpisodicValues(_, []) :- !.
storeEpisodicValues(EC, [A|B]) :-
	not(leaf(A)), % Might no longer be a leaf.
	!,
	storeEpisodicValues(EC, B).
storeEpisodicValues(EC, [A|B]) :-
	predicted_q_value(A, Val, _),
	assert(episode_high_val(EC, A, Val)),
	storeEpisodicValues(EC, B).	

storeRLStats(_Val) :-
	record_full_data_traces(false), !.

storeRLStats(Val) :-
	record_full_data_traces(true),
	rrl(true),
	!,
	get_output_stream(D),
	open(D, append, Stream),
	write(Stream,Val),
	write(Stream,'\n'),
	close(Stream).

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
	writef('End of an episode, learned values:\n'),
	printLearned.
printTableAtEndOfEpisode :-
	episode_count(N),
	( (0 is N mod 9000) ; (N < 1) ),
	total_config_count(Y),
	( (0 is Y mod 5) ; (Y < 1) ),
	!,
	writef('End of an episode, learned values:\n'),
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
	clause1count(C1), clause2count(C2), clause3count(C3),
	writef('Node splits by clauses: #1: '), print(C1), 
	writef('; #2: '), print(C2), 
	writef('; #3: '), print(C3),  
	nl,
	total_config_count(NumStats), writef('* Total static configs tried so far: '), print(NumStats), writef(' !'), nl,
	get_output_stream(Dat),
	writef(Dat),
	nl.
	
printTree(ID, Symbol, Count) :-
	leaf(ID),
	!,
	printSpaces(Count),
	writef(Symbol), writef(' #'), print(ID), writef('  '),
	( test(ID, T) -> (print(T), writef('?')) ; writef('[no test]') ),
	!,
	writef(' : [value '),
	predicted_q_value(ID, Val, CountOfActions),
	print(Val),
	writef('], [count '),
	print(CountOfActions),
	writef('], [variance '),
	calculateVarianceAtLeafNode(ID, Variance),
	print(Variance),
	writef(']'),
	nl.

printTree(ID, Symbol, Count) :-
	child_y(ID, IDY),
	child_n(ID, IDN),
	printSpaces(Count),
	writef(Symbol), writef(' #'), print(ID), writef('  '),
	( test(ID, T) -> (print(T), writef('?')) ; writef('[no test]') ),
	!,
	nl,
	New is Count + 1,
	printTree(IDY, 'Y', New),
	printTree(IDN, 'N', New).
	
printLeafExamples(ID, Count) :-
	printSpaces(Count),
	printSpaces(1),
	leaf_stored_example(ID, Remainder, ExCount, _SumOfQ, _SumOfSquaredQ),
	writef('{'),
	print(Remainder),
	writef('} : '),
	print(ExCount),
	writef(' examples.\n'),
	fail.
printLeafExamples(_, _) :- !.

printSpaces(0) :- !.
printSpaces(Count) :-
	!,
	writef('  '),
	N is Count - 1,
	printSpaces(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- DEPRECATED, UNSUPPORTED ---

% To guarantee convergence to the true Q-value, we slowly decrease the learning rate parameter 'gamma' (current_learning_rate_parameter) over time.
% Although in practice, it's often sufficient to simply use a small gamma.
% The method is: Each iteration/episode, current_learning_rate_parameter -= initial_learning_rate_parameter/(totalnumruns*2 +1).
% So eventually it reaches half.
updateLearningValue :-
	initial_num_of_episodes(A),
	%episode_countdown(B), % Could use this, but don't need to.
	initial_learning_rate_parameter(X),
	current_learning_rate_parameter(Y),
	Val1 is (A * 2) + 1, % Doubled for now so it narrows down to half the original
	Val2 is X / Val1,
	New is Y - Val2,
	retractall(current_learning_rate_parameter(_)),
	assert(current_learning_rate_parameter(New)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

