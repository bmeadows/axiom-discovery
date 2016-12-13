
/****************************************************************
 * Section 0: Dynamic predicates
 */
 
:- dynamic register/3, struc/3, currentState/1, goalState/1, goalreached/0, episode_count/1, loud/0, quiet/0, current_learning_rate_parameter/1, initial_num_of_episodes/1, 
currentNodeId/1, root/1, leaf/1, parent/2, child_y/2, child_n/2, test/2, predicted_q_value/3, leaf_stored_example/5, split_count/1, no_split_count/1, lastActionWas/1, 
end_by_goal_counter/1, end_by_domain_counter/1, clause1count/1, clause2count/1, clause3count/1, finaloutputfile/1, domain_specified_end/0, qValueLearned/5, leaf_generalised/1,
data_output_file/1, total_config_count/1, learned_config/1, episode_high_val/3, examplepathrecord/4, semifinalexample/5, finalexample/4, candidate_axiom/6, final_axiom/6, 
initial_learning_rate_parameter/1, last_split_at/1, sumQCollector/1, countCollector/1, random_sampling_count/1, number_of_random_sample_draws_to_make/1, affectedLeavesThisConfig/1.

/****************************************************************
 * Section 1: Settings
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Include ONE of the following domains %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- include(domain_blocks_clean).
:- include(domain_butler_clean).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Include ONE of the following run modes %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(run_verbose).
%:- include(run_quiet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include ONE of the following debug modes %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- include(registers_off).
:- include(registers_on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include ONE of the following randomizers %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- include(seed_random).
:- include(seed_fixed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Most important parameter to vary %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percent_of_object_config_space_to_search(1). % Percentage of the space of object configurations to explore

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Other framework parameters %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sample_multiplier(100). % DEFAULT, canonical value experimentally derived

initial_learning_rate_parameter(0.1).

current_learning_rate_parameter(0.1).

explore_parameter(0.1). % Policy: 'explore' vs 'exploit'

reward_pos(10). % RL positive reward value
reward_neg(0.0). % RL negative reward value

record_full_data_traces(false). % Change this to 'true' to record all the data files (deprecated?)

parsimony_literal_penalty(0.02).
pos_or_neg_clause_limit_per_axiom(2).

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

% Deprecated, do not change; only closed-world relational learning works
closed_world(true).
rrl(true).

/****************************************************************
 * Section 2: Predicate lists
 */

% Tree predicates:

% root(id).
% child_y(id, id1).
% child_n(id, id2).
% parent(child, parent).
% leaf(id).
% test(id, test). - 'test' can be action(Action) or fluent(Contents) or attr(Value)
% predicted_q_value(leaf_id, value, actioncount).
% leaf_stored_example(leaf_id, remainderdescriptionlist, count, sumOfQ, sumOfSquaredQ).

% Configuration predicates:

% learned_config(SORTED_LIST_OF_ATTRS).
% num_possible_attribute_configs(n).
% total_config_count(n).
% percent_of_object_config_space_to_search(n).

/****************************************************************
 * Section 3: Running
 */
 
run(File) :-
	open('out.txt', write, OStream), % Default
	set_output(OStream),
	assert(finaloutputfile(File)),
	enter_register(overall),
	go,
	exit_register(overall),
	close(OStream),
	printRegisters.

go :-
	go(quiet).
	
go(loud) :-
	assert(loud),
	begin_rl.
	
go(quiet) :-
	assert(quiet),
	%time(begin_rl).
	begin_rl.

begin_rl :-
	init_random,
	makeNewRoot,
	learnOneObjectConfiguration.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

learnOneObjectConfiguration :-
	num_possible_attribute_configs(X),
	total_config_count(C),
	percent_of_object_config_space_to_search(P),
	C >= (X * P)/100,
	!,
	writefX('Necessary number of attribute configurations learned. Beginning generalisation step.\n'),
	enter_register(generalisation),
	doGeneralisation,
	exit_register(generalisation).

learnOneObjectConfiguration :-
	Str = 'data-', get_time(TimeStamp), atom_concat(Str, TimeStamp, Stream), 
	retractall(data_output_file(_)),
	assert(data_output_file(Stream)),
	setRandomInitialObjectConfig,
	makePrincipledChangeToObjectConfig,
	doAllEpisodes,
	total_config_count(A), B is A +1,
	retractall(total_config_count(_)),
	assert(total_config_count(B)),
	!,
	learnOneObjectConfiguration.
	
doAllEpisodes :-
	retractall(episode_count(_)),
	assert(episode_count(0)),
	retractall(episode_high_val(_,_,_)),
	retractall(affectedLeavesThisConfig(_List)),
	assert(affectedLeavesThisConfig([])),
	performEpisodesForOneConfig.
	
% Q-values converged for this configuration
performEpisodesForOneConfig :-
	episode_count(N),
	N > 20,
	enter_register(check_q_value_convergence),
	q_values_have_converged,
	exit_register(check_q_value_convergence),
	!,
	writefX('q converged, episode #'),
	printX(N), nlnl,
	checkForObjectConfigChangeCycle.
	
% Q-values have failed to converge
performEpisodesForOneConfig :-
	episode_count(N),
	N > 200,
	!,
	writefX('q did not converge: over episode limit of '), printX(N), nlnl,
	checkForObjectConfigChangeCycle.

% Q-values have not yet converged (continue)
performEpisodesForOneConfig :-
	episode_count(X),
	!,
	retractall(goalreached),
	retractall(lastActionWas(_)),
	resetStateAtRandom,
	
	enter_register(ep_steps),
	do_steps_in_episode, % Main function
	exit_register(ep_steps),
	
	enter_register(tree_split_checking),
	checkForTreeSplits,
	exit_register(tree_split_checking),
	
	printTableAtEndOfEpisode,
	!,
	Y is X+1,
	retractall(episode_count(X)),
	assert(episode_count(Y)),
	% updateLearningValue, % Elided
	storeRLStatisticsEachEpisode,
	performEpisodesForOneConfig.
	
checkForObjectConfigChangeCycle :-
	markCurrentConfigurationLearned,
	checkObjectConfigChange.

checkObjectConfigChange :-
	total_config_count(C),
	percent_of_object_config_space_to_search(X),
	C > X,
	!. % Finished episodes; return to recursive call
checkObjectConfigChange :-
	makePrincipledChangeToObjAttConfiguration,
	!,
	total_config_count(A), B is A +1,
	retractall(total_config_count(_)),
	assert(total_config_count(B)),
	doAllEpisodes.

% Check for duplicate configurations
makePrincipledChangeToObjectConfig :-
	checkForFurtherChangeToConfig.
	
makePrincipledChangeToObjAttConfiguration :-
	enter_register(change_obj_att_configs),
	randomWalkCurrentObjectConfig,
	exit_register(change_obj_att_configs),
	!,
	checkForFurtherChangeToConfig.

checkForFurtherChangeToConfig :-
	getCurrentObjectConfig(X),
	learned_config(X),
	!,
	makePrincipledChangeToObjAttConfiguration.
checkForFurtherChangeToConfig :-
	getCurrentObjectConfig(X),
	not(learned_config(X)),
	!.
	
getCurrentObjectConfig(X) :-
	findall(	N,
				(currentState(N), N = attr(_)),
				List1),
	sort(List1, X).

% Catch case
randomWalkCurrentObjectConfig :-
	num_possible_attribute_configs(Limit),
	total_config_count(N),
	N >= Limit, % safety measure
	trace,
	!.

randomWalkCurrentObjectConfig :-
	getCurrentObjectConfig(List),
	domainChangeObjectAtts(List).
	
markCurrentConfigurationLearned :-
	getCurrentObjectConfig(List),
	assert(learned_config(List)).
	
% Assume "convergence" means the best learned value in the MDP doesn't change by more than 1% for 20 episodes in a row.
q_values_have_converged :-
	episode_count(EC),
	last_split_at(Episode),
	Diff is EC - Episode,
	Diff > 20, % Splitting any node effectively resets the convergence counter
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
	% The overall q_values_have_converged rule will fail.
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

applyActionAndReportOracleOutcome(Action, RewardValue) :-
	goalState(GOAL),
	applyActionToStateAndUpdateHistory(Action),
	(goalAchieved(GOAL) -> (assert(goalreached), reward_pos(X), RewardValue = X) ; (reward_neg(Y), RewardValue = Y)).

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
	writefX('Action application has failed. Problem: '),
	writefX(Action),
	nlnl,
	trace.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
doGeneralisation :-
	
	findall([A1], leaf(A1), List0), length(List0,Size0),
	writefX('===> Number of leaves in the BDT: '), printX(Size0), nlnl,
	
	writefX('Extracting examples...'), nlnl,
	makeGeneralisedExamples,
	findall([ID, Attributes, S, C], examplepathrecord(ID, Attributes, S, C), List1), length(List1,Size1),
	writefX('===> Number of \'examplepathrecord\' records including duplicates; nonactions removed: '), printX(Size1), nlnl,
	
	writefX('Constructing full set of generalised examples...'), nlnl,
	constructCandidateAxioms,
	findall([CC,DD,EE,FF,GG], semifinalexample(CC, DD, EE, FF, GG), List2), length(List2,Size2),
	writefX('===> Number of \'semifinalexample\' records produced by finding subsets of each example: '), printX(Size2), nlnl,
		
	% Measure how many examples are strictly more specific than others, and report on that number.
	writefX('Making candidate tree\n'),
	makeCandidateTree,
	
	findall([A2A,B2B,C2C,D2D,E2E,F2F], candidate_axiom(A2A, B2B, C2C, D2D, E2E, F2F), List3), length(List3,Size3),
	writefX('===> Number of candidate axioms: '), printX(Size3), nlnl,
	
	writefX('\nCandidate axioms, pre parsimony:\n'),
	%listing(candidate_axiom/6),
	
	writefX('Extracting top candidate axioms. FINAL CHOICES...\n'),
	adjustAxiomsForParsimony,
	writefX('Sorting top candidate axioms. FINAL CHOICES...\n'),
	sortFinalAxioms,
	%listing(final_axiom/6),
	
	writefX('Finalising top candidate axioms, considering generality (universal filter). FINAL CHOICES...\n'),
	finalAxiomQualityImprovement,
	extractAndReportTopAxioms(0),
	
	writefX('Checking top (max ten) candidates with oracle. FINAL CHOICES...\n'),
	checkTopCandidatesWithOracle,
	
	finaloutputfile(OO),
	open(OO,write,OStr),
	set_output(OStr),
	extractAndReportTopAxioms('final'),
	close(OStr),
	
	writefX('Lifting final candidates\n'),
	liftCandidates,
	
	extractAndReportTopAxioms('final'),
	
	!.

% FIRST PASS: Transform all leaves into "path records" of the tests from them to root. Keep only attributes and actions. Delete leaves in their wake.
	% TODO come back to this to make sure everything is conceptually sound; will certainly need to change if we start 
	% learning constraints to do with anything other than attributes.
makeGeneralisedExamples :-
	% 1. Pick any leaf L
	leaf(ID),
	not(leaf_generalised(ID)),
	% 2. Collect the partial state/action described by the path from L to root, looking only at internal nodes
	getStateActionInfoOnPath(ID, StateNeg, StatePos),
	makeExamplePathRecords(ID, StateNeg, StatePos),
	% 6. Remove leaves
	assert(leaf_generalised(ID)),
	!,
	makeGeneralisedExamples.
makeGeneralisedExamples :-
	not( (leaf(ID), not(leaf_generalised(ID))) ),
	!.

makeExamplePathRecords(ID, StateNeg, StatePos) :-
	findall(X1, (member(attr(X1), StatePos)), AttsPos1),
	findall(X2, (member(attr(X2), StateNeg)), AttsNeg1),
	sort(AttsPos1, AttsPos),
	sort(AttsNeg1, AttsNeg),
	Attributes = [AttsPos, AttsNeg],
	domainGoalAction(DGA),
	(
		% Case 1: a positive action is mentioned in StatePos and it matches the one we care about
		% Case 2: look for the action we care about specified within the actual examples at the leaf
		member(action(DGA), StatePos)
	->
		addMatchingSubsetAsPathRecord(ID, Attributes)
	;
		addMatchingSubsetAsPathRecord(ID, Attributes, DGA)
	).

% Pick one example at this leaf at random
addMatchingSubsetAsPathRecord(ID, AttrsPosAndNeg) :-
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, _, _, _, _), LeafExample), LeafExamples),
	addANewPathRecord(ID, AttrsPosAndNeg, LeafExamples).

% Pick one example at this leaf at random, as long as it has DGA in its remainder
addMatchingSubsetAsPathRecord(ID, AttrsPosAndNeg, DGA) :-
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, ExRemainder, _, _, _), LeafExample, member(action(DGA), ExRemainder)), LeafExamples),
	addANewPathRecord(ID, AttrsPosAndNeg, LeafExamples).
	
addANewPathRecord(_, _, []) :- !.
addANewPathRecord(_, [[],[]], _) :- !.
addANewPathRecord(ID, [AttsYes,AttsNo], LeafExamples) :-
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(0.0)),
	assert(countCollector(0)),
	amassPathRecord(ID, LeafExamples),
	sumQCollector(SumQ),
	countCollector(Count),
	assert(examplepathrecord(ID, [AttsYes,AttsNo], SumQ, Count)),
	!.
addANewPathRecord(ID, [AttsYes,AttsNo], LeafExamples) :- printX([ID, [AttsYes,AttsNo], LeafExamples]), trace.

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

subsetp([], []).
subsetp([E|Tail], [E|NTail]):- subsetp(Tail, NTail).
subsetp([_|Tail], NTail):- subsetp(Tail, NTail).
  
% SECOND PASS: Combine path records, transforming the generalised forms into 'semifinalexample', and delete path records
% Note that we go into this having constructed the examples and in doing so removed all the fluent / physical config information
% that would have been all that distinguished some of them. Hence we now expect to have duplicates in our semifinalexample([S1,S2], Q, Worst, Count) entries.
% Hence "training samples".
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
	
% Done
updateCandidateAxioms([], _S, _C, _ID) :- !.

% Candidate exists
updateCandidateAxioms([A|B], Sum, Count, ID) :-
	%candidate exists,
	A = [PosList,NegList],
	semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList),
	!,
	%update candidate,
	Q2 is QV + Sum,
	Count2 is OldCount + Count,
	TempMean is Sum/Count,
	((TempMean < Worst) -> Worst2 = TempMean ; Worst2 = Worst),
	(member(ID, OldIDList) -> NewIDList = OldIDList ; append(OldIDList, [ID], NewIDList)),
	%retract old candidate,
	retractall(semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList)),
	assert(semifinalexample([PosList,NegList], Q2, Worst2, Count2, NewIDList)),
	%recurse
	updateCandidateAxioms(B, Sum, Count, ID).

% Make new candidate	
updateCandidateAxioms([A|B], Sum, Count, ID) :-
	%assert new candidate.
	A = [PosList,NegList],
	not(semifinalexample([PosList,NegList], _, _, _, _)),
	!,
	TempMean is Sum/Count,
	assert(semifinalexample([PosList,NegList], Sum, TempMean, Count, [ID])),
	%recurse
	updateCandidateAxioms(B, Sum, Count, ID).

% Shouldn't be possible
updateCandidateAxioms(_, _, _, _) :- trace.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getStateActionInfoOnPath(LeafID, SortedStateNeg, SortedStatePos) :-
	recursiveGetStateActionInfoOnPath(LeafID, [], SortedStateNeg, [], SortedStatePos).

% Base case
recursiveGetStateActionInfoOnPath(Node, CurrentStateNeg, ReturnSortedStateNeg, CurrentStatePos, ReturnSortedStatePos) :-
	root(Node),
	sort(CurrentStateNeg, ReturnSortedStateNeg),
	sort(CurrentStatePos, ReturnSortedStatePos),
	!.

% Non-root: leaf or intermediate node (note leaf+root is covered above, though)
% Thus is a positive or negative example of parent test (action/fluent/attr)
recursiveGetStateActionInfoOnPath(Node, CurrentStateNeg, ReturnSortedStateNeg, CurrentStatePos, ReturnSortedStatePos) :-
	not(root(Node)),
	parent(Node, Node2),
	(test(Node2, Test) ; (printX('recursiveGetStateActionInfoOnPath failed: no test\n'), trace, fail)),
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
	writefX('Finalising all semifinalexample candidates\n'),
	reduceToHighQualityCandidates,
	findall([A,B,C,D], finalexample(A, B, C, D), List3), length(List3,Size3),
	writefX('===> Number of reduced \'finalexample\' records remaining: '), printX(Size3), nlnl,
	establishSupportForCandidates,
	writefX('===> Now supported with random examples.'), nlnl,
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
	
%%%%%%%%%%%%%%%%%

% Next have to do an entire sweep through adding the values of random examples to the appropriate candidates

establishSupportForCandidates :-
	getAllPossibleTests(DomainTestsList),
	length(DomainTestsList, DTL),
	findall(	Test,
				test(_SomeNode,Test),
				TestsUsed
			),
	sort(TestsUsed, UniqueTestsUsed),
	length(UniqueTestsUsed, UIN),
	N is (DTL - UIN) +1,
	findall(	ALength,
				( leaf(ID), getAncestorTests(ID, Ancestors), length(Ancestors, ALength) ),
				PathLengths
			),
	sum_list(PathLengths, SL),
	length(PathLengths, LL),
	P is SL / LL,
	% take average
	Average is (N + P) / 2,
	sample_multiplier(SM),
	Total is Average * SM,
	assert(number_of_random_sample_draws_to_make(Total)),
	writefX('Sample multiplier: '), printX(SM), nlnl,
	writefX('Path length: '), printX(P), 
	writefX('; unused attribute tests: '), printX(N), 
	writefX('; average: '), printX(Average), nlnl,
	writefX('Number of random samples to draw: '), printX(Total), nlnl, 
	% Now actually do the support step
	!,
	findall(X, (X = leaf_stored_example(_, _, _, _, _), X), AllExamplesList),
	length(AllExamplesList, AELSize),
	writefX('Total set of samples we can draw from: '), printX(AELSize), nlnl,
	supportWithRandomExamples(AllExamplesList).

% Base case: Done
supportWithRandomExamples(_) :-
	number_of_random_sample_draws_to_make(X),
	random_sampling_count(Y),
	Y >= X, % (Not an integer)
	writefX('\nFinished support step: Drew '),
	printX(Y),
	writefX(' random examples.\n'),
	!.
% Base case: Ran out of possible samples early
supportWithRandomExamples([]) :-
	writefX('\n*****\nFinished support step: Ran out of possible samples early!\n*****\n'),
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

drawNewRandomSampleFromBDT(ExampleList, ElidedExampleList) :-
	% 1. Get a random example from a random leaf.
	% 2. Find the full example by adding (a) its locally stored object attributes (that match domain-defined testable ones) to (b) its path to root.
	% 3. Find each subset of the full example that matches a candidate.
	% 4. Add the example's value to that candidate's, and increment the candidate's count.
	% 5. Delete the example so it can't be drawn again.
	random_member(RandomExample, ExampleList),
	select(RandomExample, ExampleList, ElidedExampleList), % Can't be used again.
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
	( addToSampleCount(-1) )
	;
	(
		reduceToObjAttsAndRemoveWrappers(PositiveExampleAttributes1, PositiveExampleAttributes),
		reduceToObjAttsAndRemoveWrappers(NegativeExampleAttributes1, NegativeExampleAttributes),
		%
		findall(	Candidate,
					someSubsetMatchesSomeCandidate(PositiveExampleAttributes, NegativeExampleAttributes, Candidate),
					CandidatesToAdjust0
				),
		sort(CandidatesToAdjust0, CandidatesToAdjust), % Needed?
		addToAllCandidates(CandidatesToAdjust, AddedValue)
	)).

reduceToObjAttsAndRemoveWrappers(List1, List2) :-
	findall(X, member(attr(X), List1), List2).

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

%%%%%%%%%%%%%%%%%

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
% Base
finaliseCandidates :- !, changeExamplesToCandidates.

changeExamplesToCandidates :-
	finalexample([AttsYes,AttsNo], Q, Worst, Count),
	retractall(finalexample([AttsYes,AttsNo], Q, Worst, Count)),
	domainGoalAction(Act),
	assert(candidate_axiom(raw, not_occurs(Act), [AttsYes,AttsNo], Q, Worst, Count)),
	!,
	% 3. Recursive call
	changeExamplesToCandidates.
changeExamplesToCandidates.


%%%%%%%%%%%%%%%%%


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

replaceByArity(0, _Act, _AttsYes, _AttsNo, _) :- !.
replaceByArity(Arity, Act, AttsYes, AttsNo, AlphabetVariableList) :-
	Arity > 0,
	!,
	nth1(Arity, AlphabetVariableList, NewVariable), % Get nth starting at 1
	(arg(Arity, Act, AtomicValue)),
	setarg(Arity, Act, NewVariable),
	replaceAtomWithVar(AtomicValue, NewVariable, AttsYes),
	replaceAtomWithVar(AtomicValue, NewVariable, AttsNo),
	NewArity is Arity - 1,
	!,
	replaceByArity(NewArity, Act, AttsYes, AttsNo, AlphabetVariableList).

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
	true), % if it's not in it...
	!,
	replaceAtomWithVar(AtomicValue, NewVariable, Tail).
	
	
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
	

% (a) Only take absolute top tier
% (b) Rank by total number of supporting examples
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
	
	% 1. better than all other zero-ranks (i.e. unranked)
	not( (candidate_axiom(RequiredMean,_,_,_Q2,_,Count2), Count2 > Count) ),
	
	!,
	% 2. set it to the next rank down from the current lowest nonzero rank
	((final_axiom(Number,_,_,_,_,_), not( (final_axiom(Number2,_,_,_,_,_), Number2 > Number) ), NewRank is Number + 1)
	;
	NewRank = 1),
	
	% Basically, change the rank each time, starting from best (highest mean), i.e., smallest number rank
	
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
% 2. N is a more specific form of an already-confirmed axiom.
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
% 3. N is a more general form of an already-confirmed axiom. (unlikely, but they could have same number, so technically possible)
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_),
	M < N,
	subset(Yes1, Yes2), % Yes1 is subset of Yes2
	subset(No1, No2),
	!,
	retractall(final_axiom(M,_,[Yes2,No2],_,_,_)),
	checkCandidateAxiom(N).
% 4. Base case
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
	((NewRank < 11) -> assert(final_axiom(NewRank,Act,B,Mean,Worst,C)) ; true),
	% 3. Recurse
	!,
	reRankAxioms(NewRank).
% Base case
reRankAxioms(_) :- !.

checkTopCandidatesWithOracle :-
	% For each remaining candidate C in turn:
	% 1. Start with domain-defined physical state
	% 2. Test random obj att configurations until you get an att config S where the action succeeds
	% 3. Now:
	%		(a) Modulate S to S' by the minimum necessary changes such that the partial configuration described by C holds in S'
	% 		(b) Poll the oracle as to whether the action still succeeds in S'
	% 		(c) If it does, discard the candidate axiom C
	
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
	extractAndReportTopAxioms(5),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	oracleFilterCandidates(1),
	reRankAxioms(0),
	oracleFilterCandidates(1),
	reRankAxioms(0),
	oracleFilterCandidates(1),
	reRankAxioms(0),
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(10).
	
	
oracleFilterCandidates(N) :-
	not(final_axiom(N,_Act,_B,_Mean,_Worst,_C)),
	!.
oracleFilterCandidates(N) :-
	writefX('Oracle filter:\n  Checking candidate #'), printX(N), nlnl,
	setVirtuousStateRandomly,
	!,
	final_axiom(N,Act,[True,False],Mean,Worst,C),
	adjustVirtuousState(True,False),
	(not(actionFailsInDomain)
		->
		(writefX('  Pruning candidate #'), printX(N), nlnl,
		retractall(final_axiom(N,Act,[True,False],Mean,Worst,C)))
		;
		true
	),
	M is N +1,
	!,
	oracleFilterCandidates(M).
	
setVirtuousStateRandomly :-
	resetStateAtRandom,
	setRandomInitialObjectConfig,
	storeCurrentStateAndConfig(List),
	!,
	(actionFailsInDomain -> setVirtuousStateRandomly ; restoreStateAndConfig(List)).
	
actionFailsInDomain :-
	domainGoalAction(Action),
	not(validAction(Action)), % PHYSICAL configuration is wrong for it to work
	!.
actionFailsInDomain :-
	domainGoalAction(Action),
	applyActionToStateAndUpdateHistory(Action), 
	goalState(GOAL),    % 1. This defines an unexpected failure;
	goalAchieved(GOAL), % 2. That's why this is positive
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
	not( (member(F,False), currentState(attr(F))) ),
	not( (member(T,True), not(currentState(attr(T)))) ),
	!.
adjustVirtuousState(True,False) :-
	swapAllOutFromState(False), % Replaces with some attribute
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

swapOut(ObjectAttribute) :-
	not( currentState(attr(ObjectAttribute)) ),
	!.
swapOut(ObjectAttribute) :-
	currentState(attr(ObjectAttribute)),
	retractall(currentState(attr(ObjectAttribute))),
	domain_attribute_alternatives(ObjectAttribute, AltList),
	random_member(Alt, AltList),
	assert(currentState(attr(Alt))).
swapIn(ObjectAttribute) :-
	currentState(attr(ObjectAttribute)),
	!.
swapIn(ObjectAttribute) :-
	domain_attribute_alternatives(ObjectAttribute, AltList),
	deleteAllObjAtts(AltList),
	assert(currentState(attr(ObjectAttribute))).
	
deleteAllObjAtts([]) :- !.
deleteAllObjAtts([A|B]) :-
	retractall(currentState(attr(A))),
	deleteAllObjAtts(B).


% % % % %

extractAndReportTopAxioms('final') :-
	writef('Number of oracle passes: final\n'),
	printAxiomListRecursive(1, true),
	writef('\n'),
	!.
	
extractAndReportTopAxioms(NToReport) :-
	writefX('\n------------------------\n'),
	writefX('Number of oracle passes: '), printX(NToReport), nlnl,
	printAxiomListRecursive(1, false),
	writefX('\n------------------------\n').

% Print list by rank
printAxiomListRecursive(Rank, _) :-
	Rank > 99,
	!.
printAxiomListRecursive(Rank, _) :-
	not(final_axiom(Rank, _Act, _YN, _QMean, _Worst, _Count)),
	!.
printAxiomListRecursive(Rank, true) :-
	final_axiom(Rank, Act, [Yes,No], QMean, Worst, Count),
	print(Rank), writef('. '),
	((Rank < 10) -> writef(' ') ; true), % Zero pad
	printAxiomNicelyWithoutNewline(Act, Yes, No, true),
	writef('  [mean '), print(QMean), writef('], [worst '), print(Worst), writef('], [support: '), print(Count), writef(' examples]'), nl,
	NewRank is Rank + 1,
	printAxiomListRecursive(NewRank, true).
printAxiomListRecursive(Rank, false) :-
	final_axiom(Rank, Act, [Yes,No], QMean, Worst, Count),
	printX(Rank), writefX('. '),
	((Rank < 10) -> writefX(' ') ; true), % Zero pad
	printAxiomNicelyWithoutNewline(Act, Yes, No, false),
	writefX('  [mean '), printX(QMean), writefX('], [worst '), printX(Worst), writefX('], [support: '), printX(Count), writefX(' examples]'), nlnl,
	NewRank is Rank + 1,
	printAxiomListRecursive(NewRank, false).

printAxiomNicelyWithoutNewline(Act, Yes, No, true) :-
	print(Act),
	writef(' :- '),
	writeAllInList(Yes, true),
	writeAllInListNegated(No, true),
	writef('end.').
printAxiomNicelyWithoutNewline(Act, Yes, No, false) :-
	printX(Act),
	writefX(' :- '),
	writeAllInList(Yes, false),
	writeAllInListNegated(No, false),
	writefX('end.').

writeAllInList([],_) :- !.
writeAllInList([A|B],true) :-
	!,
	print(A), writef(', '), writeAllInList(B,true).
writeAllInList([A|B],false) :-
	!,
	printX(A), writefX(', '), writeAllInList(B,false).

writeAllInListNegated([],_) :- !.
writeAllInListNegated([A|B],true) :-
	!,
	writef('not('), print(A), writef('), '), writeAllInListNegated(B,true).
writeAllInListNegated([A|B],false) :-
	!,
	writefX('not('), printX(A), writefX('), '), writeAllInListNegated(B,false).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_steps_in_episode :-
	one_step.

one_step :-
	enter_register('ep_steps:goalreached'),
	goalreached,
	%
	end_by_goal_counter(X),
	retractall(end_by_goal_counter(_)),
	Y is X + 1,
	assert(end_by_goal_counter(Y)),
	exit_register('ep_steps:goalreached'),
	!.
	
one_step :-
	enter_register('ep_steps:domainend'),
	domain_specified_end,
	%
	end_by_domain_counter(X),
	retractall(end_by_domain_counter(_)),
	Y is X + 1,
	assert(end_by_domain_counter(Y)),
	exit_register('ep_steps:domainend'),
	!.
	
one_step :-
	rrl(true),
	!,
	enter_register('ep_steps:main'),
	
	enter_register('ep_steps:main1'),
	
	enter_register('ep_steps:main1:a'),
	findall(N, currentState(N), StateListUnsorted),
	exit_register('ep_steps:main1:a'),
	enter_register('ep_steps:main1:b'),
	sort(StateListUnsorted, StateList),
	exit_register('ep_steps:main1:b'),
	
	exit_register('ep_steps:main1'),
	
	/*
	enter_register('ep_steps:main1s'),
	setof(N, currentState(N), StateList),
	exit_register('ep_steps:main1s'),
	exit_register('ep_steps:main1'),*/
		
	printlnX('Getting valid actions'),
	enter_register('ep_steps:main2'),
	findall(X, validAction(X), ActionList), % Actions that are valid in the current state
	exit_register('ep_steps:main2'),
	
	printlnX('Selecting an action...'),
	explore_parameter(Exp),
	random(F),
	
	(
		(F < Exp)
	->
		% Explore
		(printlnX('Getting random valid action'),
		random_member(Action, ActionList)
		)
	;
		% Policy
		(printlnX('Getting best valid action based on Q estimates'),
		
		enter_register('ep_steps:getBestByBDT'),
		pickBestByBinaryTreeEstimate(ActionList, Action),
		exit_register('ep_steps:getBestByBDT')
		)
		
	),
	
	printd('Applying action:  '),
	printlnX(Action),
	
	enter_register('ep_steps:applyAction'),
	applyActionAndReportOracleOutcome(Action, RewardValue),
	exit_register('ep_steps:applyAction'),
	
	% We are now in new state. Find max stored q-value from possible actions from this new state
	enter_register('ep_steps:findall1'),
	findall(Y, validAction(Y), ActionList2),
	exit_register('ep_steps:findall1'),
	
	enter_register('ep_steps:getBestByBDT'),
	getHighestQValueForAnyActionFromCurrentState(ActionList2, FutureValue),
	exit_register('ep_steps:getBestByBDT'),
	
	% Note you are passing in 'StateList', i.e., a record of the state that the system was in 
	% when entering this step, prior to applying the action.
	printlnX('Updating Q-value'),
	append(StateList, [action(Action)], StateDescriptionWithAction),
	
	enter_register('ep_steps:trickleQVDown'),
	trickleNewQValueExampleDownTree(StateDescriptionWithAction, RewardValue, FutureValue),
	exit_register('ep_steps:trickleQVDown'),
	
    !,
	exit_register('ep_steps:main'),
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
	(leaf(ID) -> true ; (writefX('split failed\n'), trace, fail)),
	% Update count of splits made
	(split_count(CurrentCount), NewCount is CurrentCount+1, retractall(split_count(_)), assert(split_count(NewCount))),
	% Update counter, preventing convergence for a while
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
	% Modify stored q-value for StateList:
	% Q(state, action) = current Q(state,action) +   gamma * (reward(state,action) + Max[Q(next state, all actions)] - current Q(state,action))
	root(ID),
	trickleDownTree(ID, StateDescriptionWithAction, RewardValue, FutureValue).
	
trickleDownTree(Node, StateDescriptionWithAction, RewardValue, FutureValue) :-
	not(leaf(Node)),
	!,
	test(Node, Test),
	(
	(member(Test, StateDescriptionWithAction) ; (Test = action(Action), member(Action,StateDescriptionWithAction), writefX('CHECK: l1275\n'))) % Test at node is positive?
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

	% Driessens et al: example Q =
	%   reward + (gamma)(max future Q)
	current_learning_rate_parameter(Gamma),
	_ExampleQValue is RewardValue + (Gamma * (FutureValue)),
	
	%updateExamplesStoredAtLeaf(Node, StateDescriptionWithAction, ExampleQValue). % Problematic; deprecated
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

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


calculateVarianceAtLeafNode(ID, Variance) :-
	enter_register('calculate_variance_at_leaf'),
	% Look at all examples at leaf
	enter_register('calculate_variance_at_leaf:findall1'),
	findall(Count, leaf_stored_example(ID, _, Count, _, _), CountList),
	exit_register('calculate_variance_at_leaf:findall1'),
	enter_register('calculate_variance_at_leaf:findall2'),
	findall(SumQ, leaf_stored_example(ID, _, _, SumQ, _), SumQList),
	exit_register('calculate_variance_at_leaf:findall2'),
	enter_register('calculate_variance_at_leaf:findall3'),
	findall(SumSquareQ, leaf_stored_example(ID, _, _, _, SumSquareQ), SumSquareQList),
	exit_register('calculate_variance_at_leaf:findall3'),
	enter_register('calculate_variance_at_leaf:sums'),
	sum_list(CountList, Count),
	sum_list(SumQList, SumQ),
	sum_list(SumSquareQList, SumSquareQ),
	!,
	exit_register('calculate_variance_at_leaf:sums'),
	enter_register('calculate_variance_at_leaf:find'),
	findVariance(Count, SumQ, SumSquareQ, Variance),
	exit_register('calculate_variance_at_leaf:find'),
	exit_register('calculate_variance_at_leaf').

findVariance(Count, Sum, SumOfSquared, Variance) :-
	(((Count == 0);(Sum == 0)) -> Variance = 100000
	;
	(
	MeanQ is Sum/Count,
	SquaredMeanQ is MeanQ * MeanQ,
	MeanSquareSum is SumOfSquared/Count,
	Variance is abs(MeanSquareSum - SquaredMeanQ)
	)),
	((Variance < 0) -> (trace, writefX('Variance failed?\n')) ; true).
	
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	
get_best_by_binary_tree_estimate(ValidActionList, BestAction, BestValue) :-
	enter_register('ep_steps:getBestByBDT:getAQpairs'),
	getAllActionQPairs(ValidActionList, ListOfPairs),
	exit_register('ep_steps:getBestByBDT:getAQpairs'),
	enter_register('ep_steps:getBestByBDT:part2'),
	( (ListOfPairs == []) -> (random_member(BestAction, ValidActionList), reward_neg(BestValue))
		;
	(
		member([_,BestValue], ListOfPairs),
		not( (member([_,Y], ListOfPairs), Y > BestValue) ),
		findall(Act2, member([Act2,BestValue], ListOfPairs), NewList),
		random_member(BestAction, NewList)
	)
	),
	exit_register('ep_steps:getBestByBDT:part2').
	

% (If list is empty, assume default low/negative value)
getHighestQValueForAnyActionFromCurrentState([], N) :- reward_neg(N), !.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	rrl(true),
	!,
	get_best_by_binary_tree_estimate(ActionList, _, BestValue).

% Variant without RRL/trees, just Q-learning.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	rrl(false),
	!,
	findall(N, (currentState(N)), StateListUnsorted),
	sort(StateListUnsorted, StateInListForm),
	% All actions in actionlist are valid, but not all might have stored values.
	% So if none do, return -1 or default
	findall(Value, (qValueLearned(StateInListForm, InstantiatedAction, Value, _Count), member(InstantiatedAction, ActionList)), ListOfVals),
	(ListOfVals == []
	->
	(reward_neg(N), BestValue = N)
	;
	max_list(ListOfVals,BestValue)).
	
%%%%%

getAllActionQPairs(ValidActionList, ListOfPairs) :- 
	policySearch(ValidActionList),
	%
	findall([Action,Value], struc(_LeafID1,Action,Value), Triples),
	((member(X,Triples),member(Y,Triples),X\=Y,X=[Id,_,_],Y=[Id,_,_]) -> (trace,print(1901),nl,print(X),nl,print(Y),nl) ; true),
	%
	findall([Action,Value], struc(_LeafID2,Action,Value), ListOfPairs),
	retractall(struc(_LeafID3,_Action,_QV)).

policySearch(_) :-
	root(ID),
	(not(test(ID, _)) ; leaf(ID)),
	!.
policySearch(ValidActionList) :-
	root(ID),
	!,
	enter_register('ep_steps:getBestByBDT:getAQpairs:policy'),
	recursiveBDTSearch(ValidActionList, [ ID ]),
	exit_register('ep_steps:getBestByBDT:getAQpairs:policy'),
	!.

recursiveBDTSearch([], _) :- !. % Don't bother if no actions are valid (this clause is probably never called because I think that's checked upstream)
recursiveBDTSearch(_, []) :- !. % Done when fringe is empty.
recursiveBDTSearch(_, [X|_]) :-
	var(X), trace.
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement = [ID,Action], leaf(ID), % Valid: Look up its Q-value and store it as some structure.
	predicted_q_value(ID, Value, _),
	assert(struc(ID, Action, Value)),
	!,
	recursiveBDTSearch(ValidActionList, Tail).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement \= [_,_], FringeElement = ID, leaf(ID), % No ancestor with a matching action, so remove it from the fringe and ignore it.
	!,
	recursiveBDTSearch(ValidActionList, Tail).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement = [ID,Action], not(leaf(ID)), % Internal node always has test - all that matters is that the new fringe entrant gets this ancestral 'Action' passed down.
	test(ID, action(_AnyAction)),
	!,
	% 1. Ignore 'yes' child
	% 2. Pass ancestral action down to 'no' child, adding it to fringe
	child_n(ID, ChildID),
	append(Tail, [[ChildID,Action]], NewFringe),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement = [ID,Action], not(leaf(ID)), % Internal node always has test - all that matters is that the new fringe entrant gets this ancestral 'Action' passed down.
	test(ID, Test), Test \= action(_), % NOT action
	(currentState(Test) -> child_y(ID, ChildID) ; child_n(ID, ChildID)),
	% 1. Pass ancestral action down to either 'yes' or 'no' child, adding it to fringe
	% 2. Ignore other child
	append(Tail, [[ChildID,Action]], NewFringe),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement \= [_,_], FringeElement = ID, not(leaf(ID)), % Internal node (always has test) with no Action to be passed down, UNLESS it's the test here.
	test(ID, action(Act)),
	!,
	% if on the permissible list, then its yes child inherits the new action and both children are added to fringe
	% else only no child is added to fringe
	child_y(ID, Yes),
	child_n(ID, No),
	(member(Act, ValidActionList) -> append(Tail, [[Yes,Act],No], NewFringe) ; append(Tail, [No], NewFringe)),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement \= [_,_], FringeElement = ID, not(leaf(ID)), % Internal node (always has test) with no Action to be passed down, UNLESS it's the test here.
	test(ID, Test), Test \= action(_), % NOT action
	(currentState(Test) -> child_y(ID, ChildID) ; child_n(ID, ChildID)),
	% 1. Pass ancestral action down to either 'yes' or 'no' child, adding it to fringe
	% 2. Ignore other child
	append(Tail, [ChildID], NewFringe),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).

	%%%%
	
getAllActionQPairs_SECONDTRY(ValidActionList, ListOfPairs) :-
	enter_register('ep_steps:getBestByBDT:?1'),
	findall(	[StateNeg, StatePos, Value],
				( leaf(ID), enter_register('ep_steps:getBestByBDT:?1-INNER'), enter_register('ep_steps:getBestByBDT:?1-getpath'), getStateActionInfoOnPath(ID, StateNeg, StatePos), exit_register('ep_steps:getBestByBDT:?1-getpath'), predicted_q_value(ID, Value, _), exit_register('ep_steps:getBestByBDT:?1-INNER') ),
				Triples ),
	((member(X,Triples),member(Y,Triples),X\=Y,X=[N,P,_],Y=[N,P,_]) -> (trace,print(X),nl,print(Y),nl) ; true),
	exit_register('ep_steps:getBestByBDT:?1'),
	% StateNeg or StatePos could contain: attr(_), action(_), etc
	% 1.
	enter_register('ep_steps:getBestByBDT:?2'),
	findall(	[Action, Value],
				( enter_register('ep_steps:getBestByBDT:?2-INNER'), member([Neg,Pos,Value],Triples), member(action(Action), Pos), member(Action, ValidActionList), enter_register('ep_steps:getBestByBDT:?2-chk'), allAreInCurrentStateOrActions(Pos), noneAreInCurrentState(Neg), exit_register('ep_steps:getBestByBDT:?2-chk'), exit_register('ep_steps:getBestByBDT:?2-INNER') ),
				ListOfPairs ),
	exit_register('ep_steps:getBestByBDT:?2'),
	% 2.
	((member(X,ListOfPairs),member(Y,ListOfPairs),X\=Y,X=[A,_],Y=[A,_]) -> (trace,print(X),nl,print(Y),nl) ; true).

noneAreInCurrentState([]).
noneAreInCurrentState([H|T]) :-	
	not(currentState(H)),
	noneAreInCurrentState(T).
	
getAllActionQPairs_ORIGINAL_VERSION(ValidActionList, ListOfPairs) :-
	findall(	[Action, Value],
				matchingAncestorChain(ValidActionList, Action, Value),
				ListOfPairs ).

matchingAncestorChain(ValidActionList, Action, Value) :-
	matchingAncestorChainX(ValidActionList, Action, Value)
	->
	true
	;
	fail.

matchingAncestorChainX(ValidActionList, Action, Value) :-
	enter_register('ep_steps:getBestByBDT:findall:aChain'),
	leaf(ID),
	getAncestorTests(ID, List),
	member(action(Action), List),
	member(Action, ValidActionList),
	enter_register('ep_steps:getBestByBDT:findall:aChain:all'),
	allAreInCurrentStateOrActions(List),
	exit_register('ep_steps:getBestByBDT:findall:aChain:all'),
	predicted_q_value(ID, Value, _),
	exit_register('ep_steps:getBestByBDT:findall:aChain').
	
/*	1. Pass in set AL
	2. Take any leaf
	3. Get set T of all its ancestors' tests
	4. Find any action in T
	5. Check it's in AL
	6. Check everything that is in T also currently holds in the world (or is an action)
*/
	
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
	(test(Node, T) ; (printX('recursiveGetAncestorTests 1 failed: no test\n'), trace, fail)),
	test(Node, T),
	!,
	append(WorkingList,[T],ReturnList).
% !root & leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node, Node2),
	leaf(Node),
	!,
	recursiveGetAncestorTests(Node2, WorkingList, ReturnList).
% !root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node, Node2),
	(test(Node, T) ; (printX('recursiveGetAncestorTests 2 failed: no test\n'), trace, fail)),
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
	
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
	
% For EACH current leaf node in the BDT, consider extending by splitting that leaf.
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
	enter_register('tree_split_checking:clause1'),
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	exit_register('tree_split_checking:clause1'),
	checkListForTreeSplits(Tail).
checkListForTreeSplits([LeafID|Tail]) :-
	enter_register('tree_split_checking:clause2'),
	% 1. Calculate current total variance for leaf node
	calculateVarianceAtLeafNode(LeafID, TotalVariance),
	% 2. Get list of unique possible tests:
	% - Collate everything from partial states in examples at the leaf
	% - Sort this list
	findall(	Literal,
				(leaf_stored_example(LeafID, Content, _, _, _), member(Literal, Content)),
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
	splitNode(LeafID, Test),
	exit_register('tree_split_checking:clause2'),
	!,
	checkListForTreeSplits(Tail).

% Final clause - not enough variance improvement to split at all
checkListForTreeSplits([_LeafID|Tail]) :-
	!,
	enter_register('tree_split_checking:clause3'),
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	exit_register('tree_split_checking:clause3'),
	checkListForTreeSplits(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_output_stream(Stream) :-
	data_output_file(A),
	atom_concat(A, '.txt', Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simplest case: Report the magnitude of the highest Q-value in the tree.

% Forget about storing RL stats for now
storeRLStatisticsEachEpisode :- !.

storeRLStatisticsEachEpisode :-
	establish_convergence(Val),
	storeRLStats(Val).

establish_convergence(-987654321) :-
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

printlnXXX(X) :-
	printd(X), printd('\n').

printd(X) :-
	loud,
	number(X),
	printX(X).
	
printd(X) :-
	loud,
	not(number(X)),
	writefX(X).

printd(_) :-
	quiet.

printState :-
	currentState(X),
	printd(' - '), printlnX(X),
	fail.
printState.

printTableAtEndOfEpisode :-
	loud,
	!,
	writefX('End of an episode, learned values:\n'),
	printLearned.
printTableAtEndOfEpisode :-
%trace,
	episode_count(N),
	( (0 is N mod 9000) ; (N < 1) ),
	total_config_count(Y),
	( (0 is Y mod 5) ; (Y < 1) ),
	!,
	writefX('End of an episode, learned values:\n'),
	printLearned.
printTableAtEndOfEpisode :- !.

% Prints the tree of tests and predicted QValues
printLearned :-
	root(ID),
	printTree(ID, '*', 0),
	writefX('Final counts:\n'),
	split_count(S),
	no_split_count(NS),
	writefX('Number of times a leaf node split: '), printX(S), nlnl,
	writefX('Number of times no node was split: '), printX(NS), nlnl,
	end_by_domain_counter(ED),
	end_by_goal_counter(EG),
	writefX('Number of episode-sequences ended by domain: '), printX(ED), nlnl,
	writefX('Number of episode-sequences ended by goal success: '), printX(EG), nlnl,
	clause1count(C1), clause2count(C2), clause3count(C3),
	writefX('Node splits by clauses: #1: '), printX(C1), 
	writefX('; #2: '), printX(C2), 
	writefX('; #3: '), printX(C3),  
	nlnl,
	total_config_count(NumStats), writefX('* Total obj attr configs tried so far: '), printX(NumStats), writefX(' !'), nlnl,
	get_output_stream(Dat),
	writefX(Dat),
	nlnl.
	
printTree(ID, Symbol, Count) :-
	leaf(ID),
	!,
	printSpaces(Count),
	writefX(Symbol), writefX(' #'), printX(ID), writefX('  '),
	( test(ID, T) -> (printX(T), writefX('?')) ; writefX('[no test]') ),
	!,
	writefX(' : [value '),
	predicted_q_value(ID, Val, CountOfActions),
	printX(Val),
	writefX('], [count '),
	printX(CountOfActions),
	writefX('], [variance '),
	calculateVarianceAtLeafNode(ID, Variance),
	printX(Variance),
	writefX(']'),
	nlnl.

printTree(ID, Symbol, Count) :-
	child_y(ID, IDY),
	child_n(ID, IDN),
	printSpaces(Count),
	writefX(Symbol), writefX(' #'), printX(ID), writefX('  '),
	( test(ID, T) -> (printX(T), writefX('?')) ; writefX('[no test]') ),
	!,
	nlnl,
	New is Count + 1,
	printTree(IDY, 'Y', New),
	printTree(IDN, 'N', New).
	
printLeafExamples(ID, Count) :-
	printSpaces(Count),
	printSpaces(1),
	leaf_stored_example(ID, Remainder, ExCount, _SumOfQ, _SumOfSquaredQ),
	writefX('{'),
	printX(Remainder),
	writefX('} : '),
	printX(ExCount),
	writefX(' examples.\n'),
	fail.
printLeafExamples(_, _) :- !.

printSpaces(0) :- !.
printSpaces(Count) :-
	!,
	writefX('  '),
	N is Count - 1,
	printSpaces(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

