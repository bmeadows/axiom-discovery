
/****************************************************************
 * Section 0: Dynamic predicates
 */
 
:- dynamic register/3, struc/3, currentState/1, goalState/1, goalreached/0, episode_count/1, loud/0, quiet/0, current_learning_rate_parameter/1, initial_num_of_episodes/1, 
currentNodeId/1, root/1, leaf/1, parent/2, child_y/2, child_n/2, test/2, predicted_q_value/3, leaf_stored_example/6, split_count/1, no_split_count/1, num_possible_attribute_configs/1,
end_by_goal_counter/1, end_by_domain_counter/1, clause1count/1, clause2count/1, clause3count/1, finaloutputfile/1, output_file_counter/1, output_file_error/1, output_file_time/1, domain_specified_end/0, qValueLearned/5, leaf_generalised/2,
data_output_file/1, total_config_count/1, learned_config/1, episode_high_val/3, examplepathrecord/5, semifinalexample/6, finalexample/5, candidate_axiom/7, final_axiom/7, lastActionWas/1, 
initial_learning_rate_parameter/1, last_split_at/1, sumQCollector/1, countCollector/1, random_sampling_count/1, number_of_random_sample_draws_to_make/1, affectedLeavesThisConfig/1,
suppress_text/0, number_of_configs_to_search/1, noiseChancePercent/1.

/****************************************************************
 * Section 1: Settings
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Include ONE of the following domains %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- include(domain_blocks_new).
:- include(domain_butler_new).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Include ONE of the following run modes %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(run_verbose).
%:- include(run_quiet).

% To remove constant textual feedback, uncomment the following.
%suppress_text.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include ONE of the following debug modes %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- include(registers_off).
:- include(registers_on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include ONE of the following randomizers %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(seed_random).
%:- include(seed_fixed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Most important parameter to vary %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percent_of_object_config_space_to_search(1). % Percentage of the space of object configurations to explore

noiseChancePercent(0). % Increase this to investigate the effects of simulated perceptual noise

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Other framework parameters %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

amountVarianceReductionRequired(1).

minimum_search_size(20).

sample_multiplier(100). % DEFAULT, canonical value experimentally derived

initial_learning_rate_parameter(0.1).

current_learning_rate_parameter(0.1).

explore_parameter(0.1). % Policy: 'explore' vs 'exploit'

reward_pos(10). % RL positive reward value
reward_neg(0.0). % RL negative reward value

record_full_data_traces(false). % Change this to 'true' to record all the data files (deprecated)

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This initialises some domain information %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(process_domains).


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
% leaf_stored_example(leaf_id, remainderdescriptionlist, count, sumOfQ, sumOfSquaredQ, configID).

% Configuration predicates:

% learned_config(SORTED_LIST_OF_ATTRS).
% num_possible_attribute_configs(n).
% total_config_count(n).
% percent_of_object_config_space_to_search(n).

/****************************************************************
 * Section 3: Running
 */

runbatch(CounterFile, ErrorFile, TimeFile) :-
	open('out.txt', write, OStream), % Default
	set_output(OStream),
	assert(finaloutputfile('default.txt')),
	assert(output_file_counter(CounterFile)),
	assert(output_file_error(ErrorFile)),
	assert(output_file_time(TimeFile)),
	enter_register(overall),
	go,
	exit_register(overall),
	close(OStream),
	printRegisters.

runbatchdefault :-
	runbatch('count.txt', 'error.txt', 'time.txt').

run(File) :-
	(suppress_text
	->
	(open('out.txt', write, OStream), % Default
	set_output(OStream))
	; true),
	assert(finaloutputfile(File)),
	enter_register(overall),
	go,
	exit_register(overall),
	(suppress_text
	->
	close(OStream)
	; true),
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
	setSpaceSize,
	% This register is important for batch tests - do not change.
	enter_register(b1),
	learnAllObjectConfigurations.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% x = max(numbasedonpercentspace, 50)
% y = min(numconfigs, x)
% explore y configs
setSpaceSize :-
	num_possible_attribute_configs(A),
	percent_of_object_config_space_to_search(P),
	minimum_search_size(Min),
	B is A-1, % Necessary
	N is (B*P)/100,
	(N > Min -> X = N ; X = Min),
	(B < X -> Y = B ; Y = X),
	assert(number_of_configs_to_search(Y)).

learnAllObjectConfigurations :-
	number_of_configs_to_search(N),
	total_config_count(C),
	C >= N,
	!,
	writefX('Necessary number of attribute configurations learned. Beginning generalisation step.\n'),

	% This register is important for batch tests - do not change.
	exit_register(b1),

	% This register is important for batch tests - do not change.
	enter_register(g1),
	(doGeneralisation -> true ; true),
	exit_register(g1),
	batchReportingTime.

learnAllObjectConfigurations :-
	Str = 'data-', get_time(TimeStamp), atom_concat(Str, TimeStamp, Stream), 
	retractall(data_output_file(_)),
	assert(data_output_file(Stream)),
	setRandomInitialObjectConfig,
	makePrincipledChangeToObjectConfig,
	doAllEpisodes,
	!,
	learnAllObjectConfigurations.
	
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
	N > 50,
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
	resetStateInPrincipledWay,
	
	enter_register(ep_steps),
	do_steps_in_episode, % Main function
	exit_register(ep_steps),
	
	enter_register(tree_split_checking),

	total_config_count(NConfig),
	number_of_configs_to_search(CTSe),
	Frac is NConfig/CTSe,
	(Frac >= 0.1 -> checkForTreeSplits ; true), % Don't start tree splitting immediately!
	
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
	number_of_configs_to_search(N),
	total_config_count(C),
	C >= N,
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
	setRandomInitialObjectConfig,
	randomWalkCurrentObjectConfig,
	exit_register(change_obj_att_configs),
	!,
	checkForFurtherChangeToConfig.

checkForFurtherChangeToConfig :-
	getCurrentRelevantConfigurationOfAttributes(X),
	learned_config(X),
	!,
	makePrincipledChangeToObjAttConfiguration.
checkForFurtherChangeToConfig :-
	getCurrentRelevantConfigurationOfAttributes(X),
	not(learned_config(X)),
	!.

% Catch case
randomWalkCurrentObjectConfig :-
	num_possible_attribute_configs(Limit),
	total_config_count(N),
	N >= Limit, % safety measure
	trace,
	print(266),nl,
	!.

randomWalkCurrentObjectConfig :-
	getCurrentRelevantConfigurationOfAttributes(List),
	domainChangeObjectAtts(List).
	
markCurrentConfigurationLearned :- 
	getCurrentRelevantConfigurationOfAttributes(List),
	assert(learned_config(List)).
	
q_values_have_converged :-
	episode_count(EC),
	last_split_at(Episode),
	Diff is EC - Episode,
	Diff > 50, % Splitting any node effectively resets the convergence counter
	findall( LeafID,
				(episode_high_val(EC, LeafID, HighVal),
				not(( episode_high_val(EC, _, HigherVal), HigherVal > HighVal ))),
				LeafIDs),
	!,
	check_last_X_differences(EC, LeafIDs, 50).

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
	( ((OlderVal == 0.0) ; (OlderVal == 0)) -> fail ; true),
	( ((NewerVal == 0.0) ; (NewerVal == 0)) -> fail ; true),
	Diff1 is (NewerVal/OlderVal),
	Diff2 is (OlderVal/NewerVal),
	Diff1 < 1.01, % Less than 1% change required, for 50 generations
	Diff2 < 1.01, % Less than 1% change required, for 50 generations
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
	(currentState(A) ; 
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
	
	enter_register('generalisation:1makeexamples'),
	
	findall([A1], leaf(A1), List0), length(List0,Size0),
	writefX('===> Number of leaves in the BDT: '), printX(Size0), nlnl,
		
	writefX('Extracting examples...'), nlnl,
	makeGeneralisedExamples,
	findall([ID, ConfigID, Literals, S, C], examplepathrecord(ID, ConfigID, Literals, S, C), List1), length(List1,Size1),
	writefX('===> Number of \'examplepathrecord\' records including duplicates; nonactions removed: '), printX(Size1), nlnl,
	
	exit_register('generalisation:1makeexamples'),
	enter_register('generalisation:2constructcandidates'),
	
	writefX('Constructing full set of generalised examples...'), nlnl,
	constructCandidateAxioms,
	exit_register('generalisation:2constructcandidates'),
	enter_register('generalisation:3findallsemi'),
	findall([CC,DD,EE,FF,GG,HH], semifinalexample(CC, DD, EE, FF, GG, HH), List2), length(List2,Size2),
	writefX('===> Number of \'semifinalexample\' records produced by finding subsets of each example: '), printX(Size2), nlnl,
		
	exit_register('generalisation:3findallsemi'),
	enter_register('generalisation:4maketree'),
	
	% Measure how many examples are strictly more specific than others, and report on that number.
	writefX('Making candidate tree\n'),
	makeCandidateTree,
	exit_register('generalisation:4maketree'),
	enter_register('generalisation:5findallcand'),
	findall([A2A,B2B,C2C,D2D,E2E,F2F,LL], candidate_axiom(A2A, B2B, C2C, D2D, E2E, F2F, LL), List3), length(List3,Size3),
	writefX('===> Number of candidate axioms: '), printX(Size3), nlnl,
	
	writefX('\nCandidate axioms, pre parsimony:\n'),
	
	exit_register('generalisation:5findallcand'),
	enter_register('generalisation:6adjust'),
	
	writefX('Extracting top candidate axioms. FINAL CHOICES...\n'),
	adjustAxiomsForParsimony,
	exit_register('generalisation:6adjust'),
	
	enter_register('generalisation:7sort'),
	writefX('Sorting top candidate axioms. FINAL CHOICES...\n'),
	sortFinalAxioms,
	
	exit_register('generalisation:7sort'),
	enter_register('generalisation:8final'),
	
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
	
	exit_register('generalisation:8final'),
	enter_register('generalisation:9lift'),
	writefX('Lifting final candidates\n'),
	liftCandidates,
	
	extractAndReportTopAxioms('final'),
	
	exit_register('generalisation:9lift'),
	
	!.

% First pass: Transform all leaves into "path records" of the tests from them to root. Keep only attributes and actions. Delete leaves in their wake.
makeGeneralisedExamples :-
	% 1. Pick any leaf L
	leaf(ID),
	not(leaf_generalised(ID,_)),
	% 2. Collect the partial state/action described by the path from L to root, looking only at internal nodes
	getStateActionInfoOnPath(ID, StateNeg, StatePos),
	makeExamplePathRecords(ID, StateNeg, StatePos),
	!,
	makeGeneralisedExamples.
makeGeneralisedExamples :-
	not( (leaf(ID), not(leaf_generalised(ID,_))) ),
	!.
	
makeExamplePathRecords(ID, StateNeg, StatePos) :-
	findall(X1, (member(X1, StatePos), X1 \= action(_) ), Pos1),
	findall(X2, (member(X2, StateNeg), X2 \= action(_) ), Neg1),
	
	sort(Pos1, Pos2),
	sort(Neg1, Neg2),
	Literals = [Pos2, Neg2],
	domainGoalAction(DGA),
	(
		% Case 1: a positive action is mentioned in StatePos and it matches the one we care about
		% Case 2: look for the action we care about specified within the actual examples at the leaf
		member(action(DGA), StatePos)
	->
		addMatchingSubsetAsPathRecord(ID, Literals)
	;
		addMatchingSubsetAsPathRecordConditionally(ID, Literals, DGA)
	).

% First case: There are fewer than two configurations explored for this leaf.
% Return unchanged, otherwise it will return ALL leaf data.
% That's problematic both from an efficiency standpoint, and for a principled reason -- the leaf's parent test was chosen 
% as something that divided it from others, so don't want to go and return something that wouldn't have divided it from others.
getCommonalitiesAtLeaf(ID, P, P) :-
	lessThanTwoConfigsAtLeaf(ID),
	!.
% Second case: Return all literals held in common between all configurations explored at leaf.
% These are more or less as important as the ones actually in the ancestral chain.
getCommonalitiesAtLeaf(ID, Pos1, Pos_New) :-
	findall(SomeLiteral,
		(
		leaf_stored_example(ID, LeafContent, _, _, _, _), member(SomeLiteral, LeafContent),
		not(( 
			leaf_stored_example(ID, LeafContent2, _, _, _, _), not(member(SomeLiteral, LeafContent2))
			))
		),
		PLits1),
	sort(PLits1,PLits),
	(PLits==[] -> Pos_New = Pos1 ; append(Pos1, PLits, Pos_New)).
	
lessThanTwoConfigsAtLeaf(ID) :-
	not((
		leaf_stored_example(ID, _, _, _, _, ConfigID1),
		leaf_stored_example(ID, _, _, _, _, ConfigID2),
		ConfigID1 \= ConfigID2
	)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Key here is to go through and do it separately for each different configuration ID

addMatchingSubsetAsPathRecord(ID, AttsAndFluentsPosAndNeg) :-
	leaf_stored_example(ID, _, _, _, _, ConfigID),
	not(leaf_generalised(ID,ConfigID)),
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, _, _, _, _, ConfigID), LeafExample), LeafExamples),
	addANewPathRecord(ID, ConfigID, AttsAndFluentsPosAndNeg, LeafExamples),
	assert(leaf_generalised(ID,ConfigID)),
	!,
	addMatchingSubsetAsPathRecord(ID, AttsAndFluentsPosAndNeg).
addMatchingSubsetAsPathRecord(_, _).

addMatchingSubsetAsPathRecordConditionally(ID, AttsAndFluentsPosAndNeg, DGA) :-
	leaf_stored_example(ID, _, _, _, _, ConfigID),
	not(leaf_generalised(ID,ConfigID)),
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, ExRemainder, _, _, _, ConfigID), LeafExample, member(action(DGA), ExRemainder)), LeafExamples),
	addANewPathRecord(ID, ConfigID, AttsAndFluentsPosAndNeg, LeafExamples),
	assert(leaf_generalised(ID,ConfigID)),
	!,
	addMatchingSubsetAsPathRecordConditionally(ID, AttsAndFluentsPosAndNeg, DGA).
addMatchingSubsetAsPathRecordConditionally(_, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
addANewPathRecord(_, _, _, []) :- !.
addANewPathRecord(_, _, [[],[]], _) :- !.
addANewPathRecord(ID, ConfigID, [AttsAndFluentsYes,AttsAndFluentsNo], LeafExamples) :-
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(0.0)),
	assert(countCollector(0)),
	amassPathRecord(ID, ConfigID, LeafExamples),
	sumQCollector(SumQ),
	countCollector(Count),
	assert(examplepathrecord(ID, ConfigID, [AttsAndFluentsYes, AttsAndFluentsNo], SumQ, Count)),
	!.
addANewPathRecord(ID, ConfigID, [AttsAndFluentsYes,AttsAndFluentsNo], LeafExamples) :- printX([ID, ConfigID, [AttsAndFluentsYes,AttsAndFluentsNo], LeafExamples]), trace.

amassPathRecord(ID, ConfigID, LeafExamples) :-
	member(leaf_stored_example(ID, _ExRemainder, InternalCount, InternalSum, _, ConfigID), LeafExamples),
	sumQCollector(SumQ), SumQNew is SumQ + (InternalSum / InternalCount),
	countCollector(Count), CountNew is Count + 1,
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(SumQNew)),
	assert(countCollector(CountNew)),
	fail.
amassPathRecord(_,_,_) :- !.

subsetp([], []).
subsetp([E|Tail], [E|NTail]):- subsetp(Tail, NTail).
subsetp([_|Tail], NTail):- subsetp(Tail, NTail).
  
% Second pass: Combine path records, transforming the generalised forms into 'semifinalexample', and delete path records
% Note that we go into this having constructed the examples and in doing so removed all the fluent / physical config information
% that would have been all that distinguished some of them. Hence we now expect to have duplicates in our semifinalexample([S1,S2], Q, Worst, Count) entries.
% Hence "training samples".
constructCandidateAxioms :-
	not(examplepathrecord(_, _, _, _, _)),
	!.

constructCandidateAxioms :-
	% 1. Pick any path record
	examplepathrecord(ID, ConfigID, [Pos,Neg], Sum, Count),
	% 2. Infer its candidate-subsets
	pos_or_neg_clause_limit_per_axiom(Lim),
	findall(SubPos, (subsetp(Pos, SubPos), length(SubPos, SP), SP =< Lim), ListOfPosSubsets),
	findall(SubNeg, (subsetp(Neg, SubNeg), length(SubNeg, SN), SN =< Lim), ListOfNegSubsets),
	findall([A,B], (member(A,ListOfPosSubsets), member(B,ListOfNegSubsets), [A,B] \== [[],[]]), List),
	!,
	% 3. For each, create or find the candidate and increment its two values appropriately
	updateCandidateAxioms(List, Sum, Count, ID, ConfigID),
	% 4. Delete the path record
	retractall(examplepathrecord(ID, ConfigID, [Pos,Neg], Sum, Count)),
	% 5. Repeat.
	!,
	constructCandidateAxioms.
	
% Done
updateCandidateAxioms([], _S, _C, _ID, _CID) :- !.

% Candidate exists
updateCandidateAxioms([A|B], Sum, Count, ID, ConfigID) :-
	%candidate exists,
	A = [PosList,NegList],
	semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList, OldConfigList),
	!,
	%update candidate,
	Q2 is QV + Sum,
	Count2 is OldCount + Count,
	TempMean is Sum/Count,
	((TempMean < Worst) -> Worst2 = TempMean ; Worst2 = Worst),
	(member(ID, OldIDList) -> NewIDList = OldIDList ; append(OldIDList, [ID], NewIDList)),
	(member(ConfigID, OldConfigList) -> NewConfigList = OldConfigList ; append(OldConfigList, [ConfigID], NewConfigList)),
	retractall(semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList, OldConfigList)),
	assert(semifinalexample([PosList,NegList], Q2, Worst2, Count2, NewIDList, NewConfigList)),
	% Recursive call
	updateCandidateAxioms(B, Sum, Count, ID, ConfigID).

% Make new candidate	
updateCandidateAxioms([A|B], Sum, Count, ID, ConfigID) :-
	A = [PosList,NegList],
	not(semifinalexample([PosList,NegList], _, _, _, _, _)),
	!,
	TempMean is Sum/Count,
	assert(semifinalexample([PosList,NegList], Sum, TempMean, Count, [ID], [ConfigID])),
	% Recursive call
	updateCandidateAxioms(B, Sum, Count, ID, ConfigID).

% Shouldn't be possible
updateCandidateAxioms(_, _, _, _, _) :- trace.

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
	findall([A,B,C,D,E], finalexample(A, B, C, D, E), List3), length(List3,Size3),
	writefX('===> Number of reduced \'finalexample\' records remaining: '), printX(Size3), nlnl,
	establishSupportForCandidates,
	writefX('===> Now supported with random examples.'), nlnl,
	finaliseCandidates.

reduceToHighQualityCandidates :-
	not(semifinalexample(_,_,_,_,_,_)),
	!.

reduceToHighQualityCandidates :-
	semifinalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingLeavesList, SupportingConfigsList),
	% Arithmetic mean
	TempMean is QValueSum/ExampleCount,
	retractall(semifinalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingLeavesList, SupportingConfigsList)),
	reward_pos(N),
	Min is N * 0.99, % Take only examples that were assigned Q on average not less than 1% below max_value
	( (TempMean > Min) 
		->
		assert(finalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList)) ; true),
	!,
	reduceToHighQualityCandidates.
	
%%%%%%%%%%%%%%%%%

% Next have to do an entire sweep through adding the values of random examples to the appropriate candidates

establishSupportForCandidates :-
	allValidTests(DomainTestsList),
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
	writefX('; unused attribute (or literal) tests: '), printX(N), 
	writefX('; average: '), printX(Average), nlnl,
	writefX('Number of random samples to draw: '), printX(Total), nlnl, 
	% Now actually do the support step
	!,
	findall(X, (X = leaf_stored_example(_, _, _, _, _, _CID), X), AllExamplesList),
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
	RandomExample = leaf_stored_example(LeafID, RemainderList, Count, SumOfQ, _SumOfSquaredQ, ConfigID),
	!,
	AddedValue is SumOfQ / Count,
	getStateActionInfoOnPath(LeafID, NegativeAncestors, PositiveAncestors),
	%
	append(RemainderList, PositiveAncestors, PositiveExampleLiterals1),
	NegativeExampleLiterals1 = NegativeAncestors,
	
	domainGoalAction(DGA),
	(not(member(action(DGA), PositiveExampleLiterals1))
	->
	( addToSampleCount(-1) )
	;
	(
		reduceToLits(PositiveExampleLiterals1, PositiveExampleLits),
		reduceToLits(NegativeExampleLiterals1, NegativeExampleLits),
		%
		findall(	Candidate,
					someSubsetMatchesSomeCandidate(PositiveExampleLits, NegativeExampleLits, Candidate),
					CandidatesToAdjust0
				),
		sort(CandidatesToAdjust0, CandidatesToAdjust), % Needed?
		addToAllCandidates(CandidatesToAdjust, AddedValue, ConfigID)
	)).

reduceToLits(List1, List2) :-
	findall(X, (member(X, List1), X \= action(_)), List2).

someSubsetMatchesSomeCandidate(PositiveExampleLits, NegativeExampleLits, Candidate) :-
	closed_world(false),
	!,
	% Each in the 'negative' section of the candidate axiom must also be in the 'negative' section of the example
	finalexample([AnyPositiveSubset,AnyNegativeSubset], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList),
	subsetp(PositiveExampleLits, AnyPositiveSubset),
	subsetp(NegativeExampleLits, AnyNegativeSubset),
	Candidate = finalexample([AnyPositiveSubset,AnyNegativeSubset], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList).
	
someSubsetMatchesSomeCandidate(PositiveExampleLits, _NegExampleLits, Candidate) :-
	closed_world(true),
	!,
	% It is enough that each in the 'negative' section of the candidate axiom is NOT in the positive section of the example
	% This is important because otherwise we can only find things that have appeared as negative tests
	Candidate = finalexample([AnyPositiveSubset,CandidateNegativeLiterals], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList),
	% It exists
	finalexample(            [AnyPositiveSubset,CandidateNegativeLiterals], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList),
	subsetp(PositiveExampleLits, AnyPositiveSubset),
	not( (member(N,CandidateNegativeLiterals), member(N,PositiveExampleLits) ) ).
	
addToAllCandidates([], _, _) :- !.
addToAllCandidates([A|B], AddedValue, ConfigID) :-
	A = finalexample(Content, QValueSum, WorstQValue, ExampleCount, OldConfigList),
	NewQ is QValueSum + AddedValue,
	((AddedValue < WorstQValue) -> NewWorst = AddedValue ; NewWorst = WorstQValue),
	NewCount is ExampleCount + 1,
	(member(ConfigID, OldConfigList) -> NewConfigList = OldConfigList ; append(OldConfigList, [ConfigID], NewConfigList)),
	retractall(finalexample(Content, QValueSum, WorstQValue, ExampleCount, OldConfigList)),
	assert(finalexample(Content, NewQ, NewWorst, NewCount, NewConfigList)),
	!,
	addToAllCandidates(B, AddedValue, ConfigID).

%%%%%%%%%%%%%%%%%

% Finally, remove cases that are still supported by only one example, or that aren't supported by multiple configurations.
finaliseCandidates :-
	retractall(finalexample(_, _, _, 1, _)), % One example
	retractall(finalexample(_, _, _, _, [_SingleElementList])), % Single config in support
	%%%%%retractall(finalexample(_, _, _, _, [_One, _Two])), % Two!
	retractall(finalexample(_, _, _, _, [])), % No support, somehow
	!,
	changeExamplesToCandidates.

changeExamplesToCandidates :-
	finalexample([Yes,No], Q, Worst, Count, SupportingConfigsList),
	retractall(finalexample([Yes,No], _, _, _, _)),
	domainGoalAction(Act),
	length(SupportingConfigsList, L),
	assert(candidate_axiom(raw, not_occurs(Act), [Yes,No], Q, Worst, Count, L)),
	!,
	% 3. Recursive call
	changeExamplesToCandidates.
changeExamplesToCandidates.


%%%%%%%%%%%%%%%%%


liftCandidates :-
	final_axiom(Rank, not_occurs(Act), [Yes,No], AdjustedMean, Worst, Count, Configs),
	nonvar([Yes,No]),
	!,
	retractall(final_axiom(Rank, not_occurs(Act), [Yes,No], AdjustedMean, Worst, Count, Configs)),
	% 1. Get Arity = arity of Act
	functor(Act, _Name, Arity),
	% 2. For each of Arity in turn, replace in Act and throughout Yes and No with members of list
	% [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]
	AlphabetVariableList = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z],
	replaceByArity(Arity, Act, Yes, No, AlphabetVariableList),
	assert(final_axiom(Rank, fails(Act), [Yes,No], AdjustedMean, Worst, Count, Configs)),
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
	not(candidate_axiom(raw,_,[_,_],_,_,_,_)),
	!.
adjustAxiomsForParsimony :-
	candidate_axiom(raw,Act,[Yes,No],Q,Worst,Count,Configs),
	Mean is Q/Count,
	retractall(candidate_axiom(raw,Act,[Yes,No],Q,Worst,Count,Configs)),
	assert(candidate_axiom(Mean,Act,[Yes,No],Q,Worst,Count,Configs)),
	adjustAxiomsForParsimony.
	
	
sortFinalAxioms :-
	% get best value, pass it in
	enter_register('generalisation:7sorta'),
	reward_pos(N),
	Min is N * 0.99,
	exit_register('generalisation:7sorta'),
	!,
	enter_register('generalisation:7sortb'),
	
	rankFinalAxioms(Min, 1),
	
	exit_register('generalisation:7sortb').

rankFinalAxioms(Min, _) :-
	not((
		candidate_axiom(Mean,_,[_,_],_,_,_,_),
		Mean > Min
	)),
	!.
rankFinalAxioms(Min, Rank) :-
	enter_register('generalisation:7sortb1'),
	candidate_axiom(Mean,Act,[Yes,No],Q,Worst,Count,Configs), % Expensive
	Mean > Min,
	
	% 1. better than all other zero-ranks (i.e. unranked)
	enter_register('generalisation:7sortb2'),
	not( (candidate_axiom(M,_,_,_Q2,_,Count2,_), M > Min, Count2 > Count) ),
	exit_register('generalisation:7sortb2'),
	
	exit_register('generalisation:7sortb1'),
	!,
	% 2. set it to the passed-in rank and increase rank
		
	%((final_axiom(Number,_,_,_,_,_), not( (final_axiom(Number2,_,_,_,_,_), Number2 > Number) ), NewRank is Number + 1)
	%;
	%NewRank = 1),
	
	% Basically, change the rank each time, starting from best (highest mean), i.e., smallest number rank
	
	enter_register('generalisation:7sortb3'),
	retract(candidate_axiom(Mean,Act,[Yes,No],Q,Worst,Count,Configs)),
	asserta(final_axiom(Rank,Act,[Yes,No],Mean,Worst,Count,Configs)),
	NewRank is Rank+1,
	exit_register('generalisation:7sortb3'),
	!,
	rankFinalAxioms(Min, NewRank).

	
	
% % % % % % % % % %

finalAxiomQualityImprovement :-
	checkCandidateAxiom(1),
	reRankAxioms(0).

% 1. Run out of axioms to check.
checkCandidateAxiom(N) :-
	not( final_axiom(N,_Act,_,_Mean,_Worst,_Count,_Configs) ),
	!.
% 2. N is a more specific form of an already-confirmed axiom.
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_,_),
	M < N,
	subset(Yes2, Yes1), % Yes2 is subset of Yes1
	subset(No2, No1),
	!,
	retractall(final_axiom(N,_,[Yes1,No1],_,_,_,_)),
	Next is N+1,
	checkCandidateAxiom(Next).
% 3. N is a more general form of an already-confirmed axiom. (unlikely, but they could have same number, so technically possible)
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_,_),
	M < N,
	subset(Yes1, Yes2), % Yes1 is subset of Yes2
	subset(No1, No2),
	!,
	retractall(final_axiom(M,_,[Yes2,No2],_,_,_,_)),
	checkCandidateAxiom(N).
% 4. Base case
checkCandidateAxiom(N) :-
	!,
	Next is N+1,
	checkCandidateAxiom(Next).
	
reRankAxioms(CurrentLargestRank) :-
	% 1. Find the candidate with the rank > CurrentLargestRank but < all other extant ranks
	final_axiom(N,Act,B,Mean,Worst,C,Configs),
	N > CurrentLargestRank,
	not(( 
		final_axiom(M,_,_,_,_,_,_),
		M > CurrentLargestRank,
		M < N
	)),
	% 2. Retract it and set it to rank CurrentLargestRank+1
	retract(final_axiom(N,Act,B,Mean,Worst,C,Configs)),
	NewRank is CurrentLargestRank+1,
	% Discard any over the top ten!
	((NewRank < 11) -> assert(final_axiom(NewRank,Act,B,Mean,Worst,C,Configs)) ; true),
	% 3. Recurse
	!,
	reRankAxioms(NewRank).
% Base case
reRankAxioms(_) :- !.

removeArtificialNoiseGeneratorForChecking :-
	retract(noiseChancePercent(_)),
	assert(noiseChancePercent(0)).
	
checkTopCandidatesWithOracle :-
	% For each remaining candidate C in turn:
	% 1. Start with domain-defined physical state
	% 2. Test random obj att configurations until you get an att config S where the action succeeds
	% 3. Now:
	%		(a) Modulate S to S' by the minimum necessary changes such that the partial configuration described by C holds in S'
	% 		(b) Poll the oracle as to whether the action still succeeds in S'
	% 		(c) If it does, discard the candidate axiom C
	
	removeArtificialNoiseGeneratorForChecking,
	
	batchReportingOutputs(0),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(1),
	
	batchReportingOutputs(1),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(2),
	
	batchReportingOutputs(2),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	extractAndReportTopAxioms(3),
	
	batchReportingOutputs(3),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(4),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(5),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(6),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(7),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(8),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(9),
	
	oracleFilterCandidates(1),
	reRankAxioms(0),
	
	batchReportingOutputs(10),
	
	extractAndReportTopAxioms(10).
	
	
oracleFilterCandidates(N) :-
	not(final_axiom(N,_Act,_B,_Mean,_Worst,_C,_Configs)),
	!.
oracleFilterCandidates(N) :-
	writefX('Oracle filter:\n  Checking candidate #'), printX(N), nlnl,
	!,
	final_axiom(N,Act,[True,False],Mean,Worst,C,Configs),
	!,
	setAndTryVirtuousState(True,False),	
	(not(actionFailsInDomain)
		->
		(writefX('  Pruning candidate #'), printX(N), nlnl,
		retractall(final_axiom(N,Act,[True,False],Mean,Worst,C,Configs)))
		;
		true
	),
	M is N +1,
	!,
	oracleFilterCandidates(M).
	
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

	
% Each time this is called, it repeatedly
% 1. Generates a random new state until it finds one where the target action succeeds
% 2. Makes the necessary swapins/swapouts to make it fulfil the learned axiom
% 3. Checks for consistency of the new physical state; returns if consistent, else starts over
setAndTryVirtuousState(True,False) :-
	resetStateAtRandom,
	setRandomInitialObjectConfig,
	storeCurrentStateAndConfig(List),
	!,
	(actionFailsInDomain -> setAndTryVirtuousState(True,False) ; 
		(
		% Action succeeds under these circumstances
		restoreStateAndConfig(List),
		adjustVirtuousState(True,False),
		!,
		% If the adjusted state violates state constraints, start over. Else done.
		(physicalConstraintsViolated -> setAndTryVirtuousState(True,False) ; true)
		)
	).

adjustVirtuousState(True,False) :-
	not( (member(F,False), currentState(F)) ),
	not( (member(T,True), not(currentState(T))) ),
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

swapOut(Literal) :-
	not( currentState(Literal) ),
	!.
swapOut(Literal) :-
	currentState(Literal),
	retractall(currentState(Literal)),
	domain_test_alternatives(Literal, AltList),
	random_member(Alt, AltList),
	assert(currentState(Alt)).
swapIn(Literal) :-
	currentState(Literal),
	!.
swapIn(Literal) :-
	domain_test_alternatives(Literal, AltList),
	deleteAllLits(AltList),
	assert(currentState(Literal)).
	
deleteAllLits([]) :- !.
deleteAllLits([A|B]) :-
	retractall(currentState(A)),
	deleteAllLits(B).


% % % % %

batchReportingOutputs(Index) :-
	% If the batch support files exist (have been asserted), then output to them appropriately.
	output_file_counter(CounterFile),
	output_file_error(ErrorFile),
	!,
	open(CounterFile,append,CounterStream),
	open(ErrorFile,append,ErrorStream),
	reportTrueAndFalsePositives(CounterStream,ErrorStream,Index),
	close(CounterStream),
	close(ErrorStream),
	!.
% If no batch reporting started, do nothing.
batchReportingOutputs(_).

batchReportingTime :-
	% If the batch support files exist (have been asserted), then output to them appropriately.
	output_file_time(TFile),
	% Only if registers are being kept in the first place
	registerson(true),
	!,
	open(TFile,append,TimeStream),
	key_registers_report(TimeStream),
	close(TimeStream),
	!.
% If no batch reporting started, do nothing.
batchReportingTime.

key_registers_report(Stream) :-
	register(b1, Stored1, -1), % BDT building
	write(Stream, '\n(1) '),
	write(Stream, Stored1),
	register(g1, Stored3, -1), % Generalisation
	write(Stream, '\n(3) '),
	write(Stream, Stored3),
	!.
key_registers_report(_) :- trace. % Should not be reachable

reportTrueAndFalsePositives(CounterStream, ErrorStream, Index) :-
	% For each element in list, returns a number associated with the target affordance, or returns it back for "does not match any target affordance"
	findall(Identifier,
		(final_axiom(_R, _A, [YesLiterals,NoLiterals], _QM, _W, _C, _Configs),
		domainAffordanceClassifier([YesLiterals,NoLiterals], Identifier)),
		ReturnList),
	% Number of unique numeric responses is the true positives
	sort(ReturnList, Set),
	findall(El, (member(El, Set), number(El)), NewSet),
	length(NewSet, TPs),
	% Other responses is the false positives
	findall(El, (member(El, Set), not(number(El))), Unrecognised),
	FPs = Unrecognised,
	% Print them to the batch collater
	% Only bother with TPs before the oracle filter
	(Index == 0 -> reportTP(TPs, CounterStream) ; true),
	reportFalsePositives(FPs, ErrorStream, Index).
	
% If no false positives, store nothing. Else, store each on a new line.
reportFalsePositives([], _, _) :- !.
reportFalsePositives([A|B], ErrorStream, Index) :-
	reportFP(A, ErrorStream, Index),
	reportFalsePositives(B, ErrorStream, Index).
	
reportFP(Term, Stream, Prefix) :-
	write(Stream, '[filter '),
	write(Stream, Prefix),
	write(Stream, '] '),
	write(Stream, Term),
	write(Stream, '\n').
reportTP(Term, Stream) :-
	write(Stream, Term),
	write(Stream, '\n').
	

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
	not(final_axiom(Rank, _Act, _YN, _QMean, _Worst, _Count, _Configs)),
	!.
printAxiomListRecursive(Rank, true) :-
	final_axiom(Rank, Act, [Yes,No], QMean, Worst, Count, Configs),
	print(Rank), writef('. '),
	((Rank < 10) -> writef(' ') ; true), % Zero pad
	printAxiomNicelyWithoutNewline(Act, Yes, No, true),
	writef('  [mean '), print(QMean), writef('], [worst '), print(Worst), writef('], [support: '), print(Count), writef(' examples, '), print(Configs), writef(' configs]'), nl,
	NewRank is Rank + 1,
	printAxiomListRecursive(NewRank, true).
printAxiomListRecursive(Rank, false) :-
	final_axiom(Rank, Act, [Yes,No], QMean, Worst, Count, Configs),
	printX(Rank), writefX('. '),
	((Rank < 10) -> writefX(' ') ; true), % Zero pad
	printAxiomNicelyWithoutNewline(Act, Yes, No, false),
	writefX('  [mean '), printX(QMean), writefX('], [worst '), printX(Worst), writefX('], [support: '), printX(Count), writefX(' examples, '), printX(Configs), writefX(' configs]'), nlnl,
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
	allValidTests(AVT),
	findall(N, (currentState(N), member(N, AVT)), StateListUnsorted),
	
	exit_register('ep_steps:main1:a'),
	enter_register('ep_steps:main1:b'),
	sort(StateListUnsorted, StateList),
	exit_register('ep_steps:main1:b'),
	
	exit_register('ep_steps:main1'),
		
	printlnd('Getting valid actions'),
	enter_register('ep_steps:main2'),
	usableActionList(RelActions),
	findall(X, (validAction(X), member(action(X), RelActions)), ActionListX), % Actions that are valid in the current state
	exit_register('ep_steps:main2'),
	(ActionListX==[] -> 
	ActionList = [donothing]
	;
	ActionList = ActionListX
	),
	
	printlnd('Selecting an action...'),
	explore_parameter(Exp),
	random(F),
	
	(
		(F < Exp)
	->
		% Explore
		(printlnd('Getting random valid action'),
		random_member(Action, ActionList)
		)
	;
		% Policy
		(printlnd('Getting best valid action based on Q estimates'),
		
		enter_register('ep_steps:getBestByBDT'),
		pickBestByBinaryTreeEstimate(ActionList, Action),
		exit_register('ep_steps:getBestByBDT')
		)
		
	),
	
	printd('Applying action:  '),
	printlnd(Action),
	
	enter_register('ep_steps:applyAction'),
	applyActionAndReportOracleOutcome(Action, RewardValue),
	exit_register('ep_steps:applyAction'),
	
	/*
	Currently: The example is only used in the tree (i.e., trickled down) if it is actually relevant.
	Non-relevant actions can still be selected for trial, e.g. via the random policy.
	But examples constructed with non-relevant actions are not useful for the end goal, creating new domain axioms.
	*/
	
	%IF
	
	(relevantTestLitAction(action(Action))
	->
	
	%THEN
	
	(
		% We are now in new state. Find max stored q-value from possible actions from this new state
		enter_register('ep_steps:findall1'),
		findall(Y, validAction(Y), ActionList2),
		exit_register('ep_steps:findall1'),
	
		enter_register('ep_steps:getBestByBDT'),
		getHighestQValueForAnyActionFromCurrentState(ActionList2, FutureValue),
		exit_register('ep_steps:getBestByBDT'),
	
		% Note you are passing in 'StateList', i.e., a record of the state that the system was in 
		% when entering this step, prior to applying the action.
		printlnd('Updating Q-value'),
		append(StateList, [action(Action)], StateDescriptionWithAction),
	
		enter_register('ep_steps:trickleQVDown'),
		trickleNewQValueExampleDownTree(StateDescriptionWithAction, RewardValue, FutureValue),
		exit_register('ep_steps:trickleQVDown')
	)

	%ELSE do nothing
	
	; true),
	
	%CONTINUE
	
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
	(episode_count(EpCount)  -> 
		(retractall(last_split_at(_)),
		assert(last_split_at(EpCount)))
		;
		true),
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

	current_learning_rate_parameter(Gamma),
	_ExampleQValue is RewardValue + (Gamma * (FutureValue)),
	
	sort(StateDescriptionWithAction, Remainder),
	total_config_count(ConfigID),
	updateExamplesStoredAtLeaf(Node, Remainder, RewardValue, ConfigID),
	
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
updateExamplesStoredAtLeaf(LeafID, RemainderListWithAction, QV, ConfigID) :-
% Case 1: example already exists, stored at leaf
	leaf_stored_example(LeafID, RemainderListWithAction, Count, SumOfQ, SumOfSquaredQ, ConfigID),
	!,
	Count2 is Count + 1,
	SumOfQ2 is SumOfQ + QV,
	SumOfSquaredQ2 is SumOfSquaredQ + (QV * QV),
	retractall(leaf_stored_example(LeafID, RemainderListWithAction, Count, SumOfQ, SumOfSquaredQ, ConfigID)),
	assert(leaf_stored_example(LeafID, RemainderListWithAction, Count2, SumOfQ2, SumOfSquaredQ2, ConfigID)),
	!.
updateExamplesStoredAtLeaf(LeafID, RemainderListWithAction, QV, ConfigID) :-
% Case 2: create new example stored at leaf
	Sq is QV * QV,
	assert(leaf_stored_example(LeafID, RemainderListWithAction, 1, QV, Sq, ConfigID)),
	!.

initialise_children_stats_and_q(NodeID, NodeTest) :-
	child_y(NodeID, ChildYes),
	child_n(NodeID,  ChildNo),
	% Originally set predicted Q-value at a new leaf as the mean of those of the relevant examples.
	reward_neg(Neg),
	% 1. Look at examples responding 'yes' to new test, i.e., it's in their remainder. Count and take Q-values.
	findall( QV,
			(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs), member(NodeTest, ExampleContent), QV is SumOfQ/Count),
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
			(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs), not(member(NodeTest, ExampleContent)), QV is SumOfQ/Count),
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
	leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs),
	member(NodeTest, ExampleContent),
	!,
	retractall(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs)),
	select(NodeTest, ExampleContent, Remainder),
	asserta(leaf_stored_example(ChildYes, Remainder, Count, SumOfQ, SumOfSquaredQ, Confs)),
	!,
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest).
moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest) :-
	leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs),
	not(member(NodeTest, ExampleContent)),
	!,
	retractall(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs)),
	asserta(leaf_stored_example(ChildNo, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs)),
	!,
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest).
moveExamplesDown(_NodeID, _ChildYes, _ChildNo, _NodeTest) :- !.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


calculateVarianceAtLeafNode(ID, Variance) :-
	enter_register('calculate_variance_at_leaf'),
	enter_register('calculate_variance_at_leaf:sums'),
	getVarianceComponents(ID, Count, SumQ, SumSquareQ),
	!,
	exit_register('calculate_variance_at_leaf:sums'),
	enter_register('calculate_variance_at_leaf:find'),
	findVariance(Count, SumQ, SumSquareQ, Variance),
	exit_register('calculate_variance_at_leaf:find'),
	exit_register('calculate_variance_at_leaf').

adjustEx(A, B, C) :-
	c1(X), New1 is X+A,
	c2(Y), New2 is Y+B,
	c3(Z), New3 is Z+C,
	retract(c1(_)), retract(c2(_)), retract(c3(_)),
	assert(c1(New1)), assert(c2(New2)), assert(c3(New3)).

getVarianceComponents(ID, CountTotal, SumQTotal, SumSquareQTotal) :-
	assert(c1(0)), assert(c2(0)), assert(c3(0)),
	forall(leaf_stored_example(ID, _, Coun, SumQ, SSqQ, _Confs), adjustEx(Coun, SumQ, SSqQ)),
	c1(CountTotal), c2(SumQTotal), c3(SumSquareQTotal),
	retract(c1(_)), retract(c2(_)), retract(c3(_)).

	
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
				(leaf_stored_example(LeafID, ContentYes, CountYes, QYes, QSqYes, _ConfsYes), member(ProspectiveTest,ContentYes)),
				ListYes ),
	findall(	[CountNo, QNo, QSqNo],
				(leaf_stored_example(LeafID, ContentNo, CountNo, QNo, QSqNo, _ConfsNo), not(member(ProspectiveTest,ContentNo))),
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
	assert(predicted_q_value(ID, 0, 0)),
	domainGoalAction(DGA),
	splitNode(ID, action(DGA)).
	
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
% (DEPRECATED)
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
	
% Note there's, necessarily, no way to end up with two actions in the same path.
	
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
	checkListForTreeSplits_inner([LeafID|Tail], TotalVariance).

% Final clause - catch case - not enough variance improvement to split at all
checkListForTreeSplits([_LeafID|Tail]) :-
	!,
	enter_register('tree_split_checking:clause3'),
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	exit_register('tree_split_checking:clause3'),
	checkListForTreeSplits(Tail).

% % % % % %

checkListForTreeSplits_inner([_LeafID|Tail], TotalVariance) :-
	TotalVariance =< 0.0,
	!,
	enter_register('tree_split_checking:clause1'),
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	exit_register('tree_split_checking:clause1'),
	checkListForTreeSplits(Tail).

checkListForTreeSplits_inner([LeafID|Tail], TotalVariance) :-
	enter_register('tree_split_checking:clause2'),
	
	findall(	Literal,
				(leaf_stored_example(LeafID, Content, _, _, _, _C), member(Literal, Content)),
				List1),
	sort(List1, List2),
		
	findall(	[ProspectiveTest, MeanReduction, YesReduction, NoReduction],
				(
					member(ProspectiveTest, List2),
					calculateYesNoVariancesForTest(LeafID, ProspectiveTest, VarianceYes, VarianceNo),
					% We already know TotalVariance > 0 otherwise the previous clause would have kicked in
					YesReduction is TotalVariance - VarianceYes,
					NoReduction is TotalVariance - VarianceNo,
					MeanReduction is (YesReduction + NoReduction)/2
				),
				ListOfTestPossibilities
				),
	% First, pick the lowest-variance test where there is some sufficient reduction in variance
	% Have to use findall, otherwise it will just pick the first (alphabetically)
	
	amountVarianceReductionRequired(AVRR),
	
	findall( Test,
		(
		member(SelectableTest, ListOfTestPossibilities),
		SelectableTest = [Test, MeanReduction, YesReduction, NoReduction],
		once((YesReduction > 0 ; NoReduction  > 0)),
		MeanReduction  > AVRR, % 1 was a reasonable value for Blocks World
		not( (member(OtherTest, ListOfTestPossibilities), OtherTest \= SelectableTest, OtherTest = [_, OtherMean, _, _], OtherMean > MeanReduction) )
		),
			AllSelectableTests),
	
	random_member(ChosenTest, AllSelectableTests),
	
	splitNode(LeafID, ChosenTest),
	exit_register('tree_split_checking:clause2'),
	!,
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

% Deprecated... shouldn't ever be called successfully
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

printlnd(X) :-
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
	writefX('], configs '),
	
	findall(CID, leaf_stored_example(ID, _, _, _, _, CID), CIDLIST),
	sort(CIDLIST, Configs),
	printX(Configs),
		
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
	leaf_stored_example(ID, Remainder, ExCount, _SumOfQ, _SumOfSquaredQ, Confs),
	writefX('{'),
	printX(Remainder),
	writefX('} : '),
	printX(ExCount),
	writefX(' examples. '),
	writefX('<'),
	printX(Confs),
	writefX('>\n'),
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

