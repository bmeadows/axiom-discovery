:- dynamic filter/3.


filter(0,0,0).
filter(1,0,0).
filter(2,0,0).
filter(3,0,0).
filter(4,0,0).
filter(5,0,0).
filter(6,0,0).
filter(7,0,0).
filter(8,0,0).
filter(9,0,0).
filter(10,0,0).


printResult :-
	open('noise analysis.txt', write, Str),
	printEach(0,11,Str).

printEach(Current,Current,_) :- !.
printEach(Current,Finish,Str) :-
	filter(Current,A,B),
	printRes(Str,Current,A,B),
	New is Current + 1,
	printEach(New,Finish,Str).

printRes(Stream,N,A,B) :-
	write(Stream, N), write(Stream, ': '),
	write(Stream, A), write(Stream, ' '),
	write(Stream, B), write(Stream, ' '),
	write(Stream, '\n').
	
	
go :-
	open('error.txt', read, Str),
	read_all(Str).

read_all(File) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
    %split_string(Codes, " ", "", SubStringList),
	atom_string(ATOM, Codes),
	atomic_list_concat(SubStringList, ' ', ATOM),
	SubStringList = [_FilterString,NumberString,ActualString],
	(NumberString = '0]' -> something(0,ActualString) ; true),
	(NumberString = '1]' -> something(1,ActualString) ; true),
	(NumberString = '2]' -> something(2,ActualString) ; true),
	(NumberString = '3]' -> something(3,ActualString) ; true),
	(NumberString = '4]' -> something(4,ActualString) ; true),
	(NumberString = '5]' -> something(5,ActualString) ; true),
	(NumberString = '6]' -> something(6,ActualString) ; true),
	(NumberString = '7]' -> something(7,ActualString) ; true),
	(NumberString = '8]' -> something(8,ActualString) ; true),
	(NumberString = '9]' -> something(9,ActualString) ; true),
	(NumberString = '10]' -> something(10,ActualString) ; true),
	!,
	read_all(File).
read_all(_) :- printResult.

something(Index, Atom) :-
	atom_to_term(Atom, [Yes,No], _),
	filter(Index,CurrentYES,CurrentNO),
	sort(Yes,Y1),
	sort(No,N1),
	(
	(matchesSome(Y1,N1))
	->
	(NewYes is CurrentYES +1, NewNo is CurrentNO)
	;
	(NewYes is CurrentYES, NewNo is CurrentNO +1)
	),
	retractall(filter(Index,CurrentYES,CurrentNO)),
	asserta(filter(Index,NewYes,NewNo)).
	
matchesSome(Y1,N1) :-
	domainAffordanceClassifier([YesSubset, NoSubset]),
	subset(YesSubset,Y1),
	subset(NoSubset,N1).

	
	

/*   FILTER:  <Specifications>  <Other errors>     */


% 1. Damaged object
domainAffordanceClassifier([ [], [attr(obj_status(cup1, intact))] ]).
domainAffordanceClassifier([ [attr(obj_status(cup1, damaged))], [] ]).
% 2. Engineer/workshop
domainAffordanceClassifier([ [attr(role_type(p1, engineer)), fluent(loc(rob1, workshop))], [] ]).
domainAffordanceClassifier([ [attr(role_type(p1, engineer)), fluent(loc(p1, workshop))], [] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(p1,library)),fluent(loc(p1,office))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,library)),fluent(loc(p1,office)),fluent(loc(rob1,kitchen))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(p1,office)),fluent(loc(rob1,library))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(p1,library)),fluent(loc(rob1,office))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,office)),fluent(loc(rob1,kitchen)),fluent(loc(rob1,library))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,library)),fluent(loc(rob1,kitchen)),fluent(loc(rob1,office))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(p1,kitchen)),fluent(loc(rob1,library)),fluent(loc(rob1,office))] ]).
domainAffordanceClassifier([ [attr(role_type(p1,engineer))], [fluent(loc(rob1,kitchen)),fluent(loc(rob1,library)),fluent(loc(rob1,office))] ]).
domainAffordanceClassifier([ [fluent(loc(p1,workshop))], [attr(role_type(p1,manager)),attr(role_type(p1,sales))] ]).
domainAffordanceClassifier([ [fluent(loc(rob1,workshop))], [attr(role_type(p1,manager)),attr(role_type(p1,sales))] ]).



:- go.
