
registerson(true).

enter_register(FunctionName) :-
  statistics(process_cputime, Current),
  not(register(FunctionName, _, _)),
  assert(register(FunctionName, 0, Current)),
  !.

enter_register(FunctionName) :-
  statistics(process_cputime, Current),
  register(FunctionName, TotalStored, X),
  retract(register(FunctionName, TotalStored, X)),
  assert(register(FunctionName, TotalStored, Current)).

exit_register(FunctionName) :-
  statistics(process_cputime, Current),
  register(FunctionName, TotalStored, Start),
  Diff is Current - Start,
  NewStored is TotalStored + Diff,
  retract(register(FunctionName, TotalStored, Start)),
  assert(register(FunctionName, NewStored, -1)).

printRegisters :-
  format('~t ~w ~t~70|~n', ['Time reporting (registers):']),
  reportOnRegisters.

reportOnRegisters :-
	not(register(_, _, _)),
	!.
reportOnRegisters :-
  register(FName, Stored, _),
  retractall(register(FName, Stored, _)),
  format('~w ~t~40|: ~5f CPU seconds ~n', [FName, Stored]),
  !,
  reportOnRegisters.
