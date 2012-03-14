/* a simple program in Prolog. A crime mystery.

Note:  this is an extended version of the crimes scenariuo
you might have seen in CI213. It is here as a pretty standard
example of clear logical representation and reasoning. It can,
& should, be traced to see how the following works:

Unification
Inference
Backtracking

Notice also this form of commenting out 
code. It works for multiple lines 
*/

possible_suspect(fred).				% start of domain facts
possible_suspect(mary).
possible_suspect(jane).
possible_suspect(george).
owes_money_to(bill,mary).

crime(robbery, john, tuesday_night, park).
crime(assault, robin, thursday, pub).
crime(robbery, jim, wednesday, pub).

was_seen(fred, tuesday_night, park).
was_seen(jane, thursday, pub).
admitted_was_at(john, thursday, pub).
owes_money_to(jane,robin).

jealous_of(fred, john).				% end of domain facts

prime_suspect(Person, Crime):-			% start of domain rules (axioms)
	crime(Crime, Victim, Time, Place),
	possible_suspect(Person),
	was_at(Person, Time, Place),
	had_motive_against(Person, Victim).
	
prime_suspect(unknown, Crime).

had_motive_against(Person, Victim):-
	jealous_of(Person, Victim);
	owes_money_to(Person, Victim).

was_at(Person, Time, Place):-
	was_seen(Person, Time, Place).

was_at(Person, Time, Place):-
	admitted_was_at(Person, Time, Place).	% end of domain rules
