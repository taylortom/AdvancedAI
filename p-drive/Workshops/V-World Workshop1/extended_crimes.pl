/* an extended crime-buster program
*/

possible_suspect(fred).
possible_suspect(mary).
possible_suspect(jane).
possible_suspect(george).

crime(robbery, john, tuesday_night, park).
crime(assault, robin, thursday, pub).
crime(robbery, jim, wednesday, pub).

was_seen(fred, tuesday_night, park).
was_seen(jane, thursday, pub).
admitted_was_at(john, thursday, pub).	
										
jealous_of(fred, john).
owes_money_to(bill,mary).

owes_money_to(jane, robin).
gambler(mary).
drinker(george).

prime_suspect(Person, Crime):-
	crime(Crime, Victim, Time, Place),
	possible_suspect(Person),
	was_at(Person, Time, Place),
	had_motive_against(Person, Victim).

prime_suspect(Person, Crime):-
	Crime=robbery,
	has_debt(Person).

prime_suspect(Person, Crime):-
	Crime=assault,
	violent(Person).

prime_suspect(unknown, Crime).

had_motive_against(Person, Victim):-
	jealous_of(Person, Victim);
	owes_money_to(Person, Victim).

was_at(Person, Time, Place):-	
	was_seen(Person, Time, Place);
	admitted_was_at(Person, Time, Place).

has_debt(Person):-
	gambler(Person).

violent(Person):-
	drinker(Person).
