/*
Alexander's fuzzy example - a simple combat scenario
Ref Thor Alexander (Hard Coded Games)
AI Game Programming Wisdom, Steve Rabin (Ed),
Charles River Media, 2002, pp367-374

This is based on Alexander's example. He does not provide
any code & certainly doesn't use a fuzzy associative matrix.
However, he does mention rules that correlate with this.

The fuzzy variables & fuzzy sets (LPA qualifiers) within them
are given, as are the characteristic functions. They can be
compared & verified by running LPA's Fuzzy Editor. They can
also be experimented with using this excellent tool
*/

%FE distance created at 12:55:56 on 20/3/2010
fuzzy_variable(distance) :- 
   [0,100] ; 
   at,                                              \,  linear, [0,15] ; 
   very_close,                                      /\, linear, [10,24,35] ; 
   close,                                           /\, linear, [30,49,70] ; 
   pretty_far,                                      /\, linear, [60,75,90] ; 
   far,                                             /,  linear, [85,100] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

%FE hate created at 12:58:22 on 20/3/2010
fuzzy_variable(hate) :- 
   [0,100] ; 
   not_hated,                                       \,  linear, [0,20] ; 
   somewhat_hated,                                  /\, linear, [20,36,50] ; 
   hated,                                           /\, linear, [40,59,80] ; 
   very_hated,                                      /,  linear, [75,100] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

% Fuzzy output variable - needs to be processed afterwards
% FE behaviour edited at 11:42:11 on 22/3/2011
fuzzy_variable(behaviour) :- 
   [0,100] ; 
   do_nothing,                                      \,  linear, [11,27] ; 
   melee,                                           /\, linear, [11,27,43] ; 
   crossbow,                                        /\, linear, [37,50,65] ; 
   fireball,                                        /,  linear, [61,80] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

fuzzy_matrix(combat_behaviour):-
	hate			*		distance	-> behaviour	;
	not_hated		*		at		-> do_nothing	;
	not_hated		*		very_close	-> do_nothing	;
	not_hated		*		close		-> do_nothing	;
	not_hated		*		pretty_far	-> do_nothing	;
	not_hated		*		very_far	-> do_nothing	;
	somewhat_hated	*		at		-> melee		;
	somewhat_hated	*		very_close	-> melee		;
	somewhat_hated	*		close		-> crossbow		;
	somewhat_hated	*		pretty_far	-> do_nothing	;
	somewhat_hated	*		very_far	-> do_nothing	;
	hated			*		at		-> melee		;
	hated			*		very_close	-> melee		;
	hated			*		close		-> crossbow		;
	hated			*		pretty_far	-> crossbow		;
	hated			*		very_far	-> fireball		;
	very_hated		*		at		-> melee		;
	very_hated		*		very_close	-> melee		;
	very_hated		*		close		-> crossbow		;
	very_hated		*		pretty_far	-> fireball		;
	very_hated		*		very_far	-> fireball		.

combat(Distance,Hate):-
	uncertainty_trace,
	fuzzy_reset_membership,
	fuzzy_variable_value(distance, Distance),
	fuzzy_variable_value(hate, Hate),
	fuzzy_propagate(combat_behaviour),
	fuzzy_variable_value(behaviour, Output1),
	nl, write('the fuzzy output is: ' - Output1),nl.
