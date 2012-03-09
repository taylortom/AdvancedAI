/* (Possible) fuzzy system for (part of) V-World
G. Winstanley, March 2011
NOTE: THIS IS VERY SPECULATIVE! treat it as a starting point

call with behaviour(Hungry_status,	% 0-4000 range
			Food_proximity	% 1-4 range (or more)
			Tree_proximity	% same
			Cross_proximity	% same
			Hornet_proximity	% same
			Damage_status).	% 0-100

The result should be data written to the console screen:
			Eat_choice		% output variable 0-1
			Push_tree		% 0-100 (arbitrary)
			Push_cross		% 1-4
			Flee_hornet		% 1-3
example:

?- behaviour(1000, 1,1,1,1, 60).	% hungry & hurt

*/

%....................................................................
% FUZZY INPUT VARIABLES - crisp inputs to fuzzy set memberships

% bumble's state of hungry depends on his (crisp) level of strength
% V-World WS2 states:
% strength < 1000 = hungry, > 3000 = not hungry, <1000 - 2999> no change
% this is modified for fuzzy reasoning using fuzzy subsets within the
% fuzzy (input) variable - hungry
%FE hungry_status edited at 13:59:23 on 22/3/2010
fuzzy_variable(hungry_status) :- 
   [0,4000] ; 
   starving,                                        \,  linear, [0,540] ; 
   desperate,                                       /\, linear, [125,600,1221] ; 
   hungry,                                          /\, linear, [540,1220,1990] ; 
   not_hungry,                                      /,  linear, [1230,2000] . 

% bumble's hurt state depends on his (crisp) level of damage
% V-World WS2 states:
% damage > 50 = hurt, =0 then not hurt, <1-50> no change
% this is modified for fuzzy reasoning using 3 fuzzy subsets within the
% fuzzy (input) variable - hurt
%FE damage_status edited at 13:28:26 on 22/3/2010
fuzzy_variable(damage_status) :- 
   [0,100] ; 
   not_hurt,                                        \,  linear, [0,15] ; 
   small_damage,                                    /\, linear, [0,20,40] ; 
   hurt,                                            /,  linear, [25,50] .

% bumble 1.1 can only perceive the world state in adjacent cells.
% V-World WS3 extends this to 2 tiles. It is easy to see how this
% limit could be extended, or ray-casting could be used to perceive
% using line of sight, etc. This simple fuzzy variable can only use 2
%FE food_proximity edited at 11:0:54 on 22/3/2010
fuzzy_variable(food_proximity) :- 
   [0,10] ; 
   next_to_fruit,                                   \,  linear, [1,2] ; 
   close_to_fruit,                                  /\, linear, [1,2,3] ; 
   far_from_fruit,                                  /,  linear, [2,4] .

% similarly, bumble can perceive trees (1.1) if one is next to him
% in later versions, he might be able to perceive them 'close_to'
%FE tree_proximity created at 11:19:12 on 22/3/2010
fuzzy_variable(tree_proximity) :- 
   [0,4] ; 
   next_to_tree,                                    \,  linear, [1,2] ; 
   close_to_tree,                                   /\, linear, [1,2,3] .

% bumble can also perceive crosses (1.1) if one is next to him
% in later versions, he might be able to perceive them 'close_to'
%FE cross_proximity edited at 12:47:54 on 22/3/2010
fuzzy_variable(cross_proximity) :- 
   [0,4] ; 
   next_to_cross,                                   \, linear, [1,2] ; 
   close_to_cross,                                  /, linear, [1,2] .

% and bumble can perceive hornets (1.1) if one is next to him
% in later versions, he might be able to perceive them 'close_to'
%FE hornet_proximity edited at 14:14:40 on 22/3/2010
fuzzy_variable(hornet_proximity) :- 
   [0,4] ; 
   next_to_hornet,                                  \, linear, [1,2] ; 
   close_to_hornet,                                 /, linear, [1,2] . 

%....................................................................
% FUZZY OUTPUT VARIABLES - these represent results
%FE eat_choice edited at 13:17:53 on 22/3/2010
fuzzy_variable(eat_choice) :- 
   [0,3] ; 
   eat,                                             /, linear, [0,1] ; 
   leave_fruit_alone,                               \, linear, [0,1] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

%FE tree_response edited at 13:11:41 on 22/3/2010
fuzzy_variable(tree_response) :- 
   [0,100] ; 
   push_tree,                                       /, linear, [1,21] ; 
   leave_tree,                                      \, linear, [0,20] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

%FE repair edited at 11:27:21 on 22/3/2010
%FE repair_choice edited at 12:47:4 on 22/3/2010
fuzzy_variable(repair_choice) :- 
   [0,4] ; 
   press_cross,                                     /, linear, [0,1] ; 
   leave_cross,                                     \, linear, [0,1] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

%FE hornet_choice edited at 13:25:27 on 22/3/2010
fuzzy_variable(hornet_choice) :- 
   [0,3] ; 
   flee,                                            /, linear, [0,1] ; 
   ignore_hornet,                                   \, linear, [0,1] ; 
   centroid(all_memberships,mirror_rule,shrinking) . 

% fuzzy rules
%....................................................................
% rules to decide whether to eat the fruit

uncertainty_rule(eat_action1):-
	if hungry_status is hungry and		% hungry
	food_proximity is next_to_fruit 		% next to fruit
	then 
	eat_choice is eat.

uncertainty_rule(eat_action2):-
	if hungry_status is hungry and		% hungry
	food_proximity is close_to_fruit 		% close to fruit
	then 
	eat_choice is leave_fruit_alone.

uncertainty_rule(eat_action3):-
	if hungry_status is hungry and		% hungry
	food_proximity is far_from_fruit 		% far from fruit
	then 
	eat_choice is leave_fruit_alone.

uncertainty_rule(eat_action4):-
	if hungry_status is not_hungry 		% not hungry - leave fruit
	then 
	eat_choice is leave_fruit_alone.

uncertainty_rule(eat_action5):-
	if hungry_status is hungry and		% hungry
	tree_proximity is next_to_tree		% next to tree
	then 
	tree_response is push_tree.

uncertainty_rule(eat_action6):-
	if hungry_status is hungry and		% hungry
	tree_proximity is close_to_tree		% close to tree
	then 
	tree_response is leave_tree.

uncertainty_rule(eat_action7):-
	if hungry_status is not_hungry 		% not hungry
	then 
	tree_response is leave_tree.

%....................................................................
% rules to decide whether to push the red cross

uncertainty_rule(repair_action1):-
	if damage_status is hurt and			% hurt
	cross_proximity is next_to_cross		% next to cross
	then 
	repair_choice is press_cross.

uncertainty_rule(repair_action2):-
	if damage_status is hurt and			% hurt
	cross_proximity is close_to_cross		% close to cross
	then 
	repair_choice is leave_cross.

uncertainty_rule(repair_action3):-
	if damage_status is not_hurt			% not hurt
	then 
	repair_choice is leave_cross.

uncertainty_rule(repair_action4):-
	if damage_status is small_damage 		% hurt in some small way
	then 
	repair_choice is leave_cross.


%....................................................................
% rules to decide whether to flee the hornet

uncertainty_rule(flee_action1):-
	if damage_status is hurt and			% hurt
	hornet_proximity is next_to_hornet		% next to hornet
	then 
	hornet_choice is flee.

uncertainty_rule(flee_action2):-
	if damage_status is hurt and			% hurt
	hornet_proximity is close_to_hornet		% close to hornet
	then 
	hornet_choice is ignore_hornet.

uncertainty_rule(flee_action3):-
	if damage_status is not_hurt 			% not hurt
	then 
	hornet_choice is ignore_hornet.

uncertainty_rule(flee_action4):-
	if damage_status is small_damage		% hurt in some small way
	then 
	hornet_choice is ignore_hornet.

%....................................................................
% Propagation of fuzzy rules

% main calling predicate behaviour/6

behaviour(Hungry_status, Food_proximity, Tree_proximity, 
		Cross_proximity,Hornet_proximity,Damage_status):-
	 food(Hungry_status, Food_proximity, Tree_proximity,Eat_choice,Tree_response),
	 repair(Cross_proximity,Damage_status,Repair_choice),
	 flee(Hornet_proximity,Damage_status,Hornet_choice),
		nl,
		write('eat the fruit?' - Eat_choice),nl,
		write('push the tree?' - Tree_response),nl,
		write('press the cross?' - Repair_choice),nl,
		write('flee the hornet?' - Hornet_choice),nl.

% uncertainty propagation process, split into 3 separate clauses for
% simplicity (clarity really)

food(Hungry_status, Food_proximity, Tree_proximity,Eat_choice,Tree_response):-
	uncertainty_trace,
	fuzzy_reset_membership,
	fuzzy_variable_value(hungry_status, Hungry_status),
	fuzzy_variable_value(food_proximity, Food_proximity),
	fuzzy_variable_value(tree_proximity, Tree_proximity),
	fuzzy_propagate([eat_action1,eat_action2,eat_action3,
				eat_action4,eat_action5,eat_action6,eat_action7]),
	fuzzy_variable_value(eat_choice, Eat_choice),
	fuzzy_variable_value(tree_response, Tree_response).

repair(Cross_proximity,Damage_status,Repair_choice):-
	uncertainty_trace,
	fuzzy_reset_membership,
	fuzzy_variable_value(cross_proximity, Cross_proximity),
	fuzzy_variable_value(damage_status, Damage_status),
	fuzzy_propagate([repair_action1,repair_action2,repair_action3,repair_action4]),
	fuzzy_variable_value(repair_choice, Repair_choice).

flee(Hornet_proximity,Damage_status,Hornet_choice):-
	uncertainty_trace,
	fuzzy_reset_membership,
	fuzzy_variable_value(hornet_proximity, Hornet_proximity),
	fuzzy_variable_value(damage_status, Damage_status),
	fuzzy_propagate([flee_action1,flee_action2,flee_action3,flee_action4]),
	fuzzy_variable_value(hornet_choice, Hornet_choice).





