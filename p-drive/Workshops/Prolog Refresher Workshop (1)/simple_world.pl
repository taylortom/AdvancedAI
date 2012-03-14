/*
simple list-based representation
based on V-World

Each of the facts below is the same clause vmap/3
Argument
1.	(0,0) this is the X,Y values that specify the LEVEL
	i.e. 0,0 for the current level, 1,0 for one level EAST
	2,0 for 2 levels east
	-1,0 for one level WEST
	0,1 for one level SOUTH
	0,-1 for one level NORTH
	(these are the protocols in V-World)

2.	0-7 (in the simpligied case below) represent the Y coordinates
	of the world

3.	[w,w,w,...] These are objects in the X direction in the world
	i.e. the object at X=5,Y=3 at level (0,0) is a tree
	Note: the list positions are numbered 0-7, although this will
	be modified (see below) to cater for mamber/3
*/
:- dynamic [vmap/3]. % so that we can use assert/1 & retract/1

vmap((0,0),0,[w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,w,o,o,o,w]).
vmap((0,0),2,[w,o,o,w,o,o,o,w]).
vmap((0,0),3,[w,o,o,w,o,tree,o,w]).
vmap((0,0),4,[w,o,hornet,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,o,o,bird,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,w]).
vmap((0,0),7,[w,w,w,w,w,w,w,w]).

/* .....................................................
Start of inspect object at X,Y
Level = (0,0)
X is the position in the list starting at zero
Y is the Y-coordinate (2nd arg to vmap/3, again from zero
Object is the variable to be bound, i.e the object at
that location.
*/

inspect(Level,X,Y,Object):-
	vmap(Level,Y,List),	% make the binding to List
	K is X+1,			% member/3 references from one
	member(Object,List,K).	% what exists at that location?
	
% End of inspect object at X,Y
% .....................................................


/* .......................................................
start of Place/ replace objects in the map

replace_obj/4 places (replaces) the Object at the given
Level and X (Pos)/ Y coordinate.

It first finds and retracts the X List at that Level &
Y coordinate, does the object replacement & then asserts
the new list (Newlist)
*/

replace_obj(Level, Pos, Y, Obj):-
	retract(vmap(Level, Y, List)),
	replace(Obj,Pos,List,Newlist),
	assert(vmap(Level, Y,Newlist)).

/* This is both the boundary condition and the part that
actually does the placement/ replacement

Pos is the position in the relevant X coord list. Note it
is numbered from zero.

When this reaches zero, the correct position in the list 
is found. Only at that point does the rest of the clause
perform its actions.

first it makes a binding to Rest, i.e. those objects that
follow the required position up to the end of the list
Then it puts Obj at the head of it, i.e. replacing anything
already there.

This is the end of recursion, so as it unwinds, it puts
back those objects it removed as it recursed to the
given position.
*/

replace(Obj, 0,[_|Rest],[Obj|Rest]).

/* This part does the search through the list and sets
up the lists for after recursion. It will continually
reduce the value of Pos until = 0, at which point the
boundary condition is satisfied (see above).
*/

replace(Obj,Pos,[X|Y],[X|Z]):-
	Pos > 0,
	M is Pos - 1,
	replace(Obj,M,Y,Z).

% start of Place/ replace objects in the map
% ......................................................



