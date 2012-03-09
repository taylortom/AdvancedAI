% SEARCH ROUTINES FOR VIRTUAL WORLD (SEARCH.PL) 
% 
% Copyright 2005 Donald Nute 
% Date created: 8/10/2005 
% Last modified: 8/16/2005 
% 
% This file contains a sample agent's map of a virtual
% world and a generalized search algorithm for finding
% paths through the virtual world. 

% The general search algorithm begins with a queue containing
% the current set of nodes in the search space - in this case,
% a single path (list of locations) of length one, containing
% only the agent's current location. The algorithm is also given
% a goal condition. Search proceeds by testing the first node
% in the queue to see if it satisfies the goal condition. If
% it does, then it is returned as the solution to the problem.
% Otherwise, all nodes below the first node in the search space
% (all expansions of the first node) are generated, the
% first node in the queue is removed from the queue, and its
% expansions are inserted into the queue. Then the process
% is repeated until a solution is found or the queue is
% empty and search fails.

:- dynamic [search_type/1, mygoal/1].

search([FirstNode|_],FirstNode) :-
	solution(FirstNode).

search([FirstNode|RestOfNodes],Solution) :-
	generate_new_nodes(FirstNode,NewNodes),
	insert_nodes(NewNodes,RestOfNodes,NextSetOfNodes),
	!,
	search(NextSetOfNodes,Solution).


% The method used to insert new nodes in the queue determines
% which kind of search is used. You should assert either
% doing_depth_first_search/0 or doing_breadth_first_search/0
% before beginning your search.

% For depth first search, put new nodes at front of queue.

insert_nodes(Set1,Set2,Set3) :-
	search_type(depth_first),
	append(Set1,Set2,Set3).

% For breadth first search, put new nodes at back of queue.

insert_nodes(Set1,Set2,Set3) :-
	search_type(breadth_first),
	append(Set2,Set1,Set3).

how_long(Goal,Duration,Error) :-
	time(0,Start),
	catch(Error,Goal,Return),
	time(0,Finish),
	Start = (SDays,SMilliseconds),
	Finish = (FDays,FMilliseconds),
	Duration is (86400000 * FDays) + FMilliseconds - (86400000 * SDays) - SMilliseconds,
	(
	 Return = 0,
	 Message = `Goal succeeded.`
	;
	 error_message(Error,Message)
	).

% Now we define a function specific to V-World that uses the
% generalized search algorithm.
%
% find_path(SearchType,Goal,Start,Path) searches for a list of
% coordinates (a Path) that leads from the agent's current 
% location Start (of the form (X,Y)) to a location where
% an object of the sort specified by Goal is located. Goal
% might be tree or cross, for example. The kind of search
% peformed is determined by SearchType, which can be depth_first
% or breadth_first. To simplify programming, % the Path is built
% backwards and then reversed.
%
% The SearchType and Goal are asserted into clauses where they
% can be used by insert_nodes/3 and solution/1.

find_path(SearchType,Goal,Start,Path) :-
	retractall(search_type(_)),
	assert(search_type(SearchType)),
	retractall(mygoal(_)),
	assert(mygoal(Goal)),
	search([[Start]],ReversedPath),
	reverse(ReversedPath,Path).

% A path satisfies a Goal if the first (eventually, last)
% position in the path is occupied by Goal.
%
% Of course, you would need a different definition of solution
% for a different domain.

solution([(X,Y)|_]) :-
	mygoal(Goal),
	mymap(X,Y,Goal).

% generate_new_nodes(Path,Goal,Paths) finds (almost) all
% legal Paths that extend Path by exactly one step in any 
% direction. A legal path is one made up entirely of
% locations that, so far as the agent can tell from the
% map, it can occupy. These are empty locations, locations 
% where there are doors, locations that contain the goal
% object, or locations occupied by some object that the
% agent can remove (listed in clauses for the predicate 
% can_remove/1.)
%
% expand_node/3 actually embodies a heuristic since it does
% not allow circular Paths to be built. Of course, we would
% have to define generate_new_nodes/3 differently for a
% different domain.

generate_new_nodes([(X,Y)|Rest],Paths) :-
	findall([(XX,YY),(X,Y)|Rest],
	(
	 Xminus is X - 1,
	 Xplus is X + 1,
	 Yminus is Y - 1,
	 Yplus is Y + 1,
	 member(XX,[Xminus,X,Xplus]),
	 member(YY,[Yminus,Y,Yplus]),
	 mymap(XX,YY,Obj),
	 \+ member((XX,YY),[(X,Y)|Rest]),
	 (
	  mygoal(Obj)
	 ;
	  member(Obj,[o,door])
	 ;
	  can_remove(Obj)
	 )
	),
	Paths).

% These are the objects that the agent can remove
% from a location in test7.vw.

can_remove(fruit).
can_remove(gkey).
can_remove(ykey).
can_remove(bugspray).

% mymap/3 provides an agent's map of test7.vw

mymap( -5, -19, w ).

mymap( -4, -19, w ).

mymap( -3, -19, w ).

mymap( -2, -19, w ).

mymap( -1, -19, w ).

mymap( 0, -19, w ).

mymap( 1, -19, w ).

mymap( 2, -19, w ).

mymap( 3, -19, w ).

mymap( 4, -19, w ).

mymap( 5, -19, w ).

mymap( 6, -19, w ).

mymap( 7, -19, w ).

mymap( 8, -19, w ).

mymap( 9, -19, w ).

mymap( 10, -19, w ).

mymap( 11, -19, w ).

mymap( 12, -19, w ).

mymap( 13, -19, w ).

mymap( 14, -19, w ).

mymap( 15, -19, w ).

mymap( 16, -19, w ).

mymap( -5, -18, w ).

mymap( -4, -18, w ).

mymap( -3, -18, w ).

mymap( -2, -18, w ).

mymap( -1, -18, w ).

mymap( 0, -18, w ).

mymap( 1, -18, w ).

mymap( 2, -18, w ).

mymap( 3, -18, o ).

mymap( 4, -18, o ).

mymap( 5, -18, o ).

mymap( 6, -18, o ).

mymap( 7, -18, o ).

mymap( 8, -18, w ).

mymap( 9, -18, w ).

mymap( 10, -18, w ).

mymap( 11, -18, w ).

mymap( 12, -18, w ).

mymap( 13, -18, w ).

mymap( 14, -18, w ).

mymap( 15, -18, w ).

mymap( 16, -18, w ).

mymap( -5, -17, w ).

mymap( -4, -17, w ).

mymap( -3, -17, w ).

mymap( -2, -17, w ).

mymap( -1, -17, w ).

mymap( 0, -17, w ).

mymap( 1, -17, w ).

mymap( 2, -17, w ).

mymap( 3, -17, o ).

mymap( 4, -17, o ).

mymap( 5, -17, gkey ).

mymap( 6, -17, o ).

mymap( 7, -17, o ).

mymap( 8, -17, w ).

mymap( 9, -17, w ).

mymap( 10, -17, w ).

mymap( 11, -17, w ).

mymap( 12, -17, w ).

mymap( 13, -17, w ).

mymap( 14, -17, w ).

mymap( 15, -17, w ).

mymap( 16, -17, w ).

mymap( -5, -16, w ).

mymap( -4, -16, w ).

mymap( -3, -16, w ).

mymap( -2, -16, w ).

mymap( -1, -16, w ).

mymap( 0, -16, w ).

mymap( 1, -16, w ).

mymap( 2, -16, w ).

mymap( 3, -16, o ).

mymap( 4, -16, o ).

mymap( 5, -16, o ).

mymap( 6, -16, o ).

mymap( 7, -16, o ).

mymap( 8, -16, w ).

mymap( 9, -16, w ).

mymap( 10, -16, w ).

mymap( 11, -16, w ).

mymap( 12, -16, w ).

mymap( 13, -16, w ).

mymap( 14, -16, w ).

mymap( 15, -16, w ).

mymap( 16, -16, w ).

mymap( -5, -15, w ).

mymap( -4, -15, w ).

mymap( -3, -15, w ).

mymap( -2, -15, w ).

mymap( -1, -15, w ).

mymap( 0, -15, w ).

mymap( 1, -15, w ).

mymap( 2, -15, w ).

mymap( 3, -15, o ).

mymap( 4, -15, o ).

mymap( 5, -15, o ).

mymap( 6, -15, o ).

mymap( 7, -15, o ).

mymap( 8, -15, w ).

mymap( 9, -15, w ).

mymap( 10, -15, w ).

mymap( 11, -15, w ).

mymap( 12, -15, w ).

mymap( 13, -15, w ).

mymap( 14, -15, w ).

mymap( 15, -15, w ).

mymap( 16, -15, w ).

mymap( -5, -14, w ).

mymap( -4, -14, w ).

mymap( -3, -14, w ).

mymap( -2, -14, w ).

mymap( -1, -14, w ).

mymap( 0, -14, w ).

mymap( 1, -14, w ).

mymap( 2, -14, w ).

mymap( 3, -14, o ).

mymap( 4, -14, o ).

mymap( 5, -14, o ).

mymap( 6, -14, o ).

mymap( 7, -14, o ).

mymap( 8, -14, w ).

mymap( 9, -14, w ).

mymap( 10, -14, w ).

mymap( 11, -14, w ).

mymap( 12, -14, w ).

mymap( 13, -14, w ).

mymap( 14, -14, w ).

mymap( 15, -14, w ).

mymap( 16, -14, w ).

mymap( -5, -13, w ).

mymap( -4, -13, w ).

mymap( -3, -13, w ).

mymap( -2, -13, w ).

mymap( -1, -13, w ).

mymap( 0, -13, w ).

mymap( 1, -13, w ).

mymap( 2, -13, w ).

mymap( 3, -13, o ).

mymap( 4, -13, o ).

mymap( 5, -13, o ).

mymap( 6, -13, o ).

mymap( 7, -13, o ).

mymap( 8, -13, w ).

mymap( 9, -13, w ).

mymap( 10, -13, w ).

mymap( 11, -13, w ).

mymap( 12, -13, w ).

mymap( 13, -13, w ).

mymap( 14, -13, w ).

mymap( 15, -13, w ).

mymap( 16, -13, w ).

mymap( -5, -12, w ).

mymap( -4, -12, w ).

mymap( -3, -12, w ).

mymap( -2, -12, w ).

mymap( -1, -12, w ).

mymap( 0, -12, w ).

mymap( 1, -12, w ).

mymap( 2, -12, w ).

mymap( 3, -12, o ).

mymap( 4, -12, o ).

mymap( 5, -12, o ).

mymap( 6, -12, o ).

mymap( 7, -12, o ).

mymap( 8, -12, w ).

mymap( 9, -12, w ).

mymap( 10, -12, w ).

mymap( 11, -12, w ).

mymap( 12, -12, w ).

mymap( 13, -12, w ).

mymap( 14, -12, w ).

mymap( 15, -12, w ).

mymap( 16, -12, w ).

mymap( -5, -11, w ).

mymap( -4, -11, w ).

mymap( -3, -11, w ).

mymap( -2, -11, w ).

mymap( -1, -11, w ).

mymap( 0, -11, w ).

mymap( 1, -11, w ).

mymap( 2, -11, w ).

mymap( 3, -11, o ).

mymap( 4, -11, o ).

mymap( 5, -11, o ).

mymap( 6, -11, o ).

mymap( 7, -11, o ).

mymap( 8, -11, w ).

mymap( 9, -11, w ).

mymap( 10, -11, w ).

mymap( 11, -11, w ).

mymap( 12, -11, w ).

mymap( 13, -11, w ).

mymap( 14, -11, w ).

mymap( 15, -11, w ).

mymap( 16, -11, w ).

mymap( -5, -10, w ).

mymap( -4, -10, w ).

mymap( -3, -10, w ).

mymap( -2, -10, w ).

mymap( -1, -10, w ).

mymap( 0, -10, w ).

mymap( 1, -10, w ).

mymap( 2, -10, w ).

mymap( 3, -10, o ).

mymap( 4, -10, o ).

mymap( 5, -10, o ).

mymap( 6, -10, o ).

mymap( 7, -10, o ).

mymap( 8, -10, w ).

mymap( 9, -10, w ).

mymap( 10, -10, w ).

mymap( 11, -10, w ).

mymap( 12, -10, w ).

mymap( 13, -10, w ).

mymap( 14, -10, w ).

mymap( 15, -10, w ).

mymap( 16, -10, w ).

mymap( -5, -9, w ).

mymap( -4, -9, w ).

mymap( -3, -9, w ).

mymap( -2, -9, w ).

mymap( -1, -9, w ).

mymap( 0, -9, w ).

mymap( 1, -9, w ).

mymap( 2, -9, w ).

mymap( 3, -9, w ).

mymap( 4, -9, o ).

mymap( 5, -9, o ).

mymap( 6, -9, o ).

mymap( 7, -9, w ).

mymap( 8, -9, w ).

mymap( 9, -9, w ).

mymap( 10, -9, w ).

mymap( 11, -9, w ).

mymap( 12, -9, w ).

mymap( 13, -9, w ).

mymap( 14, -9, w ).

mymap( 15, -9, w ).

mymap( 16, -9, w ).

mymap( -5, -8, w ).

mymap( -4, -8, o ).

mymap( -3, -8, o ).

mymap( -2, -8, o ).

mymap( -1, -8, o ).

mymap( 0, -8, o ).

mymap( 1, -8, o ).

mymap( 2, -8, o ).

mymap( 3, -8, o ).

mymap( 4, -8, o ).

mymap( 5, -8, o ).

mymap( 6, -8, o ).

mymap( 7, -8, o ).

mymap( 8, -8, o ).

mymap( 9, -8, o ).

mymap( 10, -8, o ).

mymap( 11, -8, o ).

mymap( 12, -8, o ).

mymap( 13, -8, o ).

mymap( 14, -8, o ).

mymap( 15, -8, o ).

mymap( 16, -8, w ).

mymap( -5, -7, w ).

mymap( -4, -7, o ).

mymap( -3, -7, o ).

mymap( -2, -7, o ).

mymap( -1, -7, o ).

mymap( 0, -7, o ).

mymap( 1, -7, o ).

mymap( 2, -7, o ).

mymap( 3, -7, o ).

mymap( 4, -7, o ).

mymap( 5, -7, o ).

mymap( 6, -7, o ).

mymap( 7, -7, o ).

mymap( 8, -7, o ).

mymap( 9, -7, o ).

mymap( 10, -7, o ).

mymap( 11, -7, o ).

mymap( 12, -7, o ).

mymap( 13, -7, o ).

mymap( 14, -7, o ).

mymap( 15, -7, o ).

mymap( 16, -7, w ).

mymap( -5, -6, w ).

mymap( -4, -6, o ).

mymap( -3, -6, o ).

mymap( -2, -6, o ).

mymap( -1, -6, o ).

mymap( 0, -6, o ).

mymap( 1, -6, o ).

mymap( 2, -6, o ).

mymap( 3, -6, o ).

mymap( 4, -6, o ).

mymap( 5, -6, o ).

mymap( 6, -6, o ).

mymap( 7, -6, o ).

mymap( 8, -6, o ).

mymap( 9, -6, o ).

mymap( 10, -6, o ).

mymap( 11, -6, o ).

mymap( 12, -6, o ).

mymap( 13, -6, o ).

mymap( 14, -6, o ).

mymap( 15, -6, o ).

mymap( 16, -6, w ).

mymap( -5, -5, w ).

mymap( -4, -5, o ).

mymap( -3, -5, o ).

mymap( -2, -5, o ).

mymap( -1, -5, o ).

mymap( 0, -5, o ).

mymap( 1, -5, o ).

mymap( 2, -5, o ).

mymap( 3, -5, o ).

mymap( 4, -5, o ).

mymap( 5, -5, o ).

mymap( 6, -5, o ).

mymap( 7, -5, o ).

mymap( 8, -5, o ).

mymap( 9, -5, o ).

mymap( 10, -5, o ).

mymap( 11, -5, o ).

mymap( 12, -5, o ).

mymap( 13, -5, o ).

mymap( 14, -5, o ).

mymap( 15, -5, o ).

mymap( 16, -5, w ).

mymap( -5, -4, w ).

mymap( -4, -4, yblock ).

mymap( -3, -4, door ).

mymap( -2, -4, yblock ).

mymap( -1, -4, w ).

mymap( 0, -4, w ).

mymap( 1, -4, w ).

mymap( 2, -4, w ).

mymap( 3, -4, w ).

mymap( 4, -4, w ).

mymap( 5, -4, w ).

mymap( 6, -4, w ).

mymap( 7, -4, w ).

mymap( 8, -4, w ).

mymap( 9, -4, w ).

mymap( 10, -4, w ).

mymap( 11, -4, w ).

mymap( 12, -4, w ).

mymap( 13, -4, gblock ).

mymap( 14, -4, door ).

mymap( 15, -4, gblock ).

mymap( 16, -4, w ).

mymap( -5, -3, w ).

mymap( -4, -3, yblock ).

mymap( -3, -3, door ).

mymap( -2, -3, yblock ).

mymap( -1, -3, w ).

mymap( 0, -3, w ).

mymap( 1, -3, w ).

mymap( 2, -3, w ).

mymap( 3, -3, w ).

mymap( 4, -3, w ).

mymap( 5, -3, w ).

mymap( 6, -3, w ).

mymap( 7, -3, w ).

mymap( 8, -3, w ).

mymap( 9, -3, w ).

mymap( 10, -3, w ).

mymap( 11, -3, w ).

mymap( 12, -3, w ).

mymap( 13, -3, gblock ).

mymap( 14, -3, door ).

mymap( 15, -3, gblock ).

mymap( 16, -3, w ).

mymap( -5, -2, w ).

mymap( -4, -2, o ).

mymap( -3, -2, o ).

mymap( -2, -2, o ).

mymap( -1, -2, o ).

mymap( 0, -2, o ).

mymap( 1, -2, o ).

mymap( 2, -2, o ).

mymap( 3, -2, o ).

mymap( 4, -2, w ).

mymap( 5, -2, w ).

mymap( 6, -2, w ).

mymap( 7, -2, w ).

mymap( 8, -2, o ).

mymap( 9, -2, o ).

mymap( 10, -2, o ).

mymap( 11, -2, o ).

mymap( 12, -2, o ).

mymap( 13, -2, o ).

mymap( 14, -2, o ).

mymap( 15, -2, o ).

mymap( 16, -2, w ).

mymap( -5, -1, w ).

mymap( -4, -1, o ).

mymap( -3, -1, o ).

mymap( -2, -1, o ).

mymap( -1, -1, o ).

mymap( 0, -1, o ).

mymap( 1, -1, o ).

mymap( 2, -1, o ).

mymap( 3, -1, o ).

mymap( 4, -1, w ).

mymap( 5, -1, w ).

mymap( 6, -1, w ).

mymap( 7, -1, w ).

mymap( 8, -1, o ).

mymap( 9, -1, o ).

mymap( 10, -1, o ).

mymap( 11, -1, o ).

mymap( 12, -1, o ).

mymap( 13, -1, o ).

mymap( 14, -1, o ).

mymap( 15, -1, o ).

mymap( 16, -1, w ).

mymap( -5, 0, w ).

mymap( -4, 0, o ).

mymap( -3, 0, o ).

mymap( -2, 0, o ).

mymap( -1, 0, tree ).

mymap( 0, 0, o ).

mymap( 1, 0, o ).

mymap( 2, 0, o ).

mymap( 3, 0, o ).

mymap( 4, 0, w ).

mymap( 5, 0, w ).

mymap( 6, 0, w ).

mymap( 7, 0, w ).

mymap( 8, 0, o ).

mymap( 9, 0, o ).

mymap( 10, 0, o ).

mymap( 11, 0, o ).

mymap( 12, 0, cross ).

mymap( 13, 0, o ).

mymap( 14, 0, o ).

mymap( 15, 0, o ).

mymap( 16, 0, w ).

mymap( -5, 1, w ).

mymap( -4, 1, o ).

mymap( -3, 1, o ).

mymap( -2, 1, o ).

mymap( -1, 1, o ).

mymap( 0, 1, o ).

mymap( 1, 1, o ).

mymap( 2, 1, o ).

mymap( 3, 1, o ).

mymap( 4, 1, w ).

mymap( 5, 1, w ).

mymap( 6, 1, w ).

mymap( 7, 1, w ).

mymap( 8, 1, o ).

mymap( 9, 1, o ).

mymap( 10, 1, o ).

mymap( 11, 1, o ).

mymap( 12, 1, o ).

mymap( 13, 1, o ).

mymap( 14, 1, o ).

mymap( 15, 1, o ).

mymap( 16, 1, w ).

mymap( -5, 2, w ).

mymap( -4, 2, o ).

mymap( -3, 2, o ).

mymap( -2, 2, o ).

mymap( -1, 2, o ).

mymap( 0, 2, o ).

mymap( 1, 2, o ).

mymap( 2, 2, o ).

mymap( 3, 2, o ).

mymap( 4, 2, w ).

mymap( 5, 2, w ).

mymap( 6, 2, w ).

mymap( 7, 2, w ).

mymap( 8, 2, o ).

mymap( 9, 2, o ).

mymap( 10, 2, o ).

mymap( 11, 2, o ).

mymap( 12, 2, o ).

mymap( 13, 2, o ).

mymap( 14, 2, o ).

mymap( 15, 2, o ).

mymap( 16, 2, w ).

mymap( -5, 3, w ).

mymap( -4, 3, o ).

mymap( -3, 3, o ).

mymap( -2, 3, o ).

mymap( -1, 3, o ).

mymap( 0, 3, o ).

mymap( 1, 3, o ).

mymap( 2, 3, o ).

mymap( 3, 3, o ).

mymap( 4, 3, w ).

mymap( 5, 3, w ).

mymap( 6, 3, w ).

mymap( 7, 3, w ).

mymap( 8, 3, o ).

mymap( 9, 3, o ).

mymap( 10, 3, o ).

mymap( 11, 3, o ).

mymap( 12, 3, o ).

mymap( 13, 3, o ).

mymap( 14, 3, o ).

mymap( 15, 3, o ).

mymap( 16, 3, w ).

mymap( -5, 4, w ).

mymap( -4, 4, w ).

mymap( -3, 4, w ).

mymap( -2, 4, w ).

mymap( -1, 4, o ).

mymap( 0, 4, o ).

mymap( 1, 4, w ).

mymap( 2, 4, w ).

mymap( 3, 4, w ).

mymap( 4, 4, w ).

mymap( 5, 4, w ).

mymap( 6, 4, w ).

mymap( 7, 4, w ).

mymap( 8, 4, w ).

mymap( 9, 4, w ).

mymap( 10, 4, w ).

mymap( 11, 4, o ).

mymap( 12, 4, o ).

mymap( 13, 4, w ).

mymap( 14, 4, w ).

mymap( 15, 4, w ).

mymap( 16, 4, w ).

mymap( -5, 5, w ).

mymap( -4, 5, w ).

mymap( -3, 5, w ).

mymap( -2, 5, w ).

mymap( -1, 5, o ).

mymap( 0, 5, o ).

mymap( 1, 5, w ).

mymap( 2, 5, w ).

mymap( 3, 5, w ).

mymap( 4, 5, w ).

mymap( 5, 5, w ).

mymap( 6, 5, w ).

mymap( 7, 5, w ).

mymap( 8, 5, w ).

mymap( 9, 5, w ).

mymap( 10, 5, w ).

mymap( 11, 5, o ).

mymap( 12, 5, o ).

mymap( 13, 5, w ).

mymap( 14, 5, w ).

mymap( 15, 5, w ).

mymap( 16, 5, w ).

mymap( -5, 6, w ).

mymap( -4, 6, w ).

mymap( -3, 6, o ).

mymap( -2, 6, o ).

mymap( -1, 6, o ).

mymap( 0, 6, o ).

mymap( 1, 6, o ).

mymap( 2, 6, o ).

mymap( 3, 6, o ).

mymap( 4, 6, w ).

mymap( 5, 6, w ).

mymap( 6, 6, w ).

mymap( 7, 6, w ).

mymap( 8, 6, o ).

mymap( 9, 6, o ).

mymap( 10, 6, o ).

mymap( 11, 6, o ).

mymap( 12, 6, o ).

mymap( 13, 6, o ).

mymap( 14, 6, o ).

mymap( 15, 6, w ).

mymap( 16, 6, w ).

mymap( -5, 7, w ).

mymap( -4, 7, w ).

mymap( -3, 7, o ).

mymap( -2, 7, o ).

mymap( -1, 7, o ).

mymap( 0, 7, o ).

mymap( 1, 7, o ).

mymap( 2, 7, o ).

mymap( 3, 7, o ).

mymap( 4, 7, w ).

mymap( 5, 7, w ).

mymap( 6, 7, w ).

mymap( 7, 7, w ).

mymap( 8, 7, o ).

mymap( 9, 7, o ).

mymap( 10, 7, o ).

mymap( 11, 7, o ).

mymap( 12, 7, o ).

mymap( 13, 7, o ).

mymap( 14, 7, o ).

mymap( 15, 7, w ).

mymap( 16, 7, w ).

mymap( -5, 8, w ).

mymap( -4, 8, w ).

mymap( -3, 8, o ).

mymap( -2, 8, o ).

mymap( -1, 8, o ).

mymap( 0, 8, o ).

mymap( 1, 8, o ).

mymap( 2, 8, o ).

mymap( 3, 8, o ).

mymap( 4, 8, w ).

mymap( 5, 8, w ).

mymap( 6, 8, w ).

mymap( 7, 8, w ).

mymap( 8, 8, o ).

mymap( 9, 8, o ).

mymap( 10, 8, o ).

mymap( 11, 8, o ).

mymap( 12, 8, o ).

mymap( 13, 8, o ).

mymap( 14, 8, o ).

mymap( 15, 8, w ).

mymap( 16, 8, w ).

mymap( -5, 9, w ).

mymap( -4, 9, w ).

mymap( -3, 9, o ).

mymap( -2, 9, o ).

mymap( -1, 9, o ).

mymap( 0, 9, ykey ).

mymap( 1, 9, o ).

mymap( 2, 9, o ).

mymap( 3, 9, o ).

mymap( 4, 9, w ).

mymap( 5, 9, w ).

mymap( 6, 9, w ).

mymap( 7, 9, w ).

mymap( 8, 9, o ).

mymap( 9, 9, o ).

mymap( 10, 9, o ).

mymap( 11, 9, bugspray ).

mymap( 12, 9, o ).

mymap( 13, 9, o ).

mymap( 14, 9, o ).

mymap( 15, 9, w ).

mymap( 16, 9, w ).

mymap( -5, 10, w ).

mymap( -4, 10, w ).

mymap( -3, 10, o ).

mymap( -2, 10, o ).

mymap( -1, 10, o ).

mymap( 0, 10, o ).

mymap( 1, 10, o ).

mymap( 2, 10, o ).

mymap( 3, 10, o ).

mymap( 4, 10, w ).

mymap( 5, 10, w ).

mymap( 6, 10, w ).

mymap( 7, 10, w ).

mymap( 8, 10, o ).

mymap( 9, 10, o ).

mymap( 10, 10, o ).

mymap( 11, 10, o ).

mymap( 12, 10, o ).

mymap( 13, 10, o ).

mymap( 14, 10, o ).

mymap( 15, 10, w ).

mymap( 16, 10, w ).

mymap( -5, 11, w ).

mymap( -4, 11, w ).

mymap( -3, 11, w ).

mymap( -2, 11, w ).

mymap( -1, 11, w ).

mymap( 0, 11, w ).

mymap( 1, 11, w ).

mymap( 2, 11, w ).

mymap( 3, 11, w ).

mymap( 4, 11, w ).

mymap( 5, 11, w ).

mymap( 6, 11, w ).

mymap( 7, 11, w ).

mymap( 8, 11, w ).

mymap( 9, 11, w ).

mymap( 10, 11, w ).

mymap( 11, 11, w ).

mymap( 12, 11, w ).

mymap( 13, 11, w ).

mymap( 14, 11, w ).

mymap( 15, 11, w ).

mymap( 16, 11, w ).

mymap( -5, 12, w ).

mymap( -4, 12, w ).

mymap( -3, 12, w ).

mymap( -2, 12, w ).

mymap( -1, 12, w ).

mymap( 0, 12, w ).

mymap( 1, 12, w ).

mymap( 2, 12, w ).

mymap( 3, 12, w ).

mymap( 4, 12, w ).

mymap( 5, 12, w ).

mymap( 6, 12, w ).

mymap( 7, 12, w ).

mymap( 8, 12, w ).

mymap( 9, 12, w ).

mymap( 10, 12, w ).

mymap( 11, 12, w ).

mymap( 12, 12, w ).

mymap( 13, 12, w ).

mymap( 14, 12, w ).

mymap( 15, 12, w ).

mymap( 16, 12, w ).

