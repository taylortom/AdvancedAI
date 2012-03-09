% VIRTUAL WORLD (VWORLD.PL) uses gfx_icon_load_old (Dec 09)
% The file 47_icons.pl has been copied and appended to
% vworld.pl
% 
% Copyright 2003 Donald Nute 
% Date project started: 12/01/1996 
% Last modified by him: 12/08/2005 
% Date last modified by GW: 15/12/2009
% 
% This program operates an artificial environment into 
% which an agent may be inserted. To complete the 
% environment, a world file must be loaded using the
% File menu in this program.  

:- dynamic [vagent_data/5, vmap/3, vobject_icon/2, veff/3, vact/3, vreact/3,
 	vcurrent/2, vstop/0, vrelocate_agent/0, vgame_over/0, vmovable/1,
	vcollectable/1, vmulticollectable/1, vconsumable/1, vanimate/1, vkey/7, 
	agent/2, agent_icon/1, vdrop_effect/1, vfears/2, v_icon/3, 
	vagent_icon/2, vwants/2, vavoids/2, vgoal_to_catch_bird/0].   

:- multifile vobject_icon/2, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
	vmovable/1, vanimate/1, vmenu/2, vact/3, vreact/3, vnature/0, vdrop_effect/1,
	vfears/2, v_icon/3, vagent_icon/2, vwants/2, vavoids/2, vgoal_to_catch_bird/0.

% vstartup is the initial query when VWORLD.OVL is created.  

vstartup :-
	catch(Code,(vmain,!),Culprit),
	(
	 Code = 0
	;
	 Code = -1 -> msgbox(`Error in Program`,`The program failed.`,0,_)
	;
	 (
	  error_message(Code,ErrorMessage),
	  (write(ErrorMessage),nl,write(`while calling `),write(Culprit)) ~> Message,
	  msgbox(`Error in Program`,Message,16'00000031,_)
	 )
	),
	halt(0). 

% vmain calls the initialization routines, starts the VWORLD 
% window handler, and closes the window and menus when the dialog ends.   

vmain :-
	switch(a,0),
	switch(b,0),
 	prolog_flag(unknown,_,fail),
	retractall(vcurrent(_,_)),
 	vload_icons,
 	vcreate_screen,
 	call_dialog(vworld),
	vshow_initial_world.
/* 	repeat,		% exit dialog now handled in vexit/1 (GW)
 	wait(0),
 	wshow(vworld,Status),
 	Status = 0,
	vtidy. */

vtidy :-
 	wmclose(vfile),
 	wmclose(voptions),
 	wmclose(vhelp),
 	wclose(vworld).
% 	help('vworld.hlp',2,0).

vmain.
 
% vrun starts the action in VWORLD and keeps it going until 
% a halt condition is satisfied.  

vrun :-
	vwell_and_able(_,_,_,_),
	retractall(vcurrent(state,_)),
 	assert(vcurrent(state,running)),
 	vmenu_status(`&Creep`,0),
 	vmenu_status(`&Leap`,0),
 	vmenu_status(`&Abort`,1),
 	repeat,
	wmnusel(voptions,6,Status),
	one((
	     Status = 1
	    ;
	     wait(0)
	   )), 	
	one((vtest(Action),
		vactions(Action,Goal),
		Goal,
		veverybody_act,
 		vlast(Goal))),
 	vhalt,
	retractall(vcurrent(state,_)),
	retractall(vstop),
	retractall(vgame_over),
 	retractall(vcurrent(view,_)),
	vcolor_world,
	vmenu_status(`&Leap`,1),
 	vmenu_status(`&Creep`,1),
	vmenu_status(`&Abort`,0),
 	vmenu_status(`Manual Testing Mode`,1).
  
vtest(Move) :-
 	vagent_look(Strength,Damage,Inventory,Last,R1,R2,R3,R4,R5),
	agent([Strength,Damage,Inventory,Last,R1,R2,R3,R4,R5],Move).

% vhalt succeeds if one of the conditions for halting the 
% action in VWORLD is satisfied.  

vhalt :-
 	vgame_over,
	vmenu_status(`Change Agent Icon`,1),
	vmenu_status(`Change Agent Location`,0),
	vmenu_status(`Reset Agent Strength`,1),
	vmenu_status(`Reset Agent Damage`,1),
	vmenu_status(`Manual Testing Mode`,1).

vhalt :- 
	vstop,
	vmenu_status(`Change Agent Icon`,1),
	vmenu_status(`Change Agent Location`,1),
	vmenu_status(`Reset Agent Strength`,1),
	vmenu_status(`Reset Agent Damage`,1),
	vmenu_status(`Manual Testing Mode`,1).

vhalt :-
	switch(b,N),	% Step limit is set
	N > 0,
	switch(a,M),	% Number of steps taken
	K is M + 1,
	switch(a,K),
	K = N,
	vmenu_status(`Change Agent Icon`,1),
	vmenu_status(`Change Agent Location`,1),
	vmenu_status(`Reset Agent Strength`,1),
	vmenu_status(`Reset Agent Damage`,1),
	vmenu_status(`Manual Testing Mode`,1).
  

% vagent_look(-S,-D,-I,-L,-R1,-R2,-R3,-R4,-R5) returns the strength, damage, and  
% inventory of the agent plus the 5X5 grid of objects occupying  
% the locations in the current level of the world map centered on the 
% agent's current location.  

vagent_look(Strength,Damage,Inventory,Last,R1,R2,R3,R4,R5) :-
 	vagent_data(XX,YY,Strength,Damage,Last),
 	vlook(XX,YY,R1,R2,R3,R4,R5),
	sndmsg((vworld,400),lb_getcount,0,0,N),
	(
	 N = 0,
	 Inventory = []
	;
	 M is N - 1,
	 findall(Item,(integer_bound(0,K,M),wlbxget((vworld,400),K,Str),
		vstring_term(Str,Item)),Inventory)
	).
 
% T(+X,+Y,-R1,-R2,-R3,-R4,-R5) returns the 5X5 grid of objects  
% occupying the locations in the current level of the world map 
% centered on the location X,Y. Objects in the outer portion of
% the grid are only visible if something transparent stands between
% that object and the actor/agent at X,Y. 

vlook(XX,YY,
 	[A,B,C,D,E],
 	[F,G,H,I,J],
 	[K,L,M,N,O],
 	[P,Q,R,S,T],
 	[U,V,W,X,Y]) :-
	vmap_section(XX,YY,
 		[AT,BT,CT,DT,ET],
 		[FT,G,H,I,JT],
 		[KT,L,M,N,OT],
 		[PT,Q,R,S,TT],
 		[UT,VT,WT,XT,YT]),
	vsee_beyond(G,G,AT,A),
	vsee_beyond(G,H,BT,B),
	vsee_beyond(H,H,CT,C),
	vsee_beyond(I,H,DT,D),
	vsee_beyond(I,I,ET,E),
	vsee_beyond(I,N,JT,J),
	vsee_beyond(N,N,OT,O),
	vsee_beyond(N,S,TT,T),
	vsee_beyond(S,S,YT,Y),
	vsee_beyond(R,S,XT,X),
	vsee_beyond(R,R,WT,W),
	vsee_beyond(Q,R,VT,V),
	vsee_beyond(Q,Q,UT,U),
	vsee_beyond(L,Q,PT,P),
	vsee_beyond(L,L,KT,K),
	vsee_beyond(G,L,FT,F).

vsee_beyond(Obj1,Obj2,Obj,Obj) :-
	(
	 vtransparent(Obj1)
	;
	 vtransparent(Obj2)
	),
	!.

vsee_beyond(_,_,_,cant_see).

vmap_section(Lev,XX,YY,
 	[A,B,C,D,E],
 	[F,G,H,I,J],
 	[K,L,M,N,O],
 	[P,Q,R,S,T],
 	[U,V,W,X,Y]) :-
 	Y1 is YY - 2,
 	Y2 is YY - 1,
 	Y4 is YY + 1,
 	Y5 is YY + 2,
 	IX is XX - 2,
 	vscan(Lev,Y1,IX,[A,B,C,D,E]),
 	vscan(Lev,Y2,IX,[F,G,H,I,J]),
 	vscan(Lev,YY,IX,[K,L,M,N,O]),
 	vscan(Lev,Y4,IX,[P,Q,R,S,T]),
 	vscan(Lev,Y5,IX,[U,V,W,X,Y]).

vmap_section(XX,YY,
 	[A,B,C,D,E],
 	[F,G,H,I,J],
 	[K,L,M,N,O],
 	[P,Q,R,S,T],
 	[U,V,W,X,Y]) :-
 	vcurrent(level,Lev),
 	Y1 is YY - 2,
 	Y2 is YY - 1,
 	Y4 is YY + 1,
 	Y5 is YY + 2,
 	IX is XX - 2,
 	vscan(Lev,Y1,IX,[A,B,C,D,E]),
 	vscan(Lev,Y2,IX,[F,G,H,I,J]),
 	vscan(Lev,YY,IX,[K,L,M,N,O]),
 	vscan(Lev,Y4,IX,[P,Q,R,S,T]),
 	vscan(Lev,Y5,IX,[U,V,W,X,Y]).

% An actor or agent can see what is on the other side of a 
% consumable or transparent object.

vtransparent(Object) :-
	(
	 Object = o
	;
	 Object = o(_)
	;
	 vconsumable(Object)
	;
	 vcollectable(Object)
	;
	 member(vconsumable,Object)
	;
	 member(vcollectable,Object)
	).

% vactions(+Action,-Goal) finds the internal vworld Goal
% to implement an agent's Action.

vactions(sit,vsit).

vactions(drop(String),vdrop(Obj)) :-
	string(String),
	!,
	atom_string(Obj,String).

vactions(drop(Obj),vdrop(Obj)).

vactions(move(Dir),vmove(Dir)).

vsit :-
 	!,
 	retract(vagent_data(X,Y,S,D,_)),
 	SS is S - 5,
 	assert(vagent_data(X,Y,SS,D,same)).

% (+Atom) tries to drop an object indicated in Atom to
% a nearby vacant location. Dropping and object may have
% some effect defined by a clause for vdrop_effect(Obj)

vdrop(Item) :-
	vwell_and_able(X,Y,_,_),
	vremove_from_inventory(Item,Obj),
	(
	 vdrop_effect(Obj)
	;
	 vlocate(X,Y,o,XX,YY),
	 (
	  Obj = [Type|_],
	  vdraw(XX,YY,Type)
	 ;
	  vdraw(XX,YY,Obj)
	 )
	;
	 vadd_to_inventory(Item)
	),
	!,
	vagent_data(X,Y,Str,Dmg,_),
 	retractall(vagent_data(_,_,_,_,_)),
 	NuStr is Str - 5,
	assert(vagent_data(X,Y,NuStr,Dmg,same)).  

vdrop(_) :-
	vwell_and_able(_,_,Str,Dmg),
	NuStr is Str - 5,
	vagent_data(X,Y,_,_,_),
 	retractall(vagent_data(_,_,_,_,_)),
 	assert(vagent_data(X,Y,NuStr,Dmg,same)).
	  
% vmove(+Dir) tries to move the agent in the direction Dir. What 
% happens depends on what occupies that location.

vmove(Dir) :-
 	vwell_and_able(X,Y,Str,Dmg),
 	!,
 	vtransform(X,Y,Dir,XX,YY),
 	vinspect(current,XX,YY,Obj),
 	vpush(a,X,Y,Obj,XX,YY,Dir,NuX,NuY,AdjStr1,AdjDmg,Move),
	(
	 member(Dir,[ne,nw,se,sw]),
	 AdjStr = 14 * AdjStr1
	;
	 AdjStr = 10 * AdjStr1
	),
 	NuStr is Str + AdjStr,
 	NuDmg is Dmg + AdjDmg,
 	retractall(vagent_data(_,_,_,_,_)),
 	(
 	 Move = 1,
 	 member(Dir,[n,s,e,w,ne,nw,se,sw],N),
 	 member(Last,[s,n,w,e,sw,se,nw,ne],N)
 	;
 	 Last = same
 	),
 	assert(vagent_data(NuX,NuY,NuStr,NuDmg,Last)).  

vmove(_).  

% vmove(+Actor,+X,+Y,+XX,+YY) moves an Actor from location X,Y to 
% location XX,YY in the current level, leaving location X,Y empty.   

vmove(Actor,X,Y,XX,YY) :-
 	vdraw(X,Y,o),
	vdraw(XX,YY,o),
 	vdraw(XX,YY,Actor).

% vinjure(+N) increases the agent's damage by N.

vinjure(N) :-
	retract(vagent_data(XX,YY,Str,Dmg,Last)),
 	NuDmg is Dmg + N,
 	assert(vagent_data(XX,YY,Str,NuDmg,Last)).  

% vactor(?L,[?Actor,-X,-Y]) returns a list containing an Actor other than  
% the agent in level L and its location X,Y.  

vactor(current,[Actor,X,Y]) :-
 	vcurrent(level,L),
 	vactor(L,[Actor,X,Y]).  

vactor(L,[Actor,X,Y]) :-
 	vmap(L,Y,List),
	member(Actor,List,Z),
	one(( 	 vanimate(Actor)
	;
	 member(vanimate,Actor)
	)),
	X is Z - 1.  

% veverybody_act calls upon every actor in the current level other than the 
% agent in a random order. After all actors have acted, vnature acts and 
% the status display is updated. (vnature is defined in the world file.)  

veverybody_act :-
	findall(X,vactor(current,X),List),
 	vrandom_permutation(List,ActorList),
	vtake_turns(ActorList),
 	vvnature,
 	vstatus_report.  

vvnature :-
 	vnature,
 	!. 

vvnature.  

% vtake_turns(ActorList) calls upon the actors in the current level other 
% than the agent in random order.  

vtake_turns([]).  

vtake_turns([[Actor,X,Y]|Rest]) :-
 	vact(Actor,X,Y),
 	!,
 	vtake_turns(Rest).  

% vaction(+Actor,+X,+Y) calls upon vact/3, which must be defined in the 
% world file. If there is no information in the world file about how 
% Actor is supposed to act, vaction/3 succeeds without doing anything.  

vaction(Actor,X,Y) :-
 	vact(Actor,X,Y).  

vaction(_,_,_).  

% vpush(+Actor,+X,+Y,+Obj,+XX,+YY,+Dir,-NuX,-NuY,-AdjStr,-AdjDmg,-Last) 
% updates the world map and display that results when an Actor at 
% location X,Y in the current level tries to move into the space occupied 
% by an Obj at location XX,YY. push/12 also returns the new location  
% NuX,NuY of the Actor after the action, then changes AdjStr and AdjDmg 
% to the Actor's strength and damage resulting from the action, and 
% any Item that is to be added to the Actor's inventory as a result of 
% the action. Last = 1 if the agent changed position.  
% When an actor moves into and "empty" space or into the space of a 
% consumable object, the object disappears and the actor enjoys/suffers
% the effects.  

vpush(Actor,X,Y,Obj,XX,YY,_,XX,YY,AdjStr,AdjDmg,1) :-
	(
	 Obj = o
	;
	 Obj = o(_)
	;
 	 vconsumable(Obj)
	;
	 member(vconsumable,Obj)
	),
	!,
 	veffect(Obj,AdjStr,AdjDmg),
 	vmove(Actor,X,Y,XX,YY).  

% When an actor moves into the space of a collectable object, the 
% object is added to the actor's inventory, and the actor  
% enjoys/suffers the effects.  

vpush(Actor,X,Y,Obj,XX,YY,_,XX,YY,AdjStr,AdjDmg,1) :-
	(
 	 vcollectable(Obj)
	;
	 vmulticollectable(Obj)
	;
	 member(vcollectable,Obj)
	;
	 member(vmulticollectable,Obj)
	),
	!,
	vadd_to_inventory(Obj),
 	veffect(Obj,AdjStr,AdjDmg),
 	vmove(Actor,X,Y,XX,YY). 

% that object moves out of the way unless this would require it 
% to move into the space occupied by another object. In that case, 
% the second object must also move. Several objects may be 
% moved unless some immovable object stands in the way. The effects 
% of moving several objects with one push are accumulated and passed 
% back to the actor. This particular clause is recursive since a 
% movable object becomes an actor which may move another object.  

vpush(Actor,X,Y,Obj,XX,YY,Dir,XX,YY,AdjStr,AdjDmg,1) :-
	(
 	 vmovable(Obj)
	;
	 member(vmovable,Obj)
	),
 	vtransform(XX,YY,Dir,XXX,YYY),
 	vinspect(current,XXX,YYY,NuObj),
 	vpush(Obj,XX,YY,NuObj,XXX,YYY,Dir,XXX,YYY,AdjStr1,AdjDmg1,1),
 	veffect(Obj,AdjStr2,AdjDmg2),
 	AdjStr is AdjStr1 + AdjStr2,
 	AdjDmg is AdjDmg1 + AdjDmg2,
 	vmove(Actor,X,Y,XX,YY).  

% To enter a portal, an agent must have any key required by the 
% portal in its inventory. If an agent lacks a key for a portal, the  
% portal becomes an immovable object. Some portals may not require  
% keys. Others may be locked, but require some other action to open  
% them rather than possession of a key. Actors other than the agent 
% cannot use portals.  

vpush(a,X,Y,Portal,XX,YY,_,NuX,NuY,-1,0,1) :-
	(
	 Portal = door
	;
	 Portal = door(_)
	),
 	vcurrent(level,L),
	(
 	 vkey(K,L,XX,YY,L2,XXX,YYY)
	;
	 vkey(K,L2,XXX,YYY,L,XX,YY)
	),
 	(
 	 K = none
 	;
 	 vfind_in_inventory(K)
 	),
	(
	 NuX is XXX - 1,
	 vinspect(L2,NuX,YYY,o),
	 NuY = YYY
	;
	 NuX is XXX + 1,
	 vinspect(L2,NuX,YYY,o),
	 NuY = YYY
	;
	 NuY is YYY - 1,
	 vinspect(L2,XXX,NuY,o),
	 NuX = XXX
	;
	 NuY is YYY + 1,
	 vinspect(L2,XXX,NuY,o),
	 NuX = XXX
	;
 	 vlocate(L2,XXX,YYY,o,NuX,NuY)
	),
 	!,
 	vdraw(X,Y,o),
 	retract(vcurrent(level,_)),
 	assert(vcurrent(level,L2)),
 	vdraw(NuX,NuY,a),
 	vcolor_world.  

% If none of the above apply, the agent is pushing against an 
% immovable object. The agent does not change position, but 
% the action may have far-ranging effects.  

vpush(_,X,Y,Obj,XX,YY,_,X,Y,AdjStr,AdjDmg,0) :-
	vreaction(Obj,XX,YY),
 	veffect(Obj,AdjStr,AdjDmg).

% vscan(+L,+Y,+X,-View) returns a list of the object at level L 
% and position X,Y, plus the four objects occupying positions 
% to the right of the first object. Locations off the map can't
% be seen.

vscan(L,Y,-2,[cant_see,cant_see,A,B,C]) :-
	!,
	vscan(L,Y,0,[A,B,C,_,_]).

vscan(L,Y,-1,[cant_see,A,B,C,D]) :-
	!,
	vscan(L,Y,0,[A,B,C,D,_]).

vscan(L,Y,X,View) :-
 	vmap(L,Y,List),
	!,
 	vscan_aux(X,List,View).

vscan(_,_,_,[cant_see,cant_see,cant_see,cant_see,cant_see]).

vscan_aux(0,[A,B,C,D],[A,B,C,D,cant_see]).

vscan_aux(0,[A,B,C],[A,B,C,cant_see,cant_see]).

vscan_aux(0,[A,B,C,D,E|_],[A,B,C,D,E]). 

vscan_aux(X,[_|List],View) :-
 	XX is X - 1,
 	vscan_aux(XX,List,View).  

% vinspect(+L,+X,+Y,-Obj) returns the object Obj located at  
% X,Y in level L of the currently loaded world map. L may 
% be the atom 'current', in which case the current level is used, or it 
% may be a structure representing an explicit level in the world map.  

vinspect(L,X,Y,Obj) :-
	(
	 L = current,
	 vcurrent(level,J)
	;
	 J = L
	),
 	vmap(J,Y,List),
	K is X + 1,
	member(Obj,List,K).  

% vdraw(+X,+Y,+Obj) places the symbol for Obj in the world map at 
% location X,Y in the current level and draws the icon for Obj 
% on the screen at the X,Y location.  

vdraw(X,Y,Obj) :-
	(
	 vcurrent(view,agent),
	 vagent_data(XA,YA,_,_,_),
	 (
	  X < XA - 2
	 ;
	  XA + 2 < X
	 ;
	  Y < YA - 2
	 ;
	  YA + 2 < Y
	 ),
	 Icon = dark
	;
	 vobject_icon(Obj,Icon)
	),
 	vcurrent(level,Lev),
 	vdraw(Lev,X,Y,Obj),
 	XX is X * 32,
 	YY is Y * 32,
 	gfx_begin((vworld,1)),
 	gfx(icon(XX,YY,Icon)),
 	gfx_end((vworld,1)),
 	retractall(vrelocate_agent).  

% vdraw(+Lev,+X,+Y,+Obj) places Obj in the world map 
% at location X,Y on level Lev. It does not draw any icons.  

vdraw(Lev,N,Y,Obj) :-
 	retract(vmap(Lev,Y,List)),
 	vreplace(Obj,N,List,NewList),
 	assert(vmap(Lev,Y,NewList)).  

vreplace(Obj,0,[_|X],[Obj|X]).  

vreplace(Obj,N,[X|Y],[X|Z]) :-
 	N > 0,
 	M is N - 1,
 	vreplace(Obj,M,Y,Z).  

% vtransform(+X,+Y,+Dir,-XX,-YY) determines the XX,YY coordinates of the 
% location one square away from the location X,Y in direction DIR.  

vtransform(X,Y,n,X,YY) :-
 	YY is Y - 1. 

vtransform(X,Y,s,X,YY) :-
 	YY is Y + 1. 

vtransform(X,Y,e,XX,Y) :-
 	XX is X + 1. 

vtransform(X,Y,w,XX,Y) :-
 	XX is X - 1. 

vtransform(X,Y,ne,XX,YY) :-
 	XX is X + 1,
 	YY is Y - 1. 

vtransform(X,Y,nw,XX,YY) :-
 	XX is X - 1,
 	YY is Y - 1. 

vtransform(X,Y,se,XX,YY) :-
 	XX is X + 1,
 	YY is Y + 1. 

vtransform(X,Y,sw,XX,YY) :-
 	XX is X - 1,
 	YY is Y + 1.  

% vstatus_report displays the current strength, 
% damage, and inventory for the agent at the top 
% of the screen. Strength will never exceed 
% a maximum value of 4000, and damage will never 
% drop below a minimum of 0. vstatus_report also 
% increments the move counter and displays it, and 
% it repaints the world if the program is in agent
% view mode.  

vstatus_report :-
 	retract(vagent_data(X,Y,S,D,Last)),
 	(
 	 S > 4000 -> SS = 4000
 	;
 	 SS = S
 	),
 	(
 	 D < 0 -> DD = 0
 	;
 	 DD = D
 	),
 	assert(vagent_data(X,Y,SS,DD,Last)),
 	number_string(SS,SStr),
 	wtext((vworld,200),SStr),
 	number_string(DD,DStr),
 	wtext((vworld,300),DStr),
 	retract(vnumber_of_moves(M)),
 	MM is M + 1,
 	assert(vnumber_of_moves(MM)),
 	number_string(MM,MStr),
 	wtext((vworld,500),MStr),
	(
	 \+ vcurrent(view,agent)
	;
	 vcolor_world
	),
 	vdie,
	!.

vstatus_report.

% vwell_and_able(-X,-Y,-S,-D) returns the location, strength, 
% and damage for the agent provided the agent has strength greater 
% than 0 and damage less than 100. Otherwise, well_and_able/4 fails.  

vwell_and_able(X,Y,S,D) :-
 	vagent_data(X,Y,S,D,_),
 	S > 0,
 	D < 100.  

% vdie will replace the agent's icon on the screen with a tombstone 
% if the agent "starves" (strength is below 0) or is "killed" (damage % is above 100).  

vdie :-
 	\+ vwell_and_able(_,_,_,_),
	retractall(vcurrent(view,agent)),
	vcolor_world,
	!,
 	vagent_data(X,Y,_,_,_),
 	XX is X * 32,
 	YY is Y * 32,
 	gfx_begin((vworld,1)),
 	gfx(icon(XX,YY,grave)),
 	gfx_end((vworld,1)), 
	vmenu_status(`&Leap`,0),
	vmenu_status(`&Creep`,0),
	vmenu_status(`&Abort`,0),
	vmenu_status(`Change Agent Icon`,0),
	vmenu_status(`Change Agent Location`,0),
	vmenu_status(`Manual Testing Mode`,0),
	assert(vstop).  

vdie.  

% vlast(+Action) displays the last action taken by the agent.  

vlast(Action) :-
 	vlast_string(Action,String),
 	wtext((vworld,600),String),
	!.

vlast_string(vsit,`sit`). 

vlast_string(vmove(Dir),String) :-
	member(Dir,[n,s,e,w,ne,nw,se,sw]),
	!,
	atom_string(Dir,DirStr),
	cat([`move(`,DirStr,`)`],String,_).

vlast_string(vdrop(Obj),String) :-
	!,
	vstring_term(ObjStr,Obj),
	cat([`drop(`,ObjStr,`)`],String,_). 

vlast_string(_,`error`).  

% vcolor_world reads the world map for the current level 
% and displays it.  

vcolor_world :-
 	vcurrent(level,Lev),
 	vcolor_row(Lev,0).  

vcolor_row(Lev,N) :-
 	vmap(Lev,N,List),
	!,
	vcolor_cells(N,0,List),
 	M is N + 1,
 	vcolor_row(Lev,M).

vcolor_row(_,_).

vcolor_cells(_,_,[]).

vcolor_cells(N,K,[_|Rest]) :-
	vcurrent(view,agent),
	vagent_data(XX,YY,_,_,_),
	(
	 abs(K - XX) =< 2,		% Object within range of agent's vision
	 abs(N - YY) =< 2,
	 vagent_look(_,_,_,_,R1,R2,R3,R4,R5),
	 XXX is K - XX + 3,
	 YYY is N - YY + 3,
	 member(R,[R1,R2,R3,R4,R5],YYY),
	 member(Obj,R,XXX),
	 vobject_icon(Obj,Icon)
	;
	 Icon = dark			% Object outside range of agent's vision
	),
	X is 32 * K,
 	Y is 32 * N,
 	gfx_begin((vworld,1)),
 	gfx(icon(X,Y,Icon)),
 	gfx_end((vworld,1)),
 	J is K + 1,
	!,
 	vcolor_cells(N,J,Rest).

vcolor_cells(N,K,[Obj|Rest]) :-
	(
	 Obj = a,
 	 \+ vwell_and_able(_,_,_,_),
	 Icon = grave
	;
	 vobject_icon(Obj,Icon)
	),
 	!,
 	X is 32 * K,
 	Y is 32 * N,
 	gfx_begin((vworld,1)),
 	gfx(icon(X,Y,Icon)),
 	gfx_end((vworld,1)),
 	J is K + 1,
 	vcolor_cells(N,J,Rest).  

vcolor_cells(N,K,[_|Rest]) :-
 	M is K + 1,
 	vcolor_cells(N,M,Rest).  

% veffect(+Obj,-S,-D) returns the change in strength and damage resulting 
% when the agent interacts with Obj. veff/3 must be defined in the world
% file.  

veffect(Obj,S,D) :-
 	veff(Obj,S,D),
 	!. 

veffect(_,0,0).  

% vreaction(+Obj,+X,+Y) accomplishes the reaction of any 
% object when the agent tries to move into the space 
% it occupies. vreaction/3 calls vreact/3 which must be 
% defined within the world file. If vreact/3 is not defined
% for the object and location, then there is no reaction.  

vreaction(Obj,X,Y) :-
 	vreact(Obj,X,Y),
 	!.  

vreaction(_,_,_).  

% Randomization routines.  

vrandom(N,M,K) :-
 	J is M - N + 1,
 	K is rand(J)//1 + N.  

vrandom_member(X,List) :-
 	len(List,L),
 	vrandom(1,L,K),
 	member(X,List,K).  

vrandom_permutation(Old,New) :-
 	vrandom_permutation(Old,[],New),
 	!.  

vrandom_permutation([],X,X). 

vrandom_permutation(Old,SoFar,New) :-
 	vrandom_member(Y,Old),
 	remove(Y,Old,Next),
 	!,
 	vrandom_permutation(Next,[Y|SoFar],New).  

random_direction(Dir) :-
 	vrandom_member(Dir,[n,s,e,w,ne,se,nw,sw]).  

% v_hndl/4 is the window handler for VWORLD.  

v_hndl(vworld,msg_close,_,R) :-
	vexit(R).

v_hndl((vworld,1),msg_paint,_,_) :-
	\+ vcurrent(level,_),
	vshow_initial_world.

v_hndl(vworld,msg_menu,N,R) :-
	vmenu_item_name(N,Menu),
	vmenu(Menu,R).

voccurs(X,X).

voccurs(X,(X,_)).

voccurs(X,(_,Y)) :-
	voccurs(X,Y).

v_hndl((vworld,1),msg_leftup,(X,Y),_) :-
 	retract(vrelocate_agent),
	!,
 	XX is X//32,
 	YY is Y //32,
 	vinspect(current,XX,YY,o),
 	retract(vagent_data(XXX,YYY,S,D,_)),
 	assert(vagent_data(XX,YY,S,D,same)),
 	vdraw(XXX,YYY,o),
 	vdraw(XX,YY,a).  

v_hndl((vworld,1),msg_leftup,_,_) :-
	wfocus((vworld,1)).

 v_hndl((vworld,1),msg_char,Key,_) :-
	vwell_and_able(_,_,_,_),
	wmnusel(voptions,5,Test),
 	Test = 1,
	(
 	 member(Key,[up,prior,right,next,down,end,left,home],N),
 	 member(Move,[n,ne,e,se,s,sw,w,nw],N),
 	 Last = vmove(Move)
	;
	 Key = insert,
	 Last = vsit
	;
	 Key = delete,
	 vpick_inventory_item(Obj),
	 Last = vdrop(Obj)
	),
	!,
	Last,
 	veverybody_act,
 	vlast(Last),
	(
	 Last = vdrop(none),
	 msgbox(`How to drop an object`,
		`To drop an object, first select it in the inventory list, then click near your agent icon, then press the delete key.`,
		0,_)
	;
	 true
	).

v_hndl((vworld,400),msg_select,_,_) :-
	 msgbox(`How to drop an object`,
`To drop the object you have selected, click near your agent icon and press the delete key.

If you do not want to drop this object, click near your agent and continue the game.`,
		0,_).

v_hndl((vworld,1),msg_paint,_,_) :-
 	vcolor_world.

% vpick_inventory_item(-Obj) uses a dialog box to
% get an inventory item fromt he user.

vpick_inventory_item(Obj) :-
	sndmsg((vworld,400),lb_getcursel,0,0,N),
	(
	 N = -1,
	 Str = `none`
	;
	 wlbxget((vworld,400),N,Str)
	),
	vstring_term(Str,Obj).

v_hndl((vworld,400),msg_select,_,_) :-
	vfocus((vworld,400)).  

% Any message not handled by the above clauses are 
% passed to the standard LPA window handler.  

v_hndl(Window,Message,Data,Result) :-
 	window_handler(Window,Message,Data,Result).  

vmenu(`Load World`,_) :-
	opnbox(`Load World`,[(`VWORLD files: *.vw`,`*.vw`)],``,'vw',[NewWorld|_]),
	(
	 retract(vcurrent(world,World)),
	 abolish_files([World]),
	 vload_icons
	;
	 true
	),
	retractall(vkey(_,_,_,_,_,_,_)),
	consult(NewWorld),
	retractall(vcurrent(_,_)),
 	assert(vcurrent(world,NewWorld)),
	sndmsg((vworld,400),lb_resetcontent,0,0,_),
 	verify_world_loaded,
	fname(NewWorld,_,Name,_),
	atom_string(Name,String),
	cat([`Virtual World - `,String],Caption,_),
	wtext(vworld,Caption),
	wmnusel(voptions,4,0),
	wmnusel(voptions,5,0),
	wmnusel(voptions,6,0).  

verify_world_loaded :-
 	\+ vmap(_,_,_),
 	!,
 	msgbox(`Incorrect File`, `Your world file did not contain a world map.`,16,_).  

verify_world_loaded :-
 	retractall(vgame_over),
	retractall(vcurrent(level,_)),
	assert(vcurrent(level,(0,0))),
 	retractall(vnumber_of_moves(_)),
 	assert(vnumber_of_moves(-1)),
 	forall(v_icon(Actor,File,Pos),
		(
		 gfx_icon_load_old(Actor,File,Pos),
		 assert(vobject_icon(Actor,Actor))
		)),
 	vcolor_world,
 	vstatus_report,	
	vmenu_status(`&Leap`,0),
	vmenu_status(`&Creep`,0),
	vmenu_status(`&Abort`,0),
	vmenu_status(`Change Agent Icon`,0),
	vmenu_status(`Change Agent Location`,0),
	vmenu_status(`Reset Agent Strength`,0),
	vmenu_status(`Reset Agent Damage`,0),
	vmenu_status(`Manual Testing Mode`,0),
	vmenu_status(`Quick Time`,0),
 	vmenu_status(`Load Agent`,1).

vmenu(`Load Agent`,_) :-
	vload_agent.

vload_agent :-
 	opnbox(`Load Agent`,[(`Agent files: *.agt`,`*.agt`)],``,'agt',[NewAgent|_]),
 	(
	 retract(vcurrent(agent,Agent)),
	 abolish_files([Agent])
	;
	 true
	),
 	consult(NewAgent),
	assert(vcurrent(agent,NewAgent)),
 	verify_agent_loaded.  

verify_agent_loaded :-
 	\+ clause(agent(_,_),_),
 	msgbox(`Improper agent file.`,
 		`The file you loaded did not contain any agent/2 clauses.`,
 		16,_),
 	!.  

verify_agent_loaded :- 	
	vmenu_status(`Load Agent`,0),
	vmenu_status(`&Leap`,1),
	vmenu_status(`&Creep`,1),
	vmenu_status(`&Abort`,1),
	vmenu_status(`Change Agent Icon`,1),
	vmenu_status(`Change Agent Location`,1),
	vmenu_status(`Reset Agent Strength`,1),
	vmenu_status(`Reset Agent Damage`,1),
	vmenu_status(`Manual Testing Mode`,1),
	vmenu_status(`Quick Time`,1),

% Make the agent visible.
 	vagent_data(X,Y,_,_,_),
 	retractall(vobject_icon(a,_)),
 	vagent_icons([First|Others]),
 	(
 	 vagent_icon(Icon,vworld),
 	 member(Icon,[First|Others])
 	;
 	 vagent_icon(IconFile,N),
 	 catch(Error,gfx_icon_load_old(agent,IconFile,N)),
 	 Error = 0,
 	 Icon = agent
 	;
 	 Icon = First
 	),
 	assert(vobject_icon(a,Icon)),
 	vdraw(X,Y,a).

vmenu(`Exit`,R) :-
	vexit(R).

vexit(R) :-
	msgbox(`Exit V-World`,
 		`Do you want to exit V-World?`,36,B),
	(
	 B \== 6
	;
	 R = ok,
vtidy
	).  

% Window handler for the Actions/Stop dialog. Number of steps
% is stored in switch b.  

vstep_hndl((stop_dialog,3),msg_button,_,ok) :-
 	wtext((stop_dialog,2),S),
 	number_string(N,S),
 	integer(N),
	N > 0,
 	switch(b,N),
 	wmnusel(vactions,2,1).  

vstep_hndl((stop_dialog,3),msg_button,_,_) :-
 	msgbox(`Error`,
	 `You should enter an integer greater than 0 as your stop value.`,
	 16,_),
 	wfocus((stop_dialog,2)).  

vstep_hndl((stop_dialog,4),msg_button,_,ok) :-
 	msgbox(`Run till you die!`,
	 `No maximum number of moves has been set and the action will continue until the agent dies.`,
 		64,_),
 	switch(b,0),
 	wmnusel(vactions,2,0).

vmnuadd(Dlg,Pos,Name,MenuToAdd) :-
 	wndhdl(Dlg,Handle),
	winapi((user32,'GetMenu'),[Handle],0,Menu),
 	wmnuadd(menu(Menu),Pos,Name,MenuToAdd),
	winapi((user32,'DrawMenuBar'),[Handle],0,_).

vmnudel(Dlg,Pos) :-
 	wndhdl(Dlg,Handle),
 	winapi((user32,'GetMenu'),[Handle],0,Menu),
 	wmnudel(menu(Menu),Pos),
 	winapi,((user32,'DrawMenuBar'),[Handle],0,_).

vmnunbl(Dlg,Pos,Status) :-
 	wndhdl(Dlg,Handle),
 	winapi((user32,'GetMenu'),[Handle],0,Menu),
 	wmnunbl(menu(Menu),Pos,Status),
 	winapi((user32,'DrawMenuBar'),[Handle],0,_).  

% vcat(+List,'',-Atom) concatenates a List of atoms into Atom, placing 
% an underscore between each atom in List.  vcat([],SoFar,SoFar).  

vcat([A|Rest],'',Atom) :-
 	!,
 	vcat(Rest,A,Atom).  

vcat([A|Rest],SoFar,Atom) :-
 	cat([SoFar,'_',A],Next,_),
 	!,
 	vcat(Rest,Next,Atom).

% vfind_in_world(+Object,-Level,-X,-Y) succeeds and returns the location
% of the Object if the Object is anywhere in the world.

vfind_in_world(Obj,Level,X,Y) :-
	vmap(Level,Y,List),
	member(Obj,List,XX),
	X is XX - 1.

% vlocate(+X,+Y,+Object,-XX,-YY) locates coordinates XX,YY adjacent to 
% coordinates X,Y in the current level at which an Object is located.  

vlocate(X,Y,Object,XX,YY) :-
 	vmap_section(X,Y,
 		_,
 		[_,G,H,I,_],
 		[_,L,_,M,_],
 		[_,P,Q,R,_],
 		_),
 	member(Object,[G,H,I,L,M,P,Q,R],Pos),
 	member(Dir,[nw,n,ne,w,e,sw,s,se],Pos),
 	vtransform(X,Y,Dir,XX,YY).  

vlocate(Lev,X,Y,Object,XX,YY) :-
 	vmap_section(Lev,X,Y,
 		_,
 		[_,G,H,I,_],
 		[_,L,_,M,_],
 		[_,P,Q,R,_],
 		_),
 	member(Object,[G,H,I,L,M,P,Q,R],Pos),
 	member(Dir,[nw,n,ne,w,e,sw,s,se],Pos),
 	vtransform(X,Y,Dir,XX,YY).  

% vadjacent(+X,+Y,List) returns a list of objects other than
% walls adjacent to the X,Y position in the current level.

vadjacent(X,Y,List) :-
	vmap_section(X,Y,
 		_,
 		[_,G,H,I,_],
 		[_,L,_,M,_],
 		[_,P,Q,R,_],
 		_),
 	findall(Object,(member(Object,[G,H,I,L,M,P,Q,R]),
 		\+ member(Object,[w,o])),List).  

% vdetect(+X,+Y,+Object) succeeds if an Object other than a wall
% is visible from coordinates X,Y.  

vdetect(X,Y,Object) :-
 	vlook(X,Y,
 		[A,B,C,D,E],
 		[F,G,H,I,J],
 		[K,L,_,M,N],
 		[O,P,Q,R,S],
 		[T,U,V,W,Z]),
 	member(Object,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,Z]),
 	\+ member(Object,[w,o,o(_)]).  


% vrandom_adjacent_location(+X,+Y,-XX,-YY) returns the
% coordinates of a randomly chosen location adjacent
% to location X,Y. This routine actually returns all
% adjacent locations in random order upon backtracking.

vrandom_adjacent_location(X,Y,XX,YY) :-
	vmap_section(X,Y,_,
		  [_,R22,R23,R24,_],
		  [_,R32,R33,R34,_],
		  [_,R42,R43,R44,_],
		  _),
	vrandom_permutation([-1,0,1],XList),
	vrandom_permutation([-1,0,1],YList),
	member(XAdd,XList),
	member(YAdd,YList),
	XX is X + XAdd,
	YY is Y + YAdd.

% vmoves(-N) returns the number of moves the agent has made.  

vmoves(N) :-
	wtext((vworld,500),Str),
 	number_string(N,Str).  

% The abort handler vabort/0  

vabort :-
 	vtidy,
	exit(0). 

% vcreate_screen creates the Virtual World dialog window.

vcreate_screen :-
 	wdcreate(vworld,`Virtual World`,0,0,800,600,
 		[ws_popup,ws_sysmenu,ws_caption,ws_border,ws_visible]),
	vcreate_menu_bar,
	vscreen_data(Menus),
	vinit_menus(0,1000,Menus),
 	wccreate((vworld,2),static,`Strength: `,3,3,47,20,
 		[ws_child,ws_visible,ss_right]),
 	wccreate((vworld,200),static,``,51,3,28,15,
 		[ws_child,ws_visible,ss_left]),
 	wccreate((vworld,3),static,`Damage: `,82,3,45,20,
 		[ws_child,ws_visible,ss_right]),
 	wccreate((vworld,300),static,``,129,3,18,15,
 		[ws_child,ws_visible,ss_left]),
 	wccreate((vworld,5),static,`Moves: `,150,3,45,20,
 		[ws_child,ws_visible,ss_right]),
 	wccreate((vworld,500),static,``,196,3,35,15,
 		[ws_child,ws_visible,ss_left]),
 	wccreate((vworld,6),static,`Last Move: `,230,3,60,20,
 		[ws_child,ws_visible,ss_right]),
 	wccreate((vworld,600),static,``,295,3,250,15,
 		[ws_child,ws_visible,ss_left]),
 	wccreate((vworld,1),grafix,``,2,22,706,514,
 		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,4),static,`Inventory: `,712,3,85,18,
 		[ws_child,ws_visible,ss_center]),
 	wccreate((vworld,400),listbox,``,712,22,80,514,
 		[ws_child,ws_visible,ws_border,lbs_sort,lbs_notify]),
	window_handler(vworld,v_hndl),
	gfx_bitmap_load(start,'castle2.bmp').

vscreen_data(
	[menu(vfile,1,`&File`,
		[1,`Load World`,
 		 0,`Load Agent`,
 		 0,``,
 		 1,`Exit`]),
	 menu(voptions,1,`&Options`,
		[
		 0,`Change Agent Icon`,
 		 0,`Change Agent Location`,
 		 0,`Reset Agent Strength`,
 		 0,`Reset Agent Damage`,
		 1,`Agent View Mode`,
 		 1,`Manual Testing Mode`,
		 1,`Quick Time`]),
	 menu(0,`&Creep`),
	 menu(0,`&Leap`),
	 menu(0,`&Abort`),
	 menu(vhelp,1,`&Help`,
		[
/*		 1,`V-World Basics`,
		 1,`Building Agents`,
		 1,`Building Worlds`,
 		 0,``,
*/
 		 1,`About`])]).  

% vload_icons loads the icons used to represent the objects 
% and actors in vworld. All icons are stored in the icon 
% library wvorld.nil.

vload_icons :-
	List = ([
	dark,		'vworld.exe',		1,
	empty,	'vworld.exe',		2,
	cross,	'vworld.exe',		4,
	door,		'vworld.exe',		5,
	grave,	'vworld.exe',		6,
	wall,		'vworld.exe',		7,
	george,	'vworld.icl',		0,
	knight,	'vworld.icl',		1,
	princess,	'vworld.icl',		2,
	witch,	'vworld.icl',		3,
	hornet,	'vworld.icl',		4,
 	snail,	'vworld.icl',		5,
 	bird,		'vworld.icl',		6,
 	dragon,	'vworld.icl',		7,
 	troll,	'vworld.icl',		8,
	bumble,	'vworld.icl',		28,
 	tree,		'vworld.icl',		9,
 	thorn,	'vworld.icl',		10,
	fruit,	'vworld.icl',		11,
	birdseed,	'vworld.icl',		25,
	flower,	'vworld.icl',		27,
	sword,	'vworld.icl',		12,
	shield,	'vworld.icl',		13,
	gold,		'vworld.icl',		14,
	throne,	'vworld.icl',		15,
	bkey,		'vworld.icl',		24,
	blublock,	'vworld.icl',		16,
	blukey,	'vworld.icl',		17,
	yblock,	'vworld.icl',		18,
	ykey,		'vworld.icl',		19,
	gblock,	'vworld.icl',		20,
	gkey,		'vworld.icl',		21,
	pblock,	'vworld.icl',		22,
	pkey,		'vworld.icl',		23,
	bugspray,	'vworld.icl',		29,
	water,	'vworld.icl',		30,
	billy,	'vworld.icl',		53,
	horace,	'vworld.icl',		54,
	lgm,		'vworld.icl',		55
	]),
	vload_icons(List),
	vset_basic_icons.

vload_icons([]).

vload_icons([Icon,File,Position|Rest]) :-
	gfx_icon_load_old(Icon,File,Position),
	!,
	vload_icons(Rest). 

vstring_term(String,Atom) :-
	(
	 atom(Atom)
	;
	 var(Atom),
	 string_chars(String,Chars),
	 Chars \== [91|_]
	),
	!,
	atom_string(Atom,String).

vstring_term(`[]`,[]) :- !.

vstring_term(String, Term) :-
	\+ var(Term),
	X = ``,
	output((X,0)),
	write(Term),
	output(Temp),
	arg(1,Temp,String),
	!.
vstring_term(String,[First|Rest]) :-
	var(Term),
	!,
	string_chars(String,[_|Chars1]),
	reverse(Chars1,[_|Chars2]),
	reverse(Chars2,Chars),
	findall(N,member(42,Chars,N),[M|NList]),
	atom_string(Atom,Chars),
	K is M - 1,
	cat(List,Atom,[K]),
	List = [First|_],
	vbuild_list(Atom,NList,Rest).

vbuild_list(LongAtom,[N],[Atom1]) :-
	cat(List,LongAtom,[N]),
	List = [_,Atom].

vbuild_list(LongAtom,[N,M|NList],[First|Rest]) :-
	K is N - 1,
	J is M - N - 1,
	cat(List,LongAtom,[K,J]),
	List = [_,First,_],
	!,
	vbuild_list(LongAtom,[M|NList],Rest).

vpick_agent :-
	wdcreate(vagents,`Select an agent icon:`,287,100,184,164,
		[ws_popup,ws_visible,ws_caption,ws_border]),
	wccreate((vagents,1),grafix,``,0,0,182,144,
		[ws_child,ws_visible]),
	vagents_paint,
	window_handler(vagents,vagent_picker),
	call_dialog(vagents,Icon),
	retractall(vobject_icon(a,_)),
 	assert(vobject_icon(a,Icon)),
 	vagent_data(X,Y,_,_,_),
	vdraw(X,Y,o),
 	vdraw(X,Y,a).

vagent_picker((vagents,1),msg_leftup,(X,Y),Icon) :-
	I is X//36 + 5 * (Y//36) + 1,
	vagent_icons(List),
	member(Icon,List,I).

vagent_picker((vagents,1),msg_paint,_,_) :-
	vagents_paint.

vagent_picker(vagents,msg_focus,_,_) :-
	vagents_paint.

vagents_paint :-
	vagent_icons(List),
	forall(member(Icon,List,Position),
		(
		 X is ((Position - 1) mod 5) * 36 + 2,
		 Y is (Position - 1)//5 * 36 + 2,
		 gfx_begin((vagents,1)),
		 gfx(icon(X,Y,Icon)),
		 gfx_end((vagents,1))
		)).

vshow_initial_world :-
	gfx_begin((vworld,1)),
	gfx(bitmap(0,0,800,600,3,2,start)),
	gfx_end((vworld,1)).

vmenu(`&Leap`,_) :-
	switch(a,0),	
% No steps taken yet this run.
	vrun.  

vmenu(`&Creep`,_) :-
 	assert(vstop),
 	vrun.  

vmenu(`&Abort`,_) :-
	(
	 \+ vcurrent(state,running)
	;
	 assert(vstop)
	).  

vmenu(`Change Agent Icon`,_) :-
 	vpick_agent.  

vmenu(`Change Agent Location`,_) :-
 	assert(vrelocate_agent).  

vmenu(`Reset Agent Strength`,_) :-
 	retract(vagent_data(X,Y,_,D,L)),
 	assert(vagent_data(X,Y,1000,D,L)),
 	vdraw(X,Y,a),
 	vstatus_report,
 	!,
 	vwell_and_able(_,_,_,_), 	
	vmenu_status(`&Leap`,1),
	vmenu_status(`&Creep`,1),
	vmenu_status(`&Abort`,1),
	vmenu_status(`Change Agent Icon`,1),
	vmenu_status(`Change Agent Location`,1),
	vmenu_status(`Manual Testing Mode`,1).

vmenu(`Reset Agent Damage`,_) :-
 	retract(vagent_data(X,Y,S,D,L)),
 	assert(vagent_data(X,Y,S,0,L)),
 	vdraw(X,Y,a),
 	vstatus_report,
 	!,
 	vwell_and_able(_,_,_,_), 	
	vmenu_status(`&Leap`,1),
	vmenu_status(`&Creep`,1),
	vmenu_status(`&Abort`,1),
	vmenu_status(`Change Agent Icon`,1),
	vmenu_status(`Change Agent Location`,1),
	vmenu_status(`Manual Testing Mode`,1).

vmenu(`Agent View Mode`,_) :-
 	wmnusel(voptions,4,Test),
 	Test = 1,
 	wmnusel(voptions,4,0),
	retractall(vcurrent(view,_)),
	vcolor_world.  

vmenu(`Agent View Mode`,_) :-
 	wmnusel(voptions,4,1),
 	retractall(vcurrent(view,_)),
	assert(vcurrent(view,agent)),
	vcolor_world.  

vmenu(`Manual Testing Mode`,_) :-
 	wmnusel(voptions,5,Test),
 	Test = 1,
 	wmnusel(voptions,5,0).  

vmenu(`Manual Testing Mode`,_) :-
 	wmnusel(voptions,5,1),
 	wfocus((vworld,1)).

vmenu(`Quick Time`,_) :-
	wmnusel(voptions,6,Status),
	(
	 Status = 0,
	 wmnusel(voptions,6,1)
	;
	 wmnusel(voptions,6,0)
	).  

vmenu(`V-World Basics`,_) :-
 	chdir(Dir),cat([Dir,'\',
	'v-world.htm'],File,_),
	open_file(File).

% open_file(+File) finds the program associated with files of the
% type File, then opens File using that program. File should include
% the complete path name to the file to be opened.

open_file(File) :-
	atom_string(File,FileString),
	fcreate(temp,[],-2,260,0),
	winapi(('shell32.dll','FindExecutableA'),[FileString,``,temp],0,_),
	wintxt(temp,0,0,AssociatedProgramString),
	atom_string(AssociatedProgram,AssociatedProgramString),
	exec(AssociatedProgram,File,_).

vmenu(`Building Agents`,_) :-
	chdir(Dir),cat([Dir,'\','build_agents.htm'],File,_),
	open_file(File).

vmenu(`Building Worlds`,_) :-
	chdir(Dir),cat([Dir,'\','build_worlds.htm'],File,_),
	open_file(File).

vmenu(`About`,_) :-
 	msgbox(`About VWORLD`,
 `VWORLD version 11/11/2003~J~M~J~MCopyright 2003 Donald Nute~J~MArtificial Intelligence Center~J~MThe University of Georgia`,
 	0,_).

% vinit_menus(+Position,+Number,+NestedListOfMenuData) uses the
% NestedListOfMenuData to add a series of menus to the Window beginning
% with an initial Position and an initial Number for the first menu.

vinit_menus(_,_,[]).

vinit_menus(Pos,Num,[menu(Status,Item)|Rest]) :-
	vmnuadd(Pos,Item,Num),
	vmenu_status(Item,Status),
	NewPos is Pos + 1,
	NewNum is Num + 100,
	!,
	vinit_menus(NewPos,NewNum,Rest).

vinit_menus(Pos,Num,[menu(Menu,Status,Item,SubMenus)|Rest]) :-
	wmcreate(Menu),
	vmnuadd(Pos,Item,Menu),
	vmenu_status(Item,Status),
	SubNum is Num + 1,
	vinit_submenus(Menu,0,SubNum,SubMenus),
	NewPos is Pos + 1,
	NewNum is Num + 100,
	!,
	vinit_menus(NewPos,NewNum,Rest).

vinit_submenus(_,_,_,[]).

vinit_submenus(Menu,Pos,Num,[_,``|Rest]) :-
	wmnuadd(Menu,Pos,``,0),
	NewPos is Pos + 1,
	NewNum is Num + 1,
	!,
	vinit_submenus(Menu,NewPos,NewNum,Rest).

vinit_submenus(Menu,Pos,Num,[Status,Item|Rest]) :-
	wmnuadd(Menu,Pos,Item,Num),
	wmnunbl(Menu,Pos,Status),
	NewPos is Pos + 1,
	NewNum is Num + 1,
	!,
	vinit_submenus(Menu,NewPos,NewNum,Rest).

% vcreate_menu_bar creates a menu bar for vworld.

vcreate_menu_bar :-
   wndhdl( vworld, Handle ),
   vclose_menu_bar,
   winapi( (user32,'CreateMenu'), [], 0, MBARHandle ),
   MBARHandle \= 0,
   winapi( (user32,'SetMenu'), [Handle,MBARHandle], 0, R ),
   R \= 0.

% vclose_menu_bar removes the menu bar from vworld.

vclose_menu_bar :-
	wndhdl( vworld, Handle ),
	winapi( (user32,'DestroyMenu'), [Handle], 0, _ ).

vclose_menu_bar.

% vmnuhndl(-WinHandle,-MBARHandle) gets handles for the vworld
% window and its menubar.

vmnuhndl(WinHandle,MBARHandle) :-
	wndhdl(vworld,WinHandle),
	winapi((user32,'GetMenu'),[WinHandle],0,MBARHandle).

% vmnuadd(+Pos,+Menu,+Number) places Menu on the vworld
% menubar at location Pos using the supplied menu Number.

vmnuadd(Pos,Menu,Number) :-
	vmnuhndl(Handle,MBAR),
	wmnuadd(menu(MBAR),Pos,Menu,Number),
	winapi((user32,'DrawMenuBar'),[Handle],0,_).

% vmenu_item_name(+Num,-Item) returns the Item
% name for a menu Item that has been assigned a menu
% Num by init_screen/1 using information in a 
% vscreen_data/3 clause.

vmenu_item_name(Num,Item) :-
	vscreen_data(Menus),
	X is (Num - 1000)//100,
	Z is 1000 + (100 * X),
	W is X + 1,
	(
	 Num = Z,
	 member(menu(_,Item),Menus,W)
	;
	 Y is 2 * (Num - 1000 - (100 * X)),
	 member(menu(_,_,_,List),Menus,W),
	 member(Item,List,Y)
	).

% vmenu_status(+Item,?Status) will get or
% or change the status for a menu Item.

vmenu_status(Item,Status) :-
	vscreen_data(Menus),
	member(menu(Menu,_,_,List),Menus),
	member(Item,List,N),
	Pos is (N / 2) - 1,
	wmnunbl(Menu,Pos,Status),
	wndhdl( vworld, Handle ),
	winapi((user32,'DrawMenuBar'),[Handle],0,_).

% If a menu is not present, do nothing.

vmenu_status(_,_).

% Various window handlers will call vmenu_action(+Win,+Num,-Response)
% when they receive a msg_menu message. m_action/3 will be described 
% for each dialog.

vmenu_action(Win,Num,Response) :-
	vmenu_item_name(Win,Num,Item),
	vmenu(Win,Item,Response).

% BASIC ENTITIES AND THEIR BEHAVIORS ARE DEFINED BELOW.

vset_basic_icons :-
	forall(member((Object,Icon),
		[(o,empty),		% Empty space 
		 (o(_),empty),	% Empty space 
		 (w,wall),		% Solid wall 
		 (door,door),
		 (door(_),door),
		 (cant_see,dark)]),
		(
		 vobject_icon(Object,Icon)
		;
		 assert(vobject_icon(Object,Icon))
		)),
	forall(member(X,
		[
		 cross,
		 grave,
		 princess,
		 witch,
		 hornet,
		 snail,
		 bird,
		 dragon,
		 troll,
		 bumble,
		 tree,
		 thorn,
		 fruit,
		 birdseed,
		 flower,
		 sword,
		 shield,
		 gold,
		 throne,
		 bkey,
		 blublock,
		 blukey,
		 yblock,
		 ykey,
		 gblock,
		 gkey,
		 pblock,
		 pkey,
		 bugspray,
		 water]),
		(
		 vobject_icon(X,X)
		;
		 assert(vobject_icon(X,X))
		)).

% Fruit is consumable. Eating fruit increases strength and 
% moving into empty space uses strength.

vconsumable(fruit).
veff(fruit,10,0).
veff(o,-1,0). 

% Pressing against a throne uses a little strength.

veff(throne,-1,0).

% Pressing against walls or movable boxes uses more strength.

veff(w,-2,0).
veff(door,-2,0).
veff(blublock,-2,0).
veff(yblock,-2,0).
veff(gblock,-2,0).
veff(pblock,-2,0).

vmovable(blublock).
vmovable(yblock).
vmovable(gblock).
vmovable(pblock).

% Several basic entities are collectable or multicollectable.

vcollectable(bkey).
vcollectable(blukey).
vcollectable(gkey).
vcollectable(ykey).
vcollectable(pkey).
vcollectable(sword).
vcollectable(shield).
vcollectable(bugspray).
vcollectable(bird).
vmulticollectable(gold).
vcollectable(birdseed).
vcollectable(flower).

% Doors can take an argument that actors can use to tell
% them apart. Going through a door requires the same amount
% of energy as moving into an empty space.

veff(p,-1,0). 
veff(p(_),-1,0). 

% A red cross completely heals the agent when pushed.

veff(cross,-1,-100).

% Fruit is nourishing.

veff(fruit,10,0). 

% If you push or brush against a thorn, it will hurt you 
% unless you have the shield.  

vanimate(thorn). 

veff(thorn,-1,Damage) :-
	one((
		 vfind_in_inventory(shield),
		 Damage = 0
		;
		 Damage = 10
	   )).  

vact(thorn,X,Y) :-
	\+ vfind_in_inventory(shield),
 	vlocate(X,Y,a,_,_),
 	vinjure(2),
 	vstatus_report.  

vact(thorn,_,_). 

% Trees drop fruit when shaken.  

veff(tree,-2,0).

vreact(tree,X,Y) :-
 	vrandom_adjacent_location(X,Y,XX,YY),
	vinspect(current,XX,YY,o),
 	vdraw(XX,YY,fruit).

vreact(tree,_,_).

% HORNETS  
% Hornets will sting you mildly if given a chance.  
% If you push them, they will sting you badly. 
% If you spray them with bugspray they will disappear. 
% 
% Hornets can be set to guard something. Otherwise,
% they will move at random.  

vanimate(hornet). 

veff(hornet,-1,25).
 
vreact(hornet,X,Y) :-
 	vfind_in_inventory(bugspray),
 	vdraw(X,Y,o).  

vact(hornet,X,Y) :-
 	vlocate(X,Y,a,_,_),
 	!,
 	vinjure(3),
 	vstatus_report.

vact(hornet,X,Y) :-
	vpatrol(hornet,X,Y).

vact(hornet,X,Y) :-
 	vmove_at_random(hornet,X,Y).

% SNAILS
%
% Snails follow you.
%
% If you push a snail, it will stop moving for one turn.
% Otherwise, a snail will move at random.

vanimate(snail). 

veff(snail,-1,0). 

vreact(snail,X,Y) :-
	assert(vfreeze(snail,X,Y)),
	assert(vfreeze(snail,X,Y)).

vact(snail,X,Y) :-
	retract(vfreeze(snail,X,Y)). 

vact(snail,X,Y) :-
	vfollow(snail,a,X,Y).

vact(snail,X,Y) :-
	vmove_at_random(snail,X,Y).

% THE BIRD
%
% The bird will avoid you unless you have what it wants
% By default, it wants birdseed. To change this,
% put vwants(bird,?Object) in the world file.
% The bird will also avoid you if you have something in
% your inventory that it fears. To make the bird fear
% something, put vfears(bird,?Object) in the world file.
% Otherwise, the bird moves at random.
%
% If you manage to catch the bird but you don't have
% what it wants, the bird will escape.
%
% You can make catching the bird the condition for
% ending the game by putting vgoal_to_catch_bird in
% your world file. You can only hold one bird at
% a time.

vanimate(bird).

vcollectable(bird).

vcollectable(birdseed).

vavoid(bird,a).

veff(bird,-1,0).

vact(bird,X,Y) :-
	one((
		 vwants(bird,Object)
		;
		 Object = birdseed
	   )),
	\+ vfind_in_inventory(Object),
	vescape(bird,X,Y).

vact(bird,X,Y) :-
	vmove_at_random(bird,X,Y).

vnature :-
	one((	vwants(bird,_),
		sndmsg((vworld,400),lb_findstringexact,0,`bird`,Pos1),
		sndmsg((vworld,400),lb_findstringexact,Pos1,`bird`,Pos2),
		Pos1 \= Pos2,
		vagent_data(A,B,C,D,E),
		vdrop(bird),
% Must reset vagent_data to reflect the situation before the
% bird escaped.
		retractall(vagent_data(_,_,_,_,_)),
		assert(vagent_data(A,B,C,D,E))
	   )),
	fail.


vnature :-
	one((	vwants(bird,Object),
		\+ vfind_in_inventory(Object),
		vfind_in_inventory(bird),
		vagent_data(A,B,C,D,E),
		vdrop(bird),
% Must reset vagent_data to reflect the situation before the
% bird escaped.
		retractall(vagent_data(_,_,_,_,_)),
		assert(vagent_data(A,B,C,D,E))
	   )),
	fail.

vnature :-
	one((	vfears(bird,Object),
		vfind_in_inventory(bird),
		vfind_in_inventory(Object),
		vagent_data(A,B,C,D,E),
		vdrop(bird),
% Must reset vagent_data to reflect the situation before the
% bird escaped.
		retractall(vagent_data(_,_,_,_,_)),
		assert(vagent_data(A,B,C,D,E))
	   )),
	fail.

vnature :-
	one((
		vgoal_to_catch_bird,
		vfind_in_inventory(birdseed),
		vfind_in_inventory(bird),
		\+ (
			vfears(bird,Object),
			vfind_in_inventory(Object)
	 	  ),
	msgbox(`A voice from nowhere says:`,
 		`Well, you finally caught the little fellow!`,0,_),
	assert(vgame_over)
	   )),
	fail.

% THE PRINCESS
%
% The princess will follow your agent if nothing is 
% guarding her.
% 
% She will move out of the way if pushed. 
%
% She will die if she comes into contact with anything 
% harmful (dragon, troll, hornet, thorn, etc.)
%
% In any game containing the princess, the game is over
% if she is killed or if she comes into contact with
% the throne.
%
% Nature helps the princess follow your agent from level 
% to level if she is in the world and the witch is gone.  

vanimate(princess).

vmovable(princess). 
 
veff(princess,-1,0). 

vact(princess,XX,YY) :-
	\+ vcurrent(princess,following),
	vlook(XX,YY,
 		[A,B,C,D,E],
 		[F,G,H,I,J],
 		[K,L,_,N,O],
 		[P,Q,R,S,T],
 		[U,V,W,X,Y]),
	member(a,[A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S,T,U,V,W,X,Y]),
	(
	 \+ vguard(_,princess)
	;
	 vguard(Somebody,princess),
	 \+ vfind_in_world(Somebody,_,_,_)
	),
	assert(vcurrent(princess,following)),
	fail.

vact(princess,_,_) :- 
	retract(vfreeze(princess)).

vact(princess,X,Y) :-
	vprincess_dead_or_rescued(X,Y).

vact(princess,X,Y) :-
	(
	 \+ vguard(_,princess)
	;
	 vguard(Somebody,princess),
	 \+ vfind_in_world(Somebody,_,_,_)
	),
	vfollow(princess,a,X,Y).

vact(princess,_,_). 

vreact(princess,_,_). 

vprincess_dead_or_rescued(X,Y) :-
 	vlook(X,Y,_,
		  [_,R22,R23,R24,_],
		  [_,R32,R33,R34,_],
		  [_,R42,R43,R44,_],
		  _),
	(
	 member(Actor,[R22,R23,R24,R32,R33,R34,R42,R43,R44]),
	 \+ member(Actor,[o,w,witch]),
	 (
	  veff(Actor,_,Dmg),
	  Dmg > 0
	 ;
	  member(Actor,[@])
	 ),
	 vdraw(X,Y,grave),
 	 vstatus_report,
 	 msgbox(`The princess says:`,`Alas! Villain, thou hast slain me!`,0,_)
	;
	 member(throne,[R22,R23,R24,R32,R33,R34,R42,R43,R44]),
 	 msgbox(`The princess says:`,
 		`Thank you, kind sir, for returning me to my throne!`,0,_)
	),
	retractall(vfreeze(princess)),
 	assert(vgame_over).

vnature :-
	vfind_in_world(princess,Level,XJ,YJ),
	one((\+ vgame_over,
 	     \+ vfind_in_world(witch,_,_,_),
 	     \+ vactor(current,[princess|_]),
 	     vwell_and_able(X,Y,_,_),
 	     vlocate(X,Y,o,XX,YY),
 	     vdraw(Level,XJ,YJ,o),
 	     vdraw(XX,YY,princess),
	     vact(princess,XX,YY)
	   )),
 	fail.  

% THE WITCH
%
% If pushed or struck, the witch fights back. If
% she sees something she wants, she will pick it
% up and disappear.  
%
% If the witch sees something she wants, she will take
% it and disappear.
%
% The witch can be set to guard something. Otherwise,
% she will move at random.  

vanimate(witch).

veff(witch,-1,0) :- 
	vfind_in_inventory(gold). 

veff(witch,-1,25).

vact(witch,X,Y) :-
	vwants(witch,Object),
	vtakes_and_runs(X,Y,Object).

vact(witch,X,Y) :-
	vpatrol(witch,X,Y),
	!.

vact(witch,X,Y) :-
	vmove_at_random(witch,X,Y).  

% vtakes_and_runs(+X,+Y,+Object) causes the actor at coordinate X,Y
% to take the Object if it sees it and then to disappear.

vtakes_and_runs(X,Y,Object) :-
	vrandom_adjacent_location(X,Y,XX,YY),
	vlocate(XX,YY,Object,XXX,YYY),
	vdraw(X,Y,o),
	vdraw(XXX,YYY,o),
 	vstatus_report.  

% THE TROLL  
%
% If you push or strike the troll, he will hurt you VERY  
% badly unless you have the shield.  
%
% The troll will disappear if you attack him with the sword. 
% If you stand beside the troll and you don't have the shield, 
% he will trounce you!  
%
% The troll can be set to guard something. Otherwise,
% he will move at random. 

vcollectable(sword). 

vcollectable(shield).  

vanimate(troll).
 
veff(troll,-1,X) :-
 	(
 	 vfind_in_inventory(shield) -> X is 0
 	;
 	 X is 50
 	).  

vreact(troll,X,Y) :-
 	vfind_in_inventory(sword),
 	vdraw(X,Y,o).  

vact(troll,X,Y) :-
 	vlocate(X,Y,a,_,_),
 	\+ vfind_in_inventory(shield),
 	vinjure(20),
 	vstatus_report.

vact(troll,X,Y) :-
	vpatrol(troll,X,Y).

vact(troll,X,Y) :-
	vmove_at_random(troll,X,Y).

% DRAGON 
%
% If you push or strike the dragon, he will kill you.
% 
% If you get close to the dragon, he will toast you! 
%
% If the dragon sees something it fears, the dragon will disappear.
%
% The dragon can be set to guard something. Otherwise,
% he will move at random. 

vanimate(dragon).  

vcollectable(flower). 

veff(dragon,-1,100).  

vact(dragon,X,Y) :-
	vfears(dragon,Object),
	vruns_from(X,Y,Object).

vact(dragon,X,Y) :-
 	vlocate(X,Y,a,_,_),
 	vinjure(50),
 	vstatus_report.

vact(dragon,X,Y) :-
	vpatrol(dragon,X,Y),
	!.  

vact(dragon,_,_).  

vact(dragon,X,Y) :-
	vmove_at_random(dragon,X,Y).

% vruns_from(+X,+Y,+Object) causes the actor at coordinates X,Y
% to disappear if it sees the Object it fears.

vruns_from(X,Y,Object) :-
	vrandom_adjacent_location(X,Y,XX,YY),
%	(
	 vdetect(XX,YY,Object),
%	;
%	 vdetect(XX,YY,a),
%	 vfind_in_inventory(Object)
%	),
	vdraw(X,Y,o).

% vagent_icons(+List) stores a list of available icons for the agent.  

vagent_icons([
	bumble,
	knight,
	george,
	billy,
	horace,
	lgm]).

% Below are operations that can be used to define behaviors for
% additional characters.

% vfind_in_inventory(+Item) succeeds only if Item is in the
% inventory listbox.

vfind_in_inventory(Item) :-
	vstring_term(IStr,Item),
	(
	 sndmsg((vworld,400),lb_findstringexact,0,IStr,Pos)
	;
	 vmulticollectable(Item),
	 vamount_in_inventory(Item,_)
	),
	Pos \== -1.

% vadd_to_inventory(+Item) places Item in the inventory list box. 

vadd_to_inventory(Item) :-
	vmulticollectable(Item),
	!,
	vamount_in_inventory(Item,N),
	M is N + 1,
	atom_string(Item,IStr),
	number_string(M,MStr),
	cat([IStr,`(`,MStr,`)`],NewString,_),
	wlbxadd((vworld,400),-1,NewString).	 

vadd_to_inventory(Item) :-
	vstring_term(IStr,Item),
	wlbxadd((vworld,400),-1,IStr).

% vamount_in_inventory(+Item,-N) returns the number N of Item
% currently in the inventory and deletes Item from inventory.

vamount_in_inventory(Item,N) :-
	(write(Item),write(`(`)) ~> IStr,
	sndmsg((vworld,400),lb_findstring,0,IStr,Pos),
	wlbxget((vworld,400),Pos,String),
	wlbxdel((vworld,400),Pos),
	len(IStr,L),
	len(String,LL),
	K is LL - L - 1,
	cat(List,String,[L,K]),
	List = [_,NStr|_],
	number_string(N,NStr),
	!.
		
vamount_in_inventory(_,0).

% vremove_from_inventory(+Atom,-Item) removes an Item named in
% Atom from the inventory list box if it is there.

vremove_from_inventory(Item,Item) :-
	vmulticollectable(Item),
	vamount_in_inventory(Item,N),
	M is N - 1,
	(
	 M = 0
	; 
	 atom_string(Item,IStr),
	 number_string(M,MStr),
	 cat([IStr,`(`,MStr,`)`],NewString,_),
	 wlbxadd((vworld,400),-1,NewString)
	),
	!.

vremove_from_inventory(Atom,Item) :-
	atom_string(Atom,String),		% In manual testing mode, a  
	cat([String,`.~M~J`],Input,_),	% multicollectable item has been
	read(Term) <~ Input,			% selected.
	Term =.. [Item,_],		
	vmulticollectable(Item),
	!,
	vremove_from_inventory(Item,Item).

vremove_from_inventory(Item,Item) :-
	vstring_term(IStr,Item),
	sndmsg((vworld,400),lb_findstringexact,0,IStr,Pos),
	Pos \== -1,
	wlbxdel((vworld,400),Pos).  

% vfollow(+Actor,+Object,+X,+Y) determines and implements the move of an
% Actor at coordinates (X,Y) who follows Object.

vfollow(Actor,Object,X,Y) :-
	vrandom_adjacent_location(X,Y,XX,YY),
	vinspect(current,XX,YY,o),
	vlocate(XX,YY,Object,_,_),
	!,
	vmove(Actor,X,Y,XX,YY).

% vpatrol(+Actor,+X,+Y) determines and implements the move of an Actor
% at coordinates (X,Y) who is assigned to guard an object specified in
% the clause vguard(Actor,Object). The Actor will move at random so long
% as it can keep the Object in sight.

vpatrol(Actor,X,Y) :-
	vguard(Actor,Object),
	vrandom_adjacent_location(X,Y,XX,YY),
	vinspect(current,XX,YY,o),
	vdetect(XX,YY,Object),
	!,
	vmove(Actor,X,Y,XX,YY).

% vescape(+Actor,+X,+Y) determines and implements the move of an Actor
% at coordinates (X,Y) who is trying to avoid another actor specified in
% the clause vavoid(Actor,OtherActor).

vescape(Actor,X,Y) :-
	vavoid(Actor,OtherActor),
	vmap_section(X,Y,_,[_,G,H,I,_],[_,L,M,N,_],[_,Q,R,S,_],_),
	away_from(OtherActor,[G,H,I,L,M,N,Q,R,S],Dir),
	vtransform(X,Y,Dir,XX,YY),
	vmove(Actor,X,Y,XX,YY).	

% vmove_at_random(+Actor,+X,+Y) will perform as you would expect.

vmove_at_random(Actor,X,Y) :-
	vrandom_adjacent_location(X,Y,XX,YY),
	vinspect(current,XX,YY,o),
	!,
	vmove(Actor,X,Y,XX,YY).

vmove_at_random(_,_,_).

% away_from(+Object,+Vicinity,-Direction) checks to see if an Object
% is in the immediate Vicinity. If it is, away_from returns a
% Direction that would take an agent or actor to an empty space 
% that is away from the Object.

away_from(Obj,[NW,N,NE,W,E,SW,S,SE],Dir) :-
	member(Obj,[NW,N,NE,W,E,SW,S,SE],Num1),
	member(BadDir,[nw,n,ne,w,e,sw,s,se],Num1),
	vopp(BadDir,List),
	member(Dir,List),
	member(Dir,[nw,n,ne,w,e,sw,s,se],Num2),
	member(o,[NW,N,NE,W,E,SW,S,SE],Num2).

vopp(n,[sw,s,se]).
vopp(e,[nw,w,sw]).
vopp(w,[se,e,ne]).
vopp(s,[ne,n,nw]).
vopp(se,[w,nw,n]).
vopp(sw,[n,ne,e]).
vopp(nw,[e,se,s]).
vopp(ne,[s,sw,w]).

/*
   Load WIN-PROLOG 4.700 Old-Style Icons - Brian D Steel - 08 Dec 09
   =================================================================

   In version 4.800 of WIN-PROLOG, the handling of icons was updated to support
   the multi-size, full-colour icons that are a fundamental part of Windows
   Vista and later, but some applications still need access to the old-style
   icons that can be imported from executable files an DLLs.

   This file provides a way to load old-style icons; gfx_icon_load_old/3 takes
   exactly the same arguments as the gfx_icon_load/3 did in WIN-PROLOG 4.700
   and earlier. To use the old style icons, simply append the characters "_old"
   to your icon-loading calls.
*/


% load an icon - bds 05 mar 97 / 08 dec 09

gfx_icon_load_old( Icon, File, Index ) :-
   (  type( Icon, 3 ),
      type( File, 3 ),
      type( Index, 1 ),
      dict( 1, Dict ),
      member( Pred, Dict ),
      cmp( 0, Pred, known_icon ),
      member( Vers, Dict ),
      cmp( 0, Vers, rgl_windows_nt )
   -> (  Pred( Icon, _, _ )
      -> gfx_icon_close( Icon )
      ;  true
      ),
      wndhdl( -1, Instance ),
      stratm( String, File ),
      (  Vers
      -> winapi( (shell32,'ExtractIconW'), [Instance,String,Index], 2, Data )
      ;  winapi( (shell32,'ExtractIconA'), [Instance,String,Index], 1, Data )
      ),
      (  (  cmp( 0, Data, 0 )
         ;  cmp( 0, Data, 1 )
         )
      -> throw( 10, gfx_icon_load_old(Icon,File,Index) )
      ;  addcls( [Pred(Icon,Data,32)], 0 )
      )
   ;  (  type( Icon, 0 )
      ;  type( File, 0 )
      ;  type( Index, 0 )
      )
   -> throw( 22, gfx_icon_load_old(Icon,File,Index) )
   ;  throw( 23, gfx_icon_load_old(Icon,File,Index) )
   ).


