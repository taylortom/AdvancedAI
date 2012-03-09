% MAPMAKER.PL
% 
% Copyright 2003 Donald Nute 
% Date: 5/29/2003 
% Last modified: 9/11/2003 
% 
% This program is used to create maps of worlds for the VWORLD program

:- dynamic [vagent_data/5, vmap/3, vobject_icon/2, veff/3, vact/3, vreact/3,
 	vcurrent/2, vstop/0, vrelocate_agent/0, vgame_over/0, vmovable/1,
	vcollectable/1, vconsumable/1, vanimate/1, vkey/7, vguard/2, basic_entity/3,
	user_entity/5, v_icon/3, vwants/2, vfears/2, vnature/1, vgoal_to_catch_bird/0,
	vmulticollectable/1, vfears/2, author/1, date_created/1, last_modified/1].   

:- multifile vobject_icon/2, veff/3, vconsumable/1, vcollectable/1, vmovable/1,
	vanimate/1, vkey/7, vguard/2, v_icon/3, vwants/2, vfears/2,	vnature/1,
	vgoal_to_catch_bird/0, vmulticollectable/1, vfears/2.  

% vstartup is the initial query when VWORLD.OVL is created.  

vstartup :-
	vmain(Goal),
	catch(Code,(Goal,!),Culprit),
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

mapmaker :-
	prolog_flag(unknown,_,fail),
	retractall(vcurrent(_,_)),
	retractall(basic_entity(_,_,_)),
	vload_icons,
 	vcreate_screen,
	reset_entity_properties,
 	show_dialog(vworld),
	set_icons(basic),
	repeat,
 	wait(0),
 	wshow(vworld,Status),
 	Status = 0.

% vcreate_screen creates the Virtual World dialog window.

vcreate_screen :-
 	wdcreate(vworld,`Virtual World Map Maker`,0,0,800,600,
 		[ws_popup,ws_sysmenu,ws_caption,ws_border,ws_visible]),
	vcreate_menu_bar,
	vscreen_data(Menus),
	vinit_menus(0,1000,Menus),
 	wccreate((vworld,1),grafix,``,2,36,706,514,
 		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,2),grafix,``,716,40,69,498,
 		[ws_child,ws_visible,ws_border]),
% Vertical line
	wccreate((vworld,102),static,``,540,0,2,35,
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,100),button,``,550,1,15,15,
		[ws_child,ws_visible,bs_checkbox]),
	wccreate((vworld,101),static,`Basic Icons`,570,1,60,14,
		[ws_child,ws_visible]),
	wccreate((vworld,200),button,``,550,17,15,15,
		[ws_child,ws_visible,bs_checkbox]),
	wccreate((vworld,201),static,`User Icons`,570,16,60,14,
		[ws_child,ws_visible]),
% Vertical line
	wccreate((vworld,32),static,``,640,0,2,35,
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,31),static,`Current Object:`,650,1,50,30,
		[ws_child,ws_visible,ss_left]),
	wccreate((vworld,30),static,`w`,702,10,40,20,
		[ws_child,ws_visible,ss_center]),
	wccreate((vworld,3),grafix,``,754,1,34,34,
		[ws_child,ws_visible,ws_border]),
% Vertical line
	wccreate((vworld,302),static,``,322,0,2,35,
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,600),button,``,332,1,34,34,	% Left Arrow
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,300),button,``,366,1,34,34,	% Up Arrow
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,301),static,`Level: `,408,10,38,20,
		[ws_child,ws_visible]),
	wccreate((vworld,40),static,``,444,10,20,20,
		[ws_child,ws_visible]),
	wccreate((vworld,400),button,``,466,1,34,34,	% Down Arrow
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,500),button,``,500,1,34,34,	% Right Arrow
		[ws_child,ws_visible,ws_border]),

% Vertical line
	wccreate((vworld,702),static,``,222,0,2,35,
		[ws_child,ws_visible,ws_border]),
	wccreate((vworld,700),button,``,232,9,15,15,
		[ws_child,ws_visible,bs_checkbox]),
	wccreate((vworld,701),static,`Eraser Mode`,249,9,70,20,
		[ws_child,ws_visible]),
	window_handler(vworld,v_hndl),
	paint_buttons.

paint_buttons :-
	gfx_begin((vworld,300)),
	gfx(icon(0,0,up)),
	gfx_end((vworld,300)),
	gfx_begin((vworld,400)),
	gfx(icon(0,0,down)),
	gfx_end((vworld,400)),
	gfx_begin((vworld,500)),
	gfx(icon(0,0,right)),
	gfx_end((vworld,500)),
	gfx_begin((vworld,600)),
	gfx(icon(0,0,left)),
	gfx_end((vworld,600)).

vscreen_data(
	[menu(vfile,1,`File`,
		[1,`New Map`,
 		 1,`Load Map`,
		 0,`Save Map`,
		 0,`Save Map As`,
		 0,``,
		 1,`Exit`]),
	 menu(vactions,1,`Actions`,
		[0,`Create Entity`,
		 0,`Assign Guard`,
		 0,`Install Door`,
		 0,`Locate Agent`
		]),
	 menu(vhelp,1,`Help`,
		[1,`V-World Basics`,
		 1,`Building Worlds`,
 		 0,``,
 		 1,`About`])]).  

% vload_icons loads the icons used to represent the objects 
% and actors in vworld. All icons are stored in the icon 
% library wvorld.nil.

vload_icons :-
	gfx_icon_load_old(bumble,'vworld.icl',28),
	retractall(basic_entity(_,_,_)),
	retractall(user_entity(_,_,_,_,_)),
	gfx_icon_load_old(empty,'vworld.exe',2),
	assert(basic_entity(0,o,empty)),
	gfx_icon_load_old(door,'vworld.exe',5),
	assert(basic_entity(0,door,door)),
	gfx_icon_load_old(up,'vworld.icl',31),
	assert(basic_entity(0,up,up)),
	gfx_icon_load_old(down,'vworld.icl',32),
	assert(basic_entity(0,down,down)),
	gfx_icon_load_old(left,'vworld.icl',33),
	assert(basic_entity(0,left,left)),
	gfx_icon_load_old(right,'vworld.icl',34),
	assert(basic_entity(0,right,right)),
	v_icon_list(List),
	vload_icons(1,List).

vload_icons(_,[]).

vload_icons(N,[Entity,Icon,File,Position|Rest]) :-
	gfx_icon_load_old(Icon,File,Position),
	assert(basic_entity(N,Entity,Icon)),
	M is N + 1,
	!,
	vload_icons(M,Rest). 

paint_icons :-
	wbtnsel((vworld,100),Status),
	Status = 1,
	forall(
	(
	 basic_entity(M,_,Icon),
	 M > 0
	),
	(
	 (
	  M/2 - int(M/2) > 0,
	  X = 1
	 ;
	  X = 34
	 ),
	 Y is 1+(int((M + 1)/2) - 1) * 33,
	 gfx_begin((vworld,2)),
	 gfx(icon(X,Y,Icon)),
	 gfx_end((vworld,2))
	)).

paint_icons :-
	wbtnsel((vworld,200),Status),
	Status = 1,
	forall(user_entity(M,_,_,_,Icon),
	(
	 (
	  M/2 - int(M/2) > 0,
	  X = 1
	 ;
	  X = 34
	 ),
	 Y is 1+(int((M + 1)/2) - 1) * 33,
	 gfx_begin((vworld,2)),
	 gfx(icon(X,Y,Icon)),
	 gfx_end((vworld,2))
	)).

paint_current_entity :-
	vcurrent(entity,E),
	(
	 basic_entity(_,E,Icon)
	;
	 user_entity(_,E,_,_,Icon)
	),
	gfx_begin((vworld,3)),
	gfx(icon(0,0,Icon)),
	gfx_end((vworld,3)).

v_icon_list([
	w,		wall,		'vworld.exe',		7,
	cross,	cross,	'vworld.exe',		4,
	princess,	princess,	'vworld.icl',		2,
	witch,	witch,	'vworld.icl',		3,
	hornet,	hornet,	'vworld.icl',		4,
 	snail,	snail,	'vworld.icl',		5,
 	dragon,	dragon,	'vworld.icl',		7,
 	troll,	troll,	'vworld.icl',		8,
	bird,		bird,		'vworld.icl',		6,
 	birdseed,	birdseed,	'vworld.icl',		25,
	tree,		tree,		'vworld.icl',		9,
 	fruit,	fruit,	'vworld.icl',		11,
	flower,	flower,	'vworld.icl',		27,
	thorn,	thorn,	'vworld.icl',		10,
 	sword,	sword,	'vworld.icl',		12,
	shield,	shield,	'vworld.icl',		13,
	gold,		gold,		'vworld.icl',		14,
	throne,	throne,	'vworld.icl',		15,
	bugspray,	bugspray,	'vworld.icl',		29,
	water,	water,	'vworld.icl',		30,
	blublock,	bblock,	'vworld.icl',		16,
	blukey,	blkey,	'vworld.icl',		17,
	yblock,	yblock,	'vworld.icl',		18,
	ykey,		ykey,		'vworld.icl',		19,
	gblock,	gblock,	'vworld.icl',		20,
	gkey,		gkey,		'vworld.icl',		21,
	pblock,	pblock,	'vworld.icl',		22,
	pkey,		pkey,		'vworld.icl',		23,
	bkey,		bkey,		'vworld.icl',		24
	]).

% reset_entity_properties deletes all information about which entities
% are animate, etc., and then asserts appropriate facts about the
% basic entities.

reset_entity_properties :-
	retractall(v_icon(_,_,_)),
	retractall(vobject_icon(_,_)),
	retractall(vmap(_,_,_)),
	retractall(vagent(_,_,_,_,_)),
	retractall(vanimate(_)),
	retractall(vconsumable(_)),
	retractall(vcollectable(_)),
	retractall(vmulticollectable(_)),
	retractall(vmovable(_)),
	retractall(vguard(_,_)),
	retractall(vkey(_,_,_,_,_,_,_)),
	retractall(vwants(_,_)),
	retractall(vfears(_,_)),
	retractall(vnature),
	retractall(vgoal_to_catch_bird),
	forall(member(E,[princess,witch,hornet,snail,bird,dragon,troll,thorn]),
		assert(vanimate(E))),
	forall(member(E,[fruit]),
		assert(vconsumable(E))),
	forall(member(E,[bird,birdseed,flower,sword,shield,bugspray,bkey,blukey,ykey,gkey,pkey]),
		assert(vcollectable(E))),
	forall(member(E,[gold]),
		assert(vmulticollectable(E))),
	forall(member(E,[princess,blublock,yblock,gblock,pblock]),
		assert(vmovable(E))).
 
% vdraw(+X,+Y,+Obj) places the symbol for Obj in the world map at 
% location X,Y in the current level and draws the icon for Obj 
% on the screen at the X,Y location.  

vdraw(X,Y,Obj) :-
	(
	 basic_entity(_,Obj,Icon)
	;
	 user_entity(_,Obj,_,_,Icon)
	;
	 Obj = a,
	 Icon = bumble
	),
 	vcurrent(level,Lev),
 	vdraw(Lev,X,Y,Obj),
 	XX is X * 32,
 	YY is Y * 32,
 	gfx_begin((vworld,1)),
 	gfx(icon(XX,YY,Icon)),
 	gfx_end((vworld,1)).  

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

vcolor_cells(N,K,[Obj|Rest]) :-
	(
	 basic_entity(_,Obj,Icon)
	;
	 user_entity(_,Obj,_,_,Icon)
	;
	 Obj = a,
	 Icon = bumble
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

% v_hndl/4 is the window handler for MAPMAKER.  

v_hndl(vworld,msg_close,_,R) :-
	vexit(R).

v_hndl(vworld,msg_close,_,_).

v_hndl((vworld,1),msg_paint,_,_) :-
	\+ vcurrent(level,_).

v_hndl((vworld,1),msg_leftup,(X,Y),_) :-
	wfocus((vworld,1)),
	vmnusel(vactions,1,Status),
	Status = 1,
	!,
	assign_guard(X,Y).

v_hndl((vworld,1),msg_leftup,(X,Y),_) :-
	vmnusel(vactions,2,Status),
	Status = 1,
	!,
	install_door(X,Y).

v_hndl((vworld,1),msg_leftup,(X,Y),_) :-
	vmnusel(vactions,3,Status),
	Status = 1,
	!,
	locate_agent(X,Y).

v_hndl((vworld,1),msg_leftup,(X,Y),_) :-
	change_map(X,Y).

assign_guard(X,Y) :-
	vcurrent(level,Lev),
	Col is (X//32) + 1,
	Row is Y//32,
	vmap(Lev,Row,List),
	member(E,List,Col),
	vstring_term(EString,E),
	(
	 \+ vcurrent(guard,_),
	 vguard(E,O),
	 vstring_term(OString,O),
	 cat([EString,` is already guarding `,OString,
		`. Try again or click Assign Guard `,
		 `on the Actions menu to cancel guard selection.`],Message,_)
	;
	 \+ vcurrent(guard,_),
	 vcurrent(level,Lev),
	 vanimate(E),
	 assert(vcurrent(guard,E)),
	 Message = ``
	;
	 \+ vcurrent(guard,_),
	 cat([`The entity you picked is not animate and cannot `,
		`guard anything. Try again or click Assign Guard `,
		`on the Actions menu to cancel guard selection.`],
		Message,_)
	;
	 vcurrent(guard,O),
	 vguard(G,E),
	 vstring_term(GString,G),
	 (
	  G = O,
	  cat([GString,` is already guarding `,EString,`.`],Message,_),
	  vmnusel(vactions,1,0),
	  retractall(vcurrent(guard,_))
	 ;
	  G \== O,
	  vstring_term(OString,O),
	  cat([OString,` cannot guard `,EString,`; `,
		 GString,` is already guarding `,EString,
		 `. An object can only have one kind of guard. `,
		 `Try again or click Assign Guard `,
		 `on the Actions menu to cancel guard selection.`],Message,_)
	 )
	;
	 E \== o,
	 E \== w,
	 retract(vcurrent(guard,G)),
	 assert(vguard(G,E)),
	 vmnusel(vactions,1,0),
	 vmenu_status(`Save Map`,1),
	 cat([G,' is now guarding ',E,'.'],Message,_)
	),
	(
	 Message = ``
	;
	 msgbox(`Assign Guard`,Message,0,_)
	).	

install_door(_,_) :-
	wfocus((vworld,1)),
	vcurrent(door,_),
	!,
	cat([ `You haven't finished installing your door. `,
		`Click the item in the icons window to the right `,
		`that will serve as the key for the new door. If no key `,
		`is required, click the red wall icon.`],
		Message,_),
	msgbox(`Install Door`,Message,0,_).

install_door(X,Y) :-
	wfocus((vworld,1)),
	vmnusel(vactions,2,Status),
	Status = 1,
	vcurrent(level,(LX,LY)),
	vmap((LX,LY),_,List),
	length(List,LC),
	LastCol is LC - 1,
	Col is (X//32),
	findall(R,vmap((LX,LY),R,_),Rows),
	length(Rows,LR),
	LastRow is LR - 1,
	Row is Y//32,
	!,
	(
	 Loc is Col + 1,
	 member(w,List,Loc),
	 (
	  Col = 0,
	  Row \== 0,
	  Row \== LastRow,
	  Dir = `west`,
	  LXX is LX	 - 1,
	  LYY = LY,
	  NewCol = LastCol,
	  NewRow = Row
	 ;
	  Col = LastCol,
	  Row \== 0,
	  Row \== LastRow,
	  Dir = `east`,
	  LXX is LX + 1,
	  LYY = LY,
	  NewCol = 0,
	  NewRow = Row
	 ;
	  Row = 0,
	  Col \== 0,
	  Col \== LastCol,
	  Dir = `north`,
	  LXX = LX,
	  LYY is LY - 1,
	  NewCol = Col,
	  NewRow = LastRow
	 ;
	  Row = LastRow,
	  Col \== 0,
	  Col \== LastCol,
	  Dir = `south`,
	  LXX = LX,
	  LYY is LY + 1,
	  NewCol = Col,
	  NewRow = 0
	 )
	;
	 cat([`Doors can only be installed in an outer wall of a level. `,
		`Doors cannot be in corners.`],Message0,_),
	 msgbox(`Illegal Door Location`,Message0,0,_),
	 wmnusel(vactions,2,0),
	 fail
	),
	(
	 vmap((LXX,LYY),_,_)
	;
	 cat([`So the door will not open into nothing, we `,
		`will create a new level to the `,
		Dir,
		`of the current level.`],Message1,_),
	 msgbox(`Install Door`,Message1,0,_),
	 forall(emap(RR,List2),
	 	assert(vmap((LXX,LYY),RR,List2))),
	 vmenu_status(`Save Map`,1)
	),
	!,
	vdraw(Col,Row,door),
	vdraw((LXX,LYY),NewCol,NewRow,door),
	assert(vcurrent(door,((LX,LY),Col,Row,(LXX,LYY),NewCol,NewRow))),
	cat([ `Now click the item in the icons window to the right `,
		`that will serve as the key for the new door. If no key `,
		`is required, click the red wall icon.`],
		Message2,_),
	msgbox(`Install Door`,Message2,0,_).

locate_agent(_,_) :-
	\+ vcurrent(level,(0,0)),
	 !,
	 msgbox(`Locate Agent`,
		`The agent must be located in the initial screen for the map.`,
		0,_).

locate_agent(X,Y) :-
	NewCol is X//32,
	NewRow is Y//32,
	vmap((0,0),NewRow,List1),
	Position is NewCol + 1,
	member(Obj,List1,Position),
	(
	 Obj = o
	;
	 Obj = o(_)
	),
	(
	 vmap((0,0),Row,List2),
	 member(a,List2,Z),
	 Col is Z - 1,
	 vdraw(Col,Row,o)
	;
	 true
	),
	vdraw(NewCol,NewRow,a),
	retractall(vagent_data(_,_,_,_,_)),
	assert(vagent_data(NewCol,NewRow,1000,0,same)),
	vmnusel(vactions,3,0),
	vmenu_status(`Save Map`,1),
	!.

locate_agent(X,Y) :-
	msgbox(`Locate Agent`,
		`The agent can only be located in an empty space.`,
	0,_).

change_map(X,Y) :-
	XX is X//32,
	YY is Y//32,
	vcurrent(level,Lev),
	vmap(Lev,YY,List),
	(
 	 wbtnsel((vworld,700),Status),
	 Status = 1,
	 vdraw(XX,YY,o),
	 retractall(vguard(E,_)),
	 retractall(vguard(_,E))
	;
	 Location is XX + 1,
	 member(o,List,Location),
	 vcurrent(entity,E),
	 vdraw(XX,YY,E)
	),
	vmenu_status(`Save Map`,1).

v_hndl((vworld,1),msg_rightup,(X,Y),_) :-
	vmap(_,_,_),
	vcurrent(level,Lev),
	Col is (X//32) + 1,
	Row is Y//32,
	vmap(Lev,Row,List),
	member(E,List,Col),
	edit_entity_properties(E).

edit_entity_properties(E) :-
	LabelStyle  = [ws_child,ws_visible],
	CheckStyle  = [ws_child,ws_visible,bs_checkbox],
	ButtonStyle = [ws_child,ws_visible,bs_pushbutton],
	(
	 basic_entity(_,E,_),
	 Type = `basic`
	;
	 Type = `user`
	),
	write(E) ~> Name,
	wdcreate(e_edit,`Entity Properties`,300,230,160,230,
		[ws_popup,ws_visible,ws_caption,ws_border]),
	cat([`Entity Name: `,Name],String1,_),
	wccreate((e_edit,1),static,String1,25,4,120,18,LabelStyle),
	cat([`Entity Type: `,Type],String2,_),
	wccreate((e_edit,2),static,String2,25,25,100,18,LabelStyle),
	wccreate((e_edit,30),button,``,40,45,15,15,CheckStyle),
	wccreate((e_edit,3),static,`animate`,57,45,100,18,LabelStyle),
	(
	 \+ vanimate(E)
	;
	 wbtnsel((e_edit,30),1)
	),
	wccreate((e_edit,40),button,``,40,65,15,15,CheckStyle),
	wccreate((e_edit,4),static,`consumable`,57,65,100,18,LabelStyle),
	(
	 \+ vconsumable(E)
	;
	 wbtnsel((e_edit,40),1)
	),
	wccreate((e_edit,50),button,``,40,85,15,15,CheckStyle),
	wccreate((e_edit,5),static,`collectable`,57,85,100,18,LabelStyle),
	(
	 \+ vcollectable(E)
	;
	 wbtnsel((e_edit,50),1)
	),
	wccreate((e_edit,550),button,``,40,105,15,15,CheckStyle),
	wccreate((e_edit,55),static,`multicollectable`,57,105,100,18,LabelStyle),
	(
	 \+ vmulticollectable(E)
	;
	 wbtnsel((e_edit,550),1)
	),
	wccreate((e_edit,60),button,``,40,125,15,15,CheckStyle),
	wccreate((e_edit,6),static,`movable`,57,125,100,18,LabelStyle),
	(
	 \+ vmovable(E)
	;
	 wbtnsel((e_edit,60),1)
	),
	(
	 vguard(Guard,E),
	 wccreate((e_edit,100),button,``,40,145,15,15,CheckStyle),
	 (write(`Guarded by `),write(Guard)) ~> Message,
	 wccreate((e_edit,101),static,Message,57,145,100,18,LabelStyle),
	 wbtnsel((e_edit,100),1)
	;
	 vguard(E,Item),
	 wccreate((e_edit,100),button,``,40,145,15,15,CheckStyle),
	 ((write('Guarding '),write(Item)) ~> Message),
	 wccreate((e_edit,101),static,Message,57,145,100,18,LabelStyle),
	 wbtnsel((e_edit,100),1)
	;
	 true
	),
	wccreate((e_edit,70),button,`Save`,26,165,50,20,ButtonStyle),
	wccreate((e_edit,80),button,`Cancel`,86,165,50,20,ButtonStyle),
	window_handler(e_edit,e_hndl),
	call_dialog(e_edit,_).

e_hndl((e_edit,70),msg_button,_,ok) :-
	save_entity_properties.

save_entity_properties :-
	wtext((e_edit,1),String),
	cat(List,String,[13]),
	List = [_,Name],
	vstring_term(Name,E),
	retractall(vanimate(E)),
	retractall(vconsumable(E)),
	retractall(vcollectable(E)),
	retractall(vmulticollectable(E)),
	retractall(vmovable(E)),
	wbtnsel((e_edit,30),Animate),
	wbtnsel((e_edit,40),Consumable),
	wbtnsel((e_edit,50),Collectable),
	wbtnsel((e_edit,550),Multicollectable),
	wbtnsel((e_edit,60),Movable),
	catch(Error,wbtnsel((e_edit,100),Status)),
	(
	 Error = 1
	;
	 Status = 1
	;
	 retractall(vguard(E,_)),
	 retractall(vguard(_,E))
	),
	(
	 Animate = 0
	;
	 assert(vanimate(E))
	),
	(
	 Consumable = 0
	;
	 assert(vconsumable(E))
	),
	(
	 Collectable = 0
	;
	 assert(vcollectable(E))
	),
	(
	 Multicollectable = 0
	;
	 assert(vmulticollectable(E))
	),
	(
	 Movable = 0
	;
	 assert(vmovable(E))
	),
	vmenu_status(`Save Map`,1).

e_hndl((e_edit,80),msg_button,_,ok) :-
	stop.

stop.

e_hndl((e_edit,100),msg_button,_,_) :-
	wbtnsel((e_edit,100),Status),
	(
	 Status = 0,
	 wbtnsel((e_edit,100),1)
	;
	 wbtnsel((e_edit,100),0)
	).

e_hndl((e_edit,Button),msg_button,_,_) :-
	member(Button,[30,40,50,60]),
	toggle_button((e_edit,Button)).

toggle_button(Button) :-
	wtext((e_edit,2),Type),
	(
	 Type = `Entity Type: basic`
	;
	 wbtnsel(Button,Status),
	 Status = 0,
	 wbtnsel(Button,1)
	;
	 wbtnsel(Button,0)
	).

v_hndl((vworld,1),msg_paint,_,_) :-
 	vcolor_world.

v_hndl((vworld,Button),msg_paint,_,_) :-
	member(Button,[300,400,500,600]),
	paint_buttons.

v_hndl((vworld,Arrow),msg_button,_,_) :-
	member(Arrow,[300,400,500,600]),
	change_level(Arrow).

change_level(Arrow) :-
	vcurrent(level,(X,Y)),
	(
	 Arrow = 300,	% move level up
	 XX = X,
	 YY is Y - 1,
	 Dir = `north`
	;
	 Arrow = 400,	% move level down
	 XX = X,
	 YY is Y + 1,
	 Dir = `south`
	;
	 Arrow = 500,	% move level right
	 XX is X + 1,
	 YY = Y,
	 Dir = `east`
	;
	 Arrow = 600,	% move level left
	 XX is X - 1,
	 YY = Y,
	 Dir = `west`
	),
	(
	 vmap((XX,YY),_,_)
	;
	 cat([`Do you want to create a new level `,
		Dir,
		` of the current level?`],Message,_),
	 msgbox(`New Level`,Message,4,Result),
	 Result = 6,
	 forall(emap(R,List),
	 	assert(vmap((XX,YY),R,List))),
	 vmenu_status(`Save Map`,1)
	),
	!,
	retractall(vcurrent(level,_)),
	assert(vcurrent(level,(XX,YY))),
	write((XX,YY)) ~> S,
	wtext((vworld,40),S),
	vcolor_world.

change_level(_).

v_hndl((vworld,700),msg_button,_,_) :-
	wbtnsel((vworld,700),Status),
	(
	 Status = 0,
	 wbtnsel((vworld,700),1)
	;
	 wbtnsel((vworld,700),0)
	).

v_hndl(vworld,msg_menu,N,R) :-
	vmenu_item_name(N,Menu),
	vmenu(Menu,R).

v_hndl((vworld,2),msg_paint,_,_) :-
	paint_icons.

v_hndl((vworld,2),msg_leftup,(X,Y),_) :-
	vcurrent(door,_),
	!,
	get_key(X,Y).

get_key(X,Y) :-
	get_entity(X,Y,E),
	write(E) ~> String,
	(
	 E = w,
	 Message = `Your new door will be unlocked.`,
	 Key = none
	;
	 vcollectable(E),
	 cat([`Your new door will be locked and you `,
		`will only be able to pass through it if `,
		`you have `,
		String,
		` in your inventory`],Message,_),
	 Key = E
	;
	 cat([`A key has to be something you can pick up `,
		`and take with you. `,
		String,
		` is not collectable. Pick another key.`],
		Message,_),
	 Key = null
	),
	(
	 Key = null
	;
	 retract(vcurrent(door,(Lev1,Col1,Row1,Lev2,Col2,Row2))),
	 assert(vkey(Key,Lev1,Col1,Row1,Lev2,Col2,Row2)),
	 wmnusel(vactions,2,0),
	 msgbox(`Install Door`,Message,0,_),
	 vmenu_status(`Save Map`,1)
	).
	 
v_hndl((vworld,2),msg_leftup,(X,Y),_) :-
	get_entity(X,Y,E),
	write(E) ~> S,
	wtext((vworld,30),S),
	retractall(vcurrent(entity,_)),
	assert(vcurrent(entity,E)),
	wccreate((vworld,3),grafix,``,754,1,34,34,
		[ws_child,ws_visible,ws_border]),
	paint_current_entity.

v_hndl((vworld,2),msg_rightup,(X,Y),_) :-
	get_entity(X,Y,E),
	edit_entity_properties(E).

v_hndl((vworld,3),msg_paint,_,_) :-
	paint_current_entity.

v_hndl((vworld,100),msg_button,_,_) :-
	set_icons(basic).

v_hndl((vworld,200),msg_button,_,_) :-
	set_icons(user).

get_entity(X,Y,E) :-
	Row is Y//33,
	(
	 X < 34,
	 Col = 0
	;
	 Col = 1
	),
	M is 1 + (2 * Row) + Col,
	!,
	(
	 wbtnsel((vworld,100),Status),
	 Status = 1,
	 basic_entity(M,E,Icon)
	;
	 wbtnsel((vworld,200),Status),
	 Status = 1,
	 user_entity(M,E,_,_,Icon)
	).

% Any message not handled by the above clauses are 
% passed to the standard LPA window handler.  

v_hndl(Window,Message,Data,Result) :-
 	window_handler(Window,Message,Data,Result).  

vmenu(`New Map`,_) :-
	save_map_if_needed,
	new_map.

new_map :-
	user_input(`New Map`,`Enter a name for the new map.`,String),
	String \== `cancel`,
	String \== ``,
	user_input(`New Map`,`Enter the author's name for the new map.`,Author),
	Author \== `cancel`,
	len(String,L),
	(
	 M is L - 1,
	 integer_bound(1,N,M),
	 cat(List,String,[N,1]),
	 List = [MapString,`.`|_]
	;
	 MapString = String
	),
	atom_string(Map,MapString),
	retractall(vcurrent(_,_)),
	retractall(vobject_icon(_,_)), 
	retractall(veff(_,_,_)), 
	retractall(vconsumable(_)), 
	retractall(vcollectable(_)), 
	retractall(vmovable(_)), 
	retractall(vanimate(_)), 
	retractall(vkey(_,_,_,_,_,_,_)), 
	retractall(vguard(_,_)), 
	retractall(v_icon(_,_,_)), 
	retractall(vwants(_,_)), 
	retractall(vfears(_,_)), 
	retractall(vgoal_to_catch_bird), 
	retractall(vmulticollectable(_)), 
	retractall(vfears(_,_)),
	assert(vcurrent(map,Map)),
	retractall(author(_)),
	assert(author(Author)),
	time(1,Time),
	Time = (Days,_),
	time(Days,Year,Month,Day),
	(write(Month),write(`/`),write(Day),write(`/`),write(Year)) ~> Created,
	retractall(date_created(_)),
	assert(date_created(Created)),
	retractall(last_modified(_)),
	assert(last_modified(Created)),
	cat([`Virtual World - `,MapString],Caption,_),
	wtext(vworld,Caption),
	retractall(vcurrent(level,_)),
	reset_entity_properties,
	assert(vcurrent(level,(0,0))),
	wtext((vworld,40),`0,0`),
	set_icons(basic),
	retractall(vmap(_,_,_)),
	forall(emap(Row,List),
		assert(vmap((0,0),Row,List))),
	vcolor_world,
	vmenu_status(`Save Map`,1),
	vmenu_status(`Save Map As`,1),
	vmenu_status(`Create Entity`,1),
	vmenu_status(`Assign Guard`,1),
	vmenu_status(`Install Door`,1),
	vmenu_status(`Locate Agent`,1).

set_icons(basic) :-
	wbtnsel((vworld,100),1),
	wbtnsel((vworld,200),0),
	wccreate((vworld,2),grafix,``,716,40,69,498,
 		[ws_child,ws_visible,ws_border]),
	paint_icons,
	retractall(vcurrent(entity,_)),
	assert(vcurrent(entity,w)),
	wtext((vworld,30),`w`),
	wccreate((vworld,3),grafix,``,754,1,34,34,
		[ws_child,ws_visible,ws_border]),
	paint_current_entity.
	
set_icons(user) :-
	user_entity(1,E,_,_,_),
	wbtnsel((vworld,200),1),
	wbtnsel((vworld,100),0),
	wccreate((vworld,2),grafix,``,716,40,69,498,
 		[ws_child,ws_visible,ws_border]),
	paint_icons,
	retractall(vcurrent(entity,_)),
	assert(vcurrent(entity,E)),
	vstring_term(String,E),
	wtext((vworld,30),String),
	wccreate((vworld,3),grafix,``,754,1,34,34,
		[ws_child,ws_visible,ws_border]),
	paint_current_entity.

set_icons(user) :-
	msgbox(`User Icons`,`No user icons have been defined.`,0,_).

vmenu(`Save Map`,_) :-
	save_map.

save_map :-
	vmenu_status(`Save Map`,Status),
	Status = 0.

save_map :-
	vcurrent(map,Map),
	chdir(Dir),
	cat([Dir,'\',Map,'.map'],File,_),
	write_map(File),
	vmenu_status(`Save Map`,0).

vmenu(`Save Map As`,_) :-
	savbox(`Save Map`,[(`VWORLD map files: *.map`,`*.map`)],``,`map`,[Map|_]),
	write_map(Map).

write_map(Map) :-
	open(Map,write),
	tell(Map),
	fname(Map,_,File,Ext),
	cat([File,Ext],Name,_),
	write(`% `),write(Name),nl,
	write(`% A Virtual World`),nl,
	write(`author('`),
	author(Author),
	write(Author),
	write(`').`),nl,
	write(`date_created('`),
	date_created(Created),
	write(Created),
	write(`').`),nl,
	write(`last_modified('`),
	last_modified(Modified),
	write(Modified),
	write(`').`),nl,nl,
	write(`:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,`),
	nl,
	write(`            vmovable/1,vcollectable/1,vmulticollectable/1,vconsumable/1,vanimate/1,vguard/2].`),
	nl,nl,
	write(`:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,`),
	nl,
	write(`             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2, vwants/2, vfears/2,`),
	nl,
	write(`             vdrop_effect/2.`),
	nl,nl,
	(
	 vagent_data(AX,AY,_,_,_),
	 vdraw((0,0),AX,AY,o),
	 listing(vagent_data/5)
	;
	 true
	),
	findall(UE,(user_entity(_,UE,_,_,_),vmap(_,_,List),member(UE,List)),EList),
	(
	 EList = []
	;
	 setof(UE,member(UE,EList),UserEntities),
	 forall(
		(member(E,UserEntities),
		 user_entity(_,E,IconFile,Position,Icon)
		),
		(
		 write(`v_icon('`),
		 write(Icon),
		 write(`','`),
		 write(IconFile),
		 write(`',`),
		 write(Position),
		 write(`).`),
		 nl,nl,
		 write(`vobject_icon(`),
		 write(E),
		 write(`,'`),
		 write(Icon),
		 write(`').`),
		 nl,nl,
		 (
		  \+ vanimate(E)
		 ;
		  write(vanimate(E)),
		  write(`.`),
		  nl,nl
		 ),
		 (
		  \+ vconsumable(E)
		 ;
		  write(vconsumable(E)),
		  write(`.`),
		  nl,nl
		 ),
		 (
		  \+ vcollectable(E)
		 ;
		  write(vcollectable(E)),
		  write(`.`),
		  nl,nl
		 ),
		 (
		  \+ vmulticollectable(E)
		 ;
		  write(vmulticollectable(E)),
		  write(`.`),
		  nl,nl
		 ),
		 (
		  \+ vmovable(E)
		 ;
		  write(vmovable(E)),
		  write(`.`),
		  nl,nl
		 )
		))
	),
	listing(vguard/2),
	listing(vkey/7),
	listing(vwants/2),
	listing(vfears/2),
	listing(vnature/1),
	listing(vgoal_to_catch_bird/0),
	setof(Lev,X^Y^vmap(Lev,X,Y),Levels),
	forall(member(Level,Levels),
		(
		 setof(R,Y^vmap(Level,R,Y),Rows),
		 forall(member(Row,Rows),
			(
			 vmap(Level,Row,List),
			 write(`vmap((`),
			 write(Level),
			 write(`),`),
			 write(Row),
			 write(`,`),
			 write(List),
			 write(`).`),
			 nl
			)),
		 nl
		)),
	(
	 vagent(AX,AY,_,_,_),
	 vdraw((0,0),AX,AY,a)
	;
	 true
	),
	told,
	vmenu_status(`Save Map`,0).
		
save_map.

vmenu(`Load Map`,_) :-
	load_map.

load_map :-
	save_map_if_needed,
	opnbox(`Open Map`,[(`VWORLD map files: *.map`,`*.map`),
		(`VWORLDS : *.vw`,`*.vw`)],``,`map`,[Map|_]),
	retractall(vcurrent(_,_)),
	fname(Map,_,Name,Ext),
	(
	 Ext \== '.vw'
	;
	 cat([`The file you selected is a VWORLD file with extension .vw. `,
		`VWORLD files contain VWORLD maps, but they may `,
		`also contain other information. After editing, `,
		`the map in this file will be saved as a VWORLD Map file `,
		`with extension .map. It will contain your revised VWORLD `,
		`map, but it will not contain any of the other information `,
		`in your original VWORLD file. To use the revised map in your `,
		`VWORLD file, you must copy it from the .map file into your `,
		`.vw file.`],Message,_),
	 msgbox(`Load VWORLD File`,Message,0,_)
	),
	assert(vcurrent(map,Name)),
	reset_entity_properties,
	assert(vcurrent(level(0,0))),
	!,
	consult(Map),
	wccreate((vworld,1),grafix,``,2,36,706,514,
 		[ws_child,ws_visible,ws_border]),
	verify_world_loaded.

verify_world_loaded :-
	\+ vmap(_,_,_),
	msgbox(`Error Loading Map File`,
		`The file you selected does not include a VWORLD map.`,0,_),
	!.

verify_world_loaded :-
	forall(v_icon(Icon,File,Index),
		(
		 \+ basic_entity(_,_,Icon),
		 gfx_icon_load_old(Icon,File,Index)
		;
		 atom_string(Icon,IconString),
		 cat([`Your file contains a definition for the icon name `,
		      IconString,
			`. This is reserved as the name of a basic icon `,
			`in VWORLD and cannot be used as a user icon name. `,
			`The basic VWORLD icon will be used for entities you`,
			`have associated with this icon name.`],
			Message,_),
		 msgbox(`Illegal Icon Name`,Message,0,_)
		)),
	findall((E,I),vobject_icon(E,I),EList),
	forall(member((E,I),EList,M),
		((
		  \+ basic_entity(_,E,_),
		  \+ user_entity(_,E,_,_,_),
		  v_icon(I,File,Index),
		  assert(user_entity(M,E,File,Index,I))
		 ;
		  write(E) ~> EString,
		  cat([`You are trying to assign more than one icon to `,
			 `the entity named `,
			 EString,
			 `. This is not allowed.`],
			 Message,_),
		  msgbox(`Illegal Entity Names`,Message,0,_)
		 )
		)),
	assert(vcurrent(level,(0,0))),
	wtext((vworld,40),`0,0`),
	set_icons(basic),
	vcolor_world,
	(
	 vagent_data(X,Y,_,_,_),
	 vdraw(X,Y,a)
	;
	 true
	),
	vmenu_status(`Save Map`,0),
	vmenu_status(`Save Map As`,1),
	vmenu_status(`Create Entity`,1),
	vmenu_status(`Assign Guard`,1),
	vmenu_status(`Install Door`,1),
	vmenu_status(`Locate Agent`,1),
	wbtnsel((vworld,700),0).

% user_input(+Title,+Message,-String) creates and calls a
% dialog window with caption Title and displayed Query.
% The user may respond to the query by typing in an edit
% field.

user_input(Title,Query,String) :-
	wdcreate(question,Title,200,100,200,120,
		[ws_popup,ws_visible,ws_border,ws_caption]),
	wccreate((question,1),static,Query,10,10,180,20,
		[ws_child,ws_visible]),
	wccreate((question,2),edit,``,10,35,180,15,
		[ws_child,ws_visible,ws_border]),
	wccreate((question,3),button,`Okay`,39,60,50,20,
		[ws_child,ws_visible,bs_pushbutton]),
	wccreate((question,4),button,`Cancel`,109,60,50,20,
		[ws_child,ws_visible,bs_pushbutton]),
	window_handler(question,q_hndl),
	call_dialog(question,String).

q_hndl((question,3),msg_button,_,String) :-
	wtext((question,2),String).

q_hndl((question,4),msg_button,_,`cancel`).

vmenu(`Create Entity`,_) :-
	create_entity.

create_entity :-
	get_entity_name(E),
	E \== cancel,
	get_entity_icon(File,Index),
	vstring_term(String,E),
	atom_string(Icon,String),
	gfx_icon_load_old(E,File,Index),
	findall(X,user_entity(X,_,_,_,_),Y),
	len(Y,L),
	M is L + 1,
	assert(user_entity(M,E,File,Index,Icon)),
	set_icons(user).

get_entity_name(E) :-
	user_input(`New Entity`,`Enter a short name for the new entity`,String),
	vstring_term(String,E),
	(
	 basic_entity(_,E,_),
	 msgbox(`Illegal Entity Name`,`A basic VWORLD entity already has that name.`,0,_),
	 !,
	 fail
	;
	 user_entity(_,E,_,_,_),
	 msgbox(`Name Used`,`You already have a user entity with that name.`,0,_),
	 !,
	 fail
	;
	 true
	).

get_entity_icon(File,Index) :-
	opnbox(`Load Icon Source`,
		[(`Icon Library: *.icl`,`*.icl`),
		 (`Icon Files: *.ico`,`*.ico`),
		 (`Program Files: *.exe`,`*.exe`)],
		``,`icl`,[File|_]),
	retractall(vcurrent(iconfile,_)),
	assert(vcurrent(iconfile,File)),
	wdcreate(iconpicker,`Icons in File`,100,100,366,386,
		[ws_popup,ws_visible,ws_caption,ws_border]),
	wccreate((iconpicker,1),grafix,``,0,0,362,362,
		[ws_child,ws_visible]),
	paint_temp_icons,
	window_handler(iconpicker,i_hndl),
	call_dialog(iconpicker,Index).

i_hndl((iconpicker,1),msg_leftup,(X,Y),Index) :-
	get_icon_index(X,Y,Index).

get_icon_index(X,Y,Index) :-
	Col is X//36,
	Row is Y//36,
	Index is Col + (Row * 10).

i_hndl((iconpicker,1),msg_paint,_,_) :-
	paint_temp_icons.

paint_temp_icons :-
	vcurrent(iconfile,File),
	show_icons(0,File).

show_icons(Index,File) :-
	catch(Error,gfx_icon_load_old(temp,File,Index)),
	Error = 0,
	X is (Index mod 10) * 36 + 2,
	Y is Index//10 * 36 + 2,
	gfx_begin((iconpicker,1)),
	gfx(icon(X,Y,temp)),
	gfx_end((iconpicker,1)),
	NewIndex is Index + 1,
	!,
	show_icons(NewIndex,File).

show_icons(_,_).

vmenu(`Assign Guard`,_) :-
	assign_guard.

assign_guard :-
	vmnusel(vactions,2,Status),
	Status = 1,
	!,
	retractall(vcurrent(guard,_)),
	vmnusel(vactions,2,0).

assign_guard :-
	vmnusel(vactions,1,1),
	vmnusel(vactions,2,0),
	vmnusel(vactions,3,0),
	retractall(vcurrent(door,_)),
	cat([ `Before assigning guard duty, you must first place the `,
		`item to be guarded and the actor who will guard it in `,
		`the map. Normally, you would place them within two cells `,
		`of each other. ~J~M~J~M`,
		`To assign an actor to guard an item, first click the actor `,
		`in the map and then click the item to be guarded in the map.`,
		`~J~M~J~M`,
		`To cancel this operation, either click cancel below or click `,
		`Assign Guard on the Actions menu.~J~M~J~M`,
		`Click Okay to proceed with guard assignment.`],
		Message,_),
	msgbox(`Assign Guard`,Message,1,Reply),
	(
	 Reply = 1
	;
	 vmnusel(vactions,1,0)
	).

vmenu(`Install Door`,_) :-
	install_door.

install_door :-
	vmnusel(vactions,2,Status),
	Status = 1,
	!,
	retractall(vcurrent(key,_)),
	vmnusel(vactions,2,0).

install_door :-
	vmnusel(vactions,1,0),
	vmnusel(vactions,2,1),
	vmnusel(vactions,3,0),
	retractall(vcurrent(guard,_)),
	cat([ `To install a door, click on the outer wall of the `,
		`VWORLD map at the location where you want the new door.`,
		`~J~M~J~M`,
		`To cancel this operation, either click cancel below or click `,
		`Install Door on the Actions menu.~J~M~J~M`,
		`Click Okay to proceed with door installation.`],
		Message,_),
	msgbox(`Install Door`,Message,1,Reply),
	(
	 Reply = 1
	;
	 vmnusel(vactions,1,0)
	).

vmenu(`Locate Agent`,_) :-
	vmnusel(vactions,3,Status),
	(
	 Status = 0,
	 vmnusel(vactions,1,0),
	 vmnusel(vactions,2,0),
	 vmnusel(vactions,3,1),
	 msgbox(`Locate Agent`,
		`Click the screen in the location where you wish to place the agent.`,
		0,_)
	;
	 vmnusel(vactions,3,0)
	).

vmenu(`V-World Basics`,_) :-
 	chdir(Dir),cat([Dir,'\','v-world.htm'],File,_),
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
 `MAPMAKER version 4.300~J~M~J~MCopyright 2003 Donald Nute~J~MArtificial Intelligence Center~J~MThe University of Georgia`,
 	0,_).

vmenu(`Exit`,R) :-
	vexit(R).

vmenu(`Exit`,_).

vexit(ok) :-
	msgbox(`Exit VWORLD Mapmaker`,
 		`Do you want to exit the VWORLD Mapmaker?`,36,B),
	!,
	B = 6,
	save_map_if_needed.

save_map_if_needed :-
	(
	 vmenu_status(`Save Map`,Status),
	 Status = 0
	;
	 msgbox(`Save Map`,`Do you want to save the current VWORLD map?`,3,Response),
	 Response > 2,
	 (
	  Response = 7
	 ;
	  save_map
	 )
	).

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

% The abort handler vabort/0  

vabort :-
 	vtidy,
	exit(0). 


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

% vmnusel(+Menu,+Pos,?Status) gets or sets the status
% of a menu item.

vmnusel(Menu,Pos,Status) :-
	var(Status),
	!,
	wmnusel(Menu,Pos,Status).

vmnusel(Menu,Pos,Status) :-
	wmnusel(Menu,Pos,Status),
	wndhdl(vworld,WinHandle),
	winapi((user32,'DrawMenuBar'),[WinHandle],0,_).

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
	winapi((user32,'DrawMenuBar'),[Handle],0,_),
	!.

% If a menu is not present, do nothing.

vmenu_status(_,_).

% Various window handlers will call vmenu_action(+Win,+Num,-Response)
% when they receive a msg_menu message. m_action/3 will be described 
% for each dialog.

vmenu_action(Win,Num,Response) :-
	vmenu_item_name(Win,Num,Item),
	vmenu(Win,Item,Response).

% Empty map for a new level.

emap( 0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
emap( 1,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 2,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 3,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 4,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 5,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 6,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 7,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 8,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap( 9,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap(10,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap(11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap(12,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap(13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap(14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
emap(15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).


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



