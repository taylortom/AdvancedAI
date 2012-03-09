% bird-4.vw
% A Virtual World
author('Donald Nute').
date_created('12/8/2003').
last_modified('12/8/2003').

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1,vconsumable/1,vanimate/1,vguard/2].

:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2, vwants/2, vfears/2,
             vdrop_effect/2.


% vagent_data/5

vagent_data( 11, 4, 1000, 0, same ).

vwants(bird,birdseed).

vfears(bird,bugspray).

% You win the game when you have caught and released all the
% birds in the small room.

vnature :-
	findall(N,(vmap((0,-1),M,List),M > 10,member(bird,List,N)),Birds),
	len(Birds,3),
	msgbox(`A voice from nowhere says:`,
 		`Well, you finally caught the little fellows!`,0,_),
	assert(vgame_over),
	fail.


% vkey/7

vkey( none, (0,0), 11, 0, (0,-1), 11, 15 ).

vmap((0,-1),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),1,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),2,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),3,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),4,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),5,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),6,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),7,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),8,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),9,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),10,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),11,[w,w,w,w,w,w,w,w,w,o,o,bugspray,o,o,w,w,w,w,w,w,w,w]).
vmap((0,-1),12,[w,w,w,w,w,w,w,w,w,o,o,birdseed,o,o,w,w,w,w,w,w,w,w]).
vmap((0,-1),13,[w,w,w,w,w,w,w,w,w,o,o,throne,o,o,w,w,w,w,w,w,w,w]).
vmap((0,-1),14,[w,w,w,w,w,w,w,w,w,o,o,o,o,o,w,w,w,w,w,w,w,w]).
vmap((0,-1),15,[w,w,w,w,w,w,w,w,w,w,w,door,w,w,w,w,w,w,w,w,w,w]).

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,door,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,hornet,o,w,o,o,hornet,o,o,o,o,o,o,w,o,bird,o,o,w]).
vmap((0,0),4,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,w,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,w,o,o,hornet,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,w,o,o,o,o,o,o,hornet,o,o,w,o,o,o,o,w]).
vmap((0,0),7,[w,o,o,o,cross,o,w,w,w,w,w,w,w,w,w,w,w,o,tree,o,o,w]).
vmap((0,0),8,[w,o,bird,o,o,o,w,o,o,o,o,o,o,o,o,o,w,o,o,o,o,w]).
vmap((0,0),9,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,w,o,o,o,hornet,w]).
vmap((0,0),10,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,w,o,o,o,o,w]).
vmap((0,0),11,[w,o,o,o,o,o,w,o,o,o,bird,o,o,o,o,o,w,o,o,o,o,w]).
vmap((0,0),12,[w,o,o,o,hornet,o,w,o,o,hornet,o,o,o,o,o,o,w,hornet,o,o,o,w]).
vmap((0,0),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

