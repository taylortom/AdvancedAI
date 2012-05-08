% DoorSearchTest.map
% A Virtual World
author('Spencer').
date_created('5/8/2012').
last_modified('5/8/2012').

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1,vconsumable/1,vanimate/1,vguard/2].

:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2, vwants/2, vfears/2,
             vdrop_effect/2.


% vagent_data/5

vagent_data( 8, 1, 1000, 0, same ).


% vguard/2


% vkey/7

vkey( none, (0,0), 10, 0, (0,-1), 10, 15 ).


% vwants/2


% vfears/2


% vnature/1


% vgoal_to_catch_bird/0

vmap((0,-1),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),1,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),2,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),3,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),4,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),5,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),6,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),7,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),8,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),9,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),10,[w,o,o,o,o,o,o,o,w,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,-1),11,[w,o,o,o,o,o,o,o,w,o,cross,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,-1),12,[w,o,o,o,o,o,o,o,w,o,o,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,-1),13,[w,o,o,o,o,o,o,o,w,o,o,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,-1),14,[w,o,o,o,o,o,o,o,w,o,o,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,-1),15,[w,w,w,w,w,w,w,w,w,w,door,w,w,w,w,w,w,w,w,w,w,w]).

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,door,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,o,o,o,w,o,o,tree,o,o,w,o,o,o,o,o,o,o,w]).
vmap((0,0),4,[w,o,o,o,o,o,o,w,o,o,o,o,hornet,w,o,o,o,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,o,o,o,o,w,w,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),7,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),8,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),9,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),10,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),12,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

