% BIRD-2.vw
%
% A Virtual World
% Author: Donald Nute
%
% Date Created: 6/19/2003
% Last Modified: 8/17/2003
%
%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1, vconsumable/1,vanimate/1, vguard/2].
:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2.

vagent_data(10,2,4000,0,same).

% vkey/7

vkey( none, (0,0), 14, 0, (0,-1), 14, 15 ).

vkey( bkey, (0,0), 21, 3, (1,0), 0, 3 ).

vkey( none, (0,-1), 21, 9, (1,-1), 0, 9 ).

vfears(bird,bugspray).
vwants(bird,birdseed).

vgoal_to_catch_bird.

vmap((0,-1),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,-1),1,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),2,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w,w,w,w,w]).
vmap((0,-1),3,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w,o,bkey,o,w]).
vmap((0,-1),4,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w,w,w,o,o,o,w]).
vmap((0,-1),5,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w,o,hornet,o,hornet,o,w]).
vmap((0,-1),6,[w,o,o,o,o,o,o,o,o,o,o,o,o,w,w,w,o,o,o,o,o,w]).
vmap((0,-1),7,[w,o,o,o,o,o,o,o,o,o,o,o,o,w,o,o,o,o,hornet,o,o,w]).
vmap((0,-1),8,[w,o,o,o,o,o,o,o,o,o,o,w,w,w,o,o,hornet,o,o,o,o,w]).
vmap((0,-1),9,[w,o,o,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,hornet,o,o,door]).
vmap((0,-1),10,[w,o,o,o,o,o,o,o,o,o,o,w,o,o,o,hornet,o,o,o,o,o,w]).
vmap((0,-1),11,[w,o,o,o,o,o,o,o,o,o,o,w,o,o,hornet,o,o,hornet,o,o,o,w]).
vmap((0,-1),12,[w,o,o,o,o,o,o,o,o,o,o,w,o,o,o,o,hornet,o,o,o,o,w]).
vmap((0,-1),13,[w,o,o,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),14,[w,o,o,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,door,w,w,w,w,w,w,w]).

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,door,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,o,o,o,o,o,w,o,o,tree,o,o,o,o,bugspray,o,o,o,door]).
vmap((0,0),4,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,cross,o,o,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),7,[w,o,o,o,o,o,o,o,o,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),8,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),9,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),10,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),12,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

vmap((1,-1),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,-1),1,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),2,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),3,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),4,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),5,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),6,[w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),7,[w,o,o,w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),8,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),9,[door,o,o,bird,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),10,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),11,[w,o,o,w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),12,[w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,-1),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

vmap((1,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,0),1,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),2,[w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),3,[door,o,o,o,birdseed,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),4,[w,o,o,o,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),5,[w,o,o,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),6,[w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),7,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),8,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),9,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),10,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),12,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((1,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

