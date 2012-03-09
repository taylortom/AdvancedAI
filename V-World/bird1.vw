% BIRD-1.vw
%
% A Virtual World
% Author: Donald Nute
%
% Date Created: 6/19/17
% Last Modified: 8/17/2003
%
%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1, vconsumable/1,vanimate/1, vguard/2].
:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2.

vagent_data(10,2,1000,0,same).

vguard(hornet,birdseed).
vwants(bird,birdseed).

vgoal_to_catch_bird.

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,o,o,o,o,o,w,o,o,tree,o,o,hornet,o,o,o,o,o,w]).
vmap((0,0),4,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,bird,o,o,o,o,o,w,o,o,o,o,cross,o,o,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),7,[w,o,w,w,w,w,w,w,w,w,o,o,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),8,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),9,[w,o,o,o,o,o,o,o,o,o,o,flower,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),10,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,snail,o,o,o,o,w]).
vmap((0,0),11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),12,[w,o,o,o,hornet,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),13,[w,o,o,o,birdseed,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

