% TEST5.VW
%
% a Virtual World
% Author: Donald Nute
%
% Date Created: 9/17/2001
% Last Modified: 8/4/2005
%
% Notes:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1, vconsumable/1,vanimate/1, vguard/2].
:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2.

vagent_data(2,2,4000,0,same).

% Every level in every world should have walls on all sides.

% The current level is set to 1 when the world file is loaded.

% Level 1:

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,o,w,o,o,o,o,o,o,hornet,o,o,o,o,o,o,o,o,w]).
vmap((0,0),4,[w,o,o,o,o,w,o,o,o,w,w,o,o,o,w,w,w,w,w,w,w,w]).
vmap((0,0),5,[w,o,o,o,o,w,o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,w]).
vmap((0,0),7,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,w,o,o,cross,o,o,w]).
vmap((0,0),8,[w,o,o,o,o,o,o,o,o,w,o,o,tree,o,o,w,o,o,o,o,o,w]).
vmap((0,0),9,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,w]).
vmap((0,0),10,[w,o,o,o,o,w,o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,w]).
vmap((0,0),11,[w,o,o,o,o,w,o,o,o,w,w,w,w,w,w,w,w,o,o,o,w,w]).
vmap((0,0),12,[w,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),13,[w,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,hornet,o,o,w]).
vmap((0,0),14,[w,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).



