% TEST6.vw
%
% A Virtual World
% Author: Donald Nute
%
% Date Created: 9/11/2003
% Last Modified: 8/4/2004
%
% Notes:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1,vconsumable/1,vanimate/1, vguard/2,
            date_created/1, last_modified/1, author/1].

:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2, vwants/2, vfears/2,
             vdrop_effect/2, date_created/1, last_modified/1, author/1.

% vagent_data/5

vagent_data( 5, 3, 4000, 0, same ).


% vkey/7

vkey( none, (0,0), 2, 0, (0,-1), 2, 15 ).

vkey( none, (0,0), 19, 0, (0,-1), 19, 15 ).

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
vmap((0,-1),11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),12,[w,o,o,o,snail,o,o,o,snail,o,o,o,snail,o,o,o,snail,o,o,snail,o,w]).
vmap((0,-1),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,-1),15,[w,w,door,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,door,w,w]).

vmap((0,0),0,[w,w,door,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,door,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,o,o,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,o,o,o,o,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,tree,o,o,o,o,w,w,w,w,w,o,o,o,cross,o,o,o,w]).
vmap((0,0),4,[w,o,o,o,o,o,o,o,o,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,0),5,[w,hornet,o,o,o,o,o,o,hornet,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,o,o,w,w,w,w,w,o,o,o,o,o,o,o,w]).
vmap((0,0),7,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),8,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),9,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),10,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),11,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),12,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),13,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),14,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

