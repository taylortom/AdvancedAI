% TEST2.VW
% A V-World File
%
% A Virtual World
% Author: Donald Nute
%
% Date Created: 6/6/2003
% Last Modified: 8/4/2004
%
% Notes:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1,vconsumable/1,vanimate/1,vguard/2,
            date_created/1, last_modified/1, author/1].

:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2, vwants/2, vfears/2,
             vdrop_effect/2, date_created/1, last_modified/1, author/1.

% vagent_data/5

vagent_data( 7, 2, 4000, 0, same ).

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,o,hornet,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),2,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),4,[w,o,o,o,tree,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),7,[w,o,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),8,[w,cross,o,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),9,[w,w,w,w,w,w,w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),10,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),11,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),12,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),13,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

