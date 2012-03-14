% CASTLE1.VW
%
% A Virtual World
% Author: Donald Nute
%
% File Created: 6/19/2003
% Last Modified: 8/4/2005
%
%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1, vconsumable/1,vanimate/1, vguard/2].
:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2.

vagent_data(9,2,4000,0,same).
vagent_icon(knight,vworld).

vguard(dragon,gold).
vguard(troll,flower).
vguard(witch,princess).
vwants(witch,gold).
vfears(dragon,flower).

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,o,o,o,o,o,o,w,o,o,o,o,o,o,o,w,o,o,o,witch,princess,w]).
vmap((0,0),2,[w,o,o,gold,o,o,o,w,o,o,o,throne,shield,o,o,w,o,o,o,o,o,w]).
vmap((0,0),3,[w,o,o,dragon,o,o,o,w,o,o,o,o,o,o,o,w,o,o,o,o,o,w]).
vmap((0,0),4,[w,o,o,o,o,o,thorn,w,fruit,o,o,o,o,o,fruit,w,o,o,o,o,o,w]).
vmap((0,0),5,[w,o,o,o,o,thorn,thorn,w,fruit,fruit,o,o,o,fruit,fruit,w,thorn,o,o,o,o,w]).
vmap((0,0),6,[w,o,o,o,w,w,w,w,w,w,o,o,o,w,w,w,thorn,thorn,o,o,o,w]).
vmap((0,0),7,[w,o,o,o,o,o,o,o,o,o,o,o,o,thorn,thorn,w,w,w,o,o,o,w]).
vmap((0,0),8,[w,thorn,o,o,o,o,o,o,o,o,o,o,o,o,thorn,w,o,o,o,o,o,w]).
vmap((0,0),9,[w,w,w,w,w,w,w,w,o,o,o,o,o,o,o,o,o,o,o,w,w,w]).
vmap((0,0),10,[w,o,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),11,[w,o,w,w,w,w,o,w,o,o,tree,o,o,o,o,o,o,o,o,o,o,w]).
vmap((0,0),12,[w,o,w,sword,o,o,o,w,o,o,o,o,o,cross,o,w,o,o,troll,o,thorn,w]).
vmap((0,0),13,[w,o,w,w,w,w,o,w,o,o,o,o,o,o,thorn,w,o,o,o,flower,thorn,w]).
vmap((0,0),14,[w,o,o,o,o,o,o,o,o,o,o,o,o,thorn,thorn,w,o,o,thorn,thorn,thorn,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

