%
% castle2.vw
%
% A Virtual World
%
% Date Created:  7/20/2003
% Last Modified: 8/4/2005
%
% Notes:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic [vagent_data/5,vmap/3,vobject_icon/2,veff/3,vact/3,vkey/7,vcurrent_level/1,
            vmovable/1,vcollectable/1,vmulticollectable/1, vconsumable/1,vanimate/1, vguard/2].
:- multifile vobject_icon/3, veff/3, vconsumable/1, vcollectable/1, vmulticollectable/1,
             vact/3, vreact/3, v_icon/3, vnature/0, vguard/2.

vagent_data(8,1,4000,0,same).
vagent_icon(knight,vworld).

vguard(bird,cross).
vguard(troll,flower).
vguard(dragon,gold).
%vguard(witch,princess).
vwants(witch,gold).
vfears(dragon,flower).

% vkey/7

vkey( blukey, (0,0), 9, 15, (0,1), 9, 0 ).

vkey( ykey, (0,1), 21, 6, (1,1), 0, 6 ).

vkey( gkey, (0,1), 18, 15, (0,2), 18, 0 ).

vkey( pkey, (0,1), 7, 15, (0,2), 7, 0 ).

vmap((0,0),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),1,[w,fruit,fruit,fruit,fruit,fruit,fruit,w,o,o,o,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),2,[w,fruit,fruit,fruit,fruit,fruit,fruit,w,o,throne,o,w,w,o,o,w,w,w,o,o,o,w]).
vmap((0,0),3,[w,fruit,fruit,fruit,fruit,fruit,fruit,w,o,o,o,w,o,o,o,w,w,w,o,blukey,o,w]).
vmap((0,0),4,[w,fruit,fruit,fruit,fruit,fruit,fruit,w,w,o,w,w,o,o,o,o,w,w,o,o,o,w]).
vmap((0,0),5,[w,fruit,fruit,fruit,fruit,fruit,fruit,w,o,o,o,o,o,w,w,o,o,w,w,w,o,w]).
vmap((0,0),6,[w,fruit,fruit,fruit,fruit,fruit,fruit,w,o,o,o,w,w,w,w,w,o,o,w,w,o,w]).
vmap((0,0),7,[w,w,w,o,o,w,w,w,o,o,o,w,w,w,w,w,o,o,w,w,o,w]).
vmap((0,0),8,[w,w,o,o,w,w,w,w,o,o,o,w,w,w,w,w,o,o,o,o,o,w]).
vmap((0,0),9,[w,w,w,o,o,w,w,w,o,o,o,w,w,w,w,w,o,sword,o,w,w,w]).
vmap((0,0),10,[w,w,w,w,o,o,w,w,o,o,o,w,w,w,w,w,o,o,o,w,w,w]).
vmap((0,0),11,[w,w,w,w,w,o,o,w,o,o,o,w,w,w,w,w,o,shield,o,w,w,w]).
vmap((0,0),12,[w,w,w,w,w,w,o,o,o,o,o,w,w,w,w,w,o,o,o,w,w,w]).
vmap((0,0),13,[w,w,w,w,w,w,w,o,o,o,o,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),14,[w,w,w,w,w,w,w,o,o,o,o,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,0),15,[w,w,w,w,w,w,w,w,blublock,door(blukey),blublock,w,w,w,w,w,w,w,w,w,w,w]).

vmap((0,1),0,[w,w,w,w,w,w,w,w,blublock,door(blukey),blublock,w,w,w,w,w,w,w,w,w,w,w]).
vmap((0,1),1,[w,tree,tree,tree,tree,tree,water,water,o,o,o,o,o,o,o,thorn,thorn,thorn,thorn,thorn,thorn,w]).
vmap((0,1),2,[w,tree,tree,tree,tree,water,water,water,water,o,o,o,o,o,o,o,thorn,thorn,thorn,thorn,thorn,w]).
vmap((0,1),3,[w,tree,tree,water,water,water,water,water,water,water,o,o,o,o,o,o,o,o,thorn,thorn,thorn,w]).
vmap((0,1),4,[w,tree,tree,water,water,water,water,water,water,water,water,o,o,o,o,thorn,o,o,o,thorn,thorn,w]).
vmap((0,1),5,[w,tree,o,tree,o,tree,water,water,water,water,water,o,o,o,o,o,thorn,thorn,o,o,o,yblock]).
vmap((0,1),6,[w,o,tree,o,tree,o,tree,water,water,water,water,o,o,o,o,o,o,thorn,thorn,o,o,door(ykey)]).
vmap((0,1),7,[w,tree,o,tree,o,tree,o,o,water,water,o,o,o,o,o,o,tree,thorn,thorn,thorn,thorn,yblock]).
vmap((0,1),8,[w,o,tree,o,tree,o,tree,o,o,o,o,o,o,o,o,o,o,thorn,thorn,thorn,thorn,w]).
vmap((0,1),9,[w,tree,ykey,tree,o,tree,o,tree,o,o,o,o,tree,tree,o,o,o,thorn,thorn,thorn,thorn,w]).
vmap((0,1),10,[w,o,tree,o,tree,o,tree,o,o,o,o,tree,tree,tree,tree,o,o,thorn,thorn,thorn,thorn,w]).
vmap((0,1),11,[w,tree,o,tree,o,tree,o,o,o,o,tree,tree,thorn,thorn,thorn,o,o,thorn,thorn,thorn,thorn,w]).
vmap((0,1),12,[w,o,o,o,tree,o,tree,o,tree,o,tree,thorn,thorn,gkey,thorn,thorn,o,o,o,thorn,thorn,w]).
vmap((0,1),13,[w,o,cross,o,o,tree,o,tree,o,tree,tree,thorn,thorn,thorn,o,o,thorn,thorn,o,o,o,w]).
vmap((0,1),14,[w,o,o,o,tree,o,tree,o,tree,tree,thorn,thorn,thorn,thorn,thorn,thorn,thorn,o,o,o,o,w]).
vmap((0,1),15,[w,w,w,w,w,w,pblock,door(pkey),pblock,w,w,w,w,w,w,w,w,gblock,door(gkey),gblock,w,w]).

vmap((0,2),0,[w,w,w,w,w,w,pblock,door(pkey),pblock,w,w,w,w,w,w,w,w,gblock,door(gkey),gblock,w,w]).
vmap((0,2),1,[w,w,w,w,w,w,w,o,w,w,w,w,w,w,w,w,o,o,o,o,w,w]).
vmap((0,2),2,[w,w,w,w,w,w,w,o,w,w,w,w,w,w,o,o,o,o,o,snail,o,w]).
vmap((0,2),3,[w,w,w,w,w,w,w,o,w,w,w,w,w,o,o,snail,o,hornet,o,o,o,w]).
vmap((0,2),4,[w,w,w,w,w,w,w,o,w,w,w,w,o,o,hornet,o,o,dragon,o,o,o,w]).
vmap((0,2),5,[w,w,w,w,w,w,w,o,w,w,w,w,o,o,o,o,o,o,gold,o,o,w]).
vmap((0,2),6,[w,w,w,w,o,o,o,o,o,o,o,w,o,o,snail,o,o,o,o,o,o,w]).
vmap((0,2),7,[w,w,w,w,o,o,o,o,witch,o,princess,w,w,o,hornet,o,o,o,o,o,w,w]).
vmap((0,2),8,[w,w,w,w,o,o,o,o,o,o,o,w,w,w,o,o,hornet,o,o,w,w,w]).
vmap((0,2),9,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,o,o,w,w,w,w]).
vmap((0,2),10,[w,w,w,w,w,w,w,fruit,fruit,fruit,fruit,w,w,w,w,w,o,o,w,w,w,w]).
vmap((0,2),11,[w,w,w,w,w,w,w,fruit,fruit,pkey,fruit,w,w,w,w,w,o,o,w,w,w,w]).
vmap((0,2),12,[w,w,w,w,w,w,w,fruit,fruit,fruit,fruit,w,w,w,w,hornet,o,o,w,w,w,w]).
vmap((0,2),13,[w,w,w,w,w,w,w,fruit,fruit,fruit,fruit,o,hornet,o,o,o,o,w,w,w,w,w]).
vmap((0,2),14,[w,w,w,w,w,w,w,fruit,fruit,fruit,fruit,o,o,o,o,o,w,w,w,w,w,w]).
vmap((0,2),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

vmap((1,1),0,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),1,[w,w,w,fruit,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),2,[w,w,o,o,o,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),3,[w,o,o,o,o,o,o,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),4,[w,o,o,o,o,o,o,fruit,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),5,[yblock,o,o,o,troll,flower,o,o,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),6,[door(ykey),o,o,o,o,o,o,o,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),7,[yblock,o,o,o,o,o,o,o,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),8,[w,o,o,o,o,o,o,o,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),9,[w,o,o,o,hornet,o,o,o,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),10,[w,w,w,fruit,o,o,o,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),11,[w,w,w,w,fruit,fruit,cross,fruit,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),12,[w,w,w,w,w,fruit,fruit,bugspray,fruit,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),13,[w,w,w,w,w,fruit,fruit,fruit,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),14,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).
vmap((1,1),15,[w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]).

