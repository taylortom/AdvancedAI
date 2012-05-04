Advanced AI
-----------

GO BUMBLE!
																	   
Features
--------
	
Things we plan to implement, as well as stuff we've already done.
	
### Required Features :

- Swarming Hornets
	- Formations
	- Decentralised Communication
		- if attacked hornets become enranged
		- remember last location of enemy, this can be communicated to another hornet
- Some sort of planning system
	- Danger associated with object (Gained from reinforcement? )
 
### Desired Features :

- Genetic Algo
- Q learning
- Descision Trees
- Heierachical Finite State Machies


### Implemented Features :

- The hornets follow Bumble upon spying him.

- Bumble no longer flees if in possession of the shield/sword

- When Bumble becomes hurt, he checks the map for the appropriate resources to fix him.

- Fixed a bug whereby Bumble could get stuck alternating between two interesting objects (e.g. tree/cross)

- Hornets no longer cause injury if Bumble's in possession of the bugspray. Also, the bugspray now actually works.

- Added a conversion from cartesian coordinates to directions

- Bumble now spies desirable/interesting objects from 2 squares away

- Fixed a bug whereby Bumble could get stuck in a corridor 1 sq wide (see castle2.vw). If there was an interesting/
desirable object at the end of the corridor, Bumble would try to go west, but would simple stay in the same sqaure. 
There's now a check to see if Bumble already attempted the same move, if so, he moves randomly.

- A map of the entire level is built. When Bumble goes through a door, an extra square is added in the appropriate 
direction to account for the wall in-between the levels. As removable items are taken by Bumble, they are also
removed from the map.

- Implemented basic fuzzy reasoning for the strength/health. Bumble now has more levels: Not hungry, small hungry, medium 
hungry, starving etc. These levels are used when determining whether Bumble should consume fruit/use crosses. If Bumble 
sees fruit, but is very hurt, it will pass by and continue looking for a cross.

- Added an exploration mode which is enabled/disabled based on Bumble's current health/strength. If not hungry/hurt, 
Bumble will explore the map for areas not visited using the preference: ne, nw, se, sw, n, e, s, w. As Bumble explores, 
the squares he visits are marked off, removing them from the exploration mode. Bumble stops exploring if he can't see 
any nearby unvisited squares.



Characters
----------

- Bumble(Working Title)
- The Dragoon
- The Princess
- Witch 
	
	
																	   
Resources
---------

The place for any useful links and documentation.

### AI Resources

- [Game AI on reddit](http://www.reddit.com/r/gameai)



### Prolog Resources

I have collected some resources on prolog and search in prolog

- [(Book) AI through prolog](http://faculty.nps.edu/ncrowe/book/book.html)
	- Useful Parts
		- [Search Including A*](http://faculty.nps.edu/ncrowe/book/chap10.html)
		- [More on search](http://faculty.nps.edu/ncrowe/book/chap11.html)
- [Prolog thread on reddit](http://www.reddit.com/r/prolog/)
	

### Win-Prolog References
	
- [Programming Guide](http://www.lpa.co.uk/ftp/4900/win_prg.pdf)
- [Technical Reference](http://www.lpa.co.uk/ftp/4900/win_ref.pdf)
- [User Guide](http://www.lpa.co.uk/ftp/4900/win_usr.pdf)
	
	
### Random

- [Nice page I found about markdown](http://daringfireball.net/projects/markdown/syntax)