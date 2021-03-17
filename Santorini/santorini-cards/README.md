# Santorini

## Player

`santorini-cards` extends the functionality of `santorini-base` to support playing 
 with the [*simple god set*](http://files.roxley.com/Santorini-Rulebook-Web-2016.08.14.pdf).

:warning: My player implements all necessary functionality for the below cards, however,
the strategy for choosing the best possible move is extremely naïve and rarely results
in a well-played game.

### God Power Descriptions

* Apollo — 
  A token’s move can optionally swap places with an adjacent opponent token, 
  as long as the token would be able to move to the opponent’s space if the opponent 
  token were not there; otherwise, the move must be to an unoccupied space as usual.

* Artemis — 
  The moved token can optionally move a second time (i.e., the same token), 
  as long as the first move doesn’t win, and as long as the second move doesn’t 
  return to the original space.

* Atlas — 
  The build phase can build a space currently at level 0, 1, 2 to make it level 4, 
  instead of building to exactly one more than the space’s current level.

* Demeter — 
  The moved token can optionally build a second time, but not on the same space as the 
  first build within a turn.

* Hephastus — 
  The moved token can optionally build a second time, but only on the same 
  space as the first build within a turn, and only if the second build does not reach level 4.

* Minotaur — 
  A token’s move can optionally enter the space of an opponent’s token, 
  but only if the token can be pushed back to an unoccupied space, and only as long 
  as the token would be able to move to the opponent’s space if the opponent token were not there. 
  The unoccupied space where the opponent’s token is pushed can be at any level less than 4. 
  Note that the opponent does not win by having a token forced to level 3; 
  furthermore, such a token will have to move back down before it can move to level 3 for a win.

* Pan — 
  A token can win either by moving up to level 3 or by moving down two or more levels. 
  (Moving down three levels is possible if a token was pushed by a Minotaur.)

* Prometheus — 
  A token can optionally build before moving, but then the move is constrained to the 
  same level or lower (i.e., the level of the token’s new space can be no larger than 
  the level of the token’s old space). The moved token must still build after moving.

1. The `player` executable. Main is defined in `playerapp/PlayerMain.hs` and the 
library is `src/Player.hs`.

Other important libraries include:

* `src/SantoriniDefs.hs`

* `src/SantoriniUtils.hs`

* `src/PlayerStrategy.hs`

## Building Player

`stack build`

The executables are found in 
`./.stack-work/install/x86_64-osx/202c490e26cd90901fb3c6e9985a679cadc0e3588f4baafa7a56b5c0320dbd69/8.10.3/bin`

### Building Player for CADE Lab

To build the player to work on the CADE lab machines this needs to be done statically 
as all of the necessary `.so` files are not available (or are outdated versions). This
can be achieved by changing the *player* configuration of the executable in `package.yaml` to 
the following

```
...
ghc-options: -Wall -O2 -static -threaded
cc-options: -static
ld-options: -static -pthread
extra-lib-dirs: <insert paths to all necessary .a files
...
```

## Test Runner

The test runner is a simple program which will place two players in competition with each other
with all combinations of available cards. The runner must be passed three command line arguments

`runner <referee program> <player 1> <plaery 2>`

The main goal of the runner is to make sure that neither player makes a bad move. This can be useful
to quickly test the validity of your moves with all the available cards (against all available cards)
if you place your executable in competition with a random agent.

