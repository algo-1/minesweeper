# Minesweeper

The classic Minesweeper game implemented in Haskell.

## Solver

Given a board (with flags from the user (do not trust these and assume them to be fair game? or do we ignore them as you first have to unflag before opening that spot so 2 turns really))

Ignoring flags, let us assume we have a board with some squares opened, we can safely assume the game is not over,
the task is to decide what square to open.

### Strategy
case - there is one unambigous square to open
open this square

case - there are more than one,
open the one with the best outcome, meaning? largest number of potential squares cleared? cannot know this without knowing where the mnines are loool so no, perhaps one that puts the user in the best spot based on probabilities | logic?

case - no unambigous squares
guess based on the best probability whatever that means
...

How to calculate probabilities?
...

How to detect patterns efficiently and use them for finding unambigous plays??
...

## TODO

cite sources in report
clean up this readme++
