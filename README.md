## Sudoku in Haskell

### To run
`cabal new-build`
`cabal new-exec Sudoku`

### Collaborators
Chloe Sheen 83113573

Xiaojing Chen 47990673


### Overview of Files:
#### Board.hs
Defines the Sudoku puzzle board, prints the board, updates the board with valid
inputs, and tracks blank cells. 
#### Check.hs
Checks that we have a valid representation of a board, and also checks that the
entire board does not have duplicate values.
#### Main.hs
Handles the input/output portion of the program.
#### Solver.hs
Solves the puzzle and deals with hints given a current board.
#### Tests.hs
Contains quickCheck properties. 
