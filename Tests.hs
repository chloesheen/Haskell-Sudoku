module Tests where
import Test.QuickCheck
import Board
import Check
import Solver
import Control.Monad (liftM, replicateM_)

-- generator for a sudoku blank
genCell :: Gen (Maybe Int)
genCell = frequency [(1, fmap Just (choose (1,9))), (9, return Nothing)]
         
instance Arbitrary Puzzle where
    arbitrary =
      do rows <- sequence [sequence [genCell | j <- [1..9] ] | i <- [1..9]]
         return (Puzzle rows)


prop_Sudoku :: Puzzle -> Bool
prop_Sudoku = isPuzzle

-- board should have 9 rows, 9 cols, 9 boxes w/ 9 cells
prop_blocks :: Puzzle -> Bool
prop_blocks s = length (createBlocks s) == 27 
                && and [length b == 9 | b <- (createBlocks s)]

prop_blanks :: Puzzle -> Bool
prop_blanks s = (length (getBlankList exampleBlankSudoku) == 81) && 
                (isSolved s) == (length (getBlankList s) == 0)                                                       
prop_checkUpdate :: Puzzle -> (Maybe Int, (Int, Int)) -> Bool
prop_checkUpdate sud (_, pos)        | not (isPuzzle sud)       = True
                                     | not (validPos pos)       = True
prop_checkUpdate _ ((Just value), _) | value >= 1 || value <= 9 = True
prop_checkUpdate sudoku (value, pos) = getValue (update sudoku (value, pos)) pos == value

exampleZero :: Puzzle
exampleZero =
  Puzzle [[Nothing,Just 6,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Just 7,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Just 8,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Just 7,Just 3,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]
  
example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

badExample :: Puzzle
badExample =
  Puzzle
    [ [Just 6, Nothing, Just 4, Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

exampleBlankSudoku :: Puzzle
exampleBlankSudoku = Puzzle (replicate 9 [Nothing | x <- [1..9]])