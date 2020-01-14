module Solver where
import Board
import Check
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import System.Console.ANSI
import Test.QuickCheck
import System.Exit (exitSuccess, exitWith)

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
-- given a puzzle and a blank position in the puzzle, a list with some inputs that are possible valid inputs
validInputs :: Puzzle -> (Int, Int) -> [Maybe Int]
validInputs p pos = map Just (validInputs' p pos)
    where
        validInputs' :: Puzzle -> (Int, Int) -> [Int]
        validInputs' p pos = filter (\x -> allHasNoDuplicates $ (update p (Just (x), pos))) [1..9]


counts :: Puzzle -> BlankList -> [Int]
counts puzzle  = foldr (\b acc -> length((validInputs puzzle b)):acc) [] 
-- for a solver, firstly consider the blanks that only have 1 exact valid solution, and update the puzzle 
-- then consider the blanks that has 2 possible valid input (but not)
nextInput :: Puzzle -> BlankList -> IO Puzzle
nextInput puzzle [] = return puzzle
nextInput puzzle (b:bs) = do
    let list = validInputs puzzle b
    case (length list) of
        1 -> do 
           let (Just val) = head list
           setSGR [SetColor Foreground Vivid Green]
           putStrLn (" Value " ++ show val ++ " with location " ++ show b ++ " was added")
           setSGR [Reset]

           return (update puzzle (Just val, b))
        _ -> nextInput puzzle bs

allPossiblePuzzles :: Puzzle -> [Puzzle]
allPossiblePuzzles puzzle = concatMap allPossiblePuzzles $ [update puzzle (v, b) |b <- getBlankList puzzle, v <- validInputs puzzle b] 


isSolved :: Puzzle -> Bool
isSolved puzzle = allHasNoDuplicates puzzle && (countBlanks puzzle == 0)

inValid :: Puzzle -> Bool
inValid p = not (isSolved p)

multisolve :: Puzzle -> [Puzzle]
multisolve s = filter (isSolved) (allPossiblePuzzles s)

solver2 :: Puzzle -> Puzzle
solver2 = head . multisolve
hint :: Puzzle -> IO Puzzle 
hint puzzle = do
    let blankList = getBlankList puzzle

    case ((length blankList) > 0) of 
        True -> nextInput puzzle blankList
        
              
        False -> return puzzle

solver :: Puzzle -> IO Puzzle
solver puzzle = do
    let blankList = getBlankList puzzle
    case ((length blankList) > 0) of 
        True -> do
            p <- hint puzzle
            solver p
        False -> return puzzle



