module Main where
import Board
import Check
import Solver
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex)
import Control.Monad (liftM, replicateM_)
import Test.QuickCheck
import Text.Read
import System.Random
import System.Console.ANSI
import System.Exit (exitSuccess, exitWith)

main :: IO ()
main = do 
         initialP <- initialPuzzle
         gameUpdate initialP " "

initialPuzzle :: IO Puzzle
initialPuzzle = do
         setSGR [SetColor Foreground Vivid Green] 
         putStrLn "\nWelcome to Sudoku! \nWhat level of difficulty would you like to play? (Input 'easy' or 'hard') "
         setSGR [Reset]
         n <- getLine 
         case n of 
          "easy" -> do 
            indexEasy <- randomRIO (1, 50) :: IO Int
            putStrLn ("\n Mode: Easy; Index: " ++ show indexEasy)
            puzzle <- readPuzzle ("puzzles/easy" ++ show indexEasy ++ ".sud")
            return puzzle
            
          "hard" -> do 
            indexHard <- randomRIO (1, 95) :: IO Int
            putStrLn ("\n Mode: Hard; Index: " ++ show indexHard)
            puzzle <- readPuzzle ("puzzles/hard" ++ show indexHard ++ ".sud")
            return puzzle
  
          _ -> initialPuzzle

readPuzzle :: FilePath -> IO Puzzle 
readPuzzle f = do 
               inputPuzzle <- readFile f
               return (Puzzle ((map.map) (\x -> case (isDigit x) of
                                 True -> Just (digitToInt x)
                                 False -> Nothing) (lines inputPuzzle)))  


gameUpdate :: Puzzle -> String-> IO()
gameUpdate puzzle s = do 
                      setSGR [SetColor Foreground Dull Red]
                      putStr s
                      setSGR [Reset]
                      
                      putStrLn " Here are the positions of the blanks that need to be filled in: "
                      printPuzzle puzzle
                      putStrLn ""
                      showBlanks puzzle 
                      let countBlank = countBlanks puzzle

                      newGOrQuit <- newGameOrQuit puzzle

                      getHint puzzle
                      getSolution puzzle
                      

                      puzzle1 <- nextPuzzle puzzle
                      putStrLn "\n Done. Here's the updated puzzle:"

                      case allHasNoDuplicates puzzle1 of 
                        True -> case countBlank of 
                          0 -> do 
                                 putStrLn "\nGame Done"
                                 newGameOrQuit puzzle1
                          _ -> gameUpdate puzzle1 ""

                        False -> do 
                                    setSGR [SetColor Foreground Vivid Red]
                                    printPuzzle puzzle1
                                    gameUpdate puzzle "\nIncorrect input with duplications, please redo it\n\n" 


nextPuzzle :: Puzzle -> IO Puzzle
nextPuzzle puzzle = do 
                  n <- getInputValue
                  inp <- getInputIndex puzzle  
                  let playerInput = (Just n, inp)
                  return (update puzzle playerInput)

getHint ::Puzzle -> IO()
getHint puzzle = do 
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn "\n  Do you want a hint? (type 'hint' for a help)"
          setSGR [Reset]
          h <- getLine
          case h of 
            "hint" -> do
                       newPuzzle <- hint puzzle
                       putStrLn "\n Done. Here's the updated puzzle:"
                       gameUpdate newPuzzle ""
                                    
            _ -> putStr ""

getSolution :: Puzzle -> IO()
getSolution puzzle = do 
                      setSGR [SetColor Foreground Vivid Blue]
                      putStrLn "\n  View solution? (type 'solver' for the complete solution)"
                      setSGR [Reset]
                      inp1 <- getLine
                      case inp1 of 
                        "solver" -> do 
                                  puzzle <- solver puzzle
                                  putStrLn "\n Here is a solution:"
                                  printPuzzle puzzle
                                  newGameOrQuit puzzle

                        _ -> putStr ""



newGameOrQuit :: Puzzle -> IO()
newGameOrQuit puzzle = do 
            setSGR [SetColor Foreground Vivid Blue]
            putStrLn "\nDo you want to restart a new Game or quit?" 
            putStrLn "  Press 'new' for a new game" 
            putStrLn "  Press 'quit' to quit"
            putStrLn "  Press any keys to stay in " 
            setSGR [Reset]
            ans <- getLine
            case ans of   
              "new" -> main
              "quit" -> quitGame 
              _ -> putStr ""

quitGame :: IO()
quitGame = do 
             putStr "\n See you next time!\n"
             exitSuccess 


getInputIndex :: Puzzle -> IO (Int, Int)
getInputIndex puzzle = do 
            setSGR [SetColor Foreground Vivid Blue]
            putStrLn "Which blank would you like to fill in?"
            putStrLn "   Input row number and press 'Enter'"
            setSGR [Reset]

            row <- getLine

            setSGR [SetColor Foreground Vivid Blue]
            putStrLn "   Input column number and press 'Enter'"
            setSGR [Reset]

            col <- getLine
            case ((length row == 1) && (length col == 1)) of 
               True -> do 
                          let valR = head(row)
                          let valC = head(col)

                          case ((isDigit valR) && (isDigit valC)) of 
                            True -> case ( ((digitToInt valR), (digitToInt valC)) `elem` (getBlankList puzzle) ) of
                                    
                                    True -> return ((digitToInt valR), (digitToInt valC))
                                    False -> getInputIndex puzzle
                            
                            False -> getInputIndex puzzle
               False -> getInputIndex puzzle

getInputValue :: IO Int
getInputValue = do 
                  setSGR [SetColor Foreground Vivid Blue]
                  putStrLn "What number (1-9) do you want to place here?"
                  setSGR [Reset]
                  
                  [n] <- getLine
                  
                  case (isDigit n) of
                    True -> if ((digitToInt n) >= 1 && (digitToInt n) <= 9) 
                                 then return (digitToInt n) 
                                 else getInputValue
                    False -> getInputValue



    

    