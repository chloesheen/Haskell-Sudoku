module Board where
import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck
import Control.Monad
-- Sudoku puzzle is implemented as a list of lists
data Puzzle = Puzzle {rows :: [[Maybe Int]]}
    deriving (Show, Eq)

type Block = [Maybe Int]
type BlankList = [(Int, Int)]

-- prints a representation of the sudoku sud on the screen
showVal :: Maybe Int -> String
showVal Nothing = " "
showVal (Just n) = show n

showRows :: [[Maybe Int]] -> String
showRows [] = " - - - - - - - - - - - - - - - - - - -" ++ "\n"
showRows (x:xs) =  " - - - - - - - - - - - - - - - - - - -" ++ "\n"
                   ++ showRow x ++ "\n" ++ showRows xs
  
  where showRow [] = " | "
        showRow (x:xs) = " | " ++ showVal x ++ showRow xs

printPuzzle :: Puzzle -> IO ()
printPuzzle (Puzzle rs) = putStr $ showRows rs  

validPos :: (Int, Int) -> Bool
validPos (row, col) | row > 9 || col > 9 || row <= 0 || col <= 0 = False
                    | otherwise = True
                  

getValue :: Puzzle -> (Int, Int) -> Maybe Int
getValue (Puzzle l) (row,col) = ( l !! row) !! col

-- Puzzle Update 
update :: Puzzle -> (Maybe Int, (Int, Int)) -> Puzzle
update (Puzzle p) (inp, (x, y)) = Puzzle (set' p (inp, (x, y))) where
          
          set' :: [[Maybe Int]] -> (Maybe Int, (Int, Int)) -> [[Maybe Int]]
          set' (row : rs) (inp, (1, y)) = (set'' row (inp, y)) : rs where
                        set'' :: [Maybe Int]-> (Maybe Int, Int) -> [Maybe Int]
                        set'' (x:xs) (inp, 1) = inp : xs
                        set'' (x:xs) (inp, y) = x : set'' xs (inp, y-1)
          set' (row : rs) (inp, (x, y)) = row : set' rs (inp, (x-1, y))
          
-- Blanks Position 

-- given a puzzle that has not yet been solved, return positions that is still blank
getBlankList :: Puzzle -> BlankList
getBlankList (Puzzle p) = map snd (filter isBlank (zip (concat p) [(x,y) | x <- [1..9], y <- [1..9]]))
 where
   isBlank :: (Maybe Int, (Int, Int)) -> Bool
   isBlank (Nothing, _) = True
   isBlank _            = False

-- count the number of blanks left in the puzzle need to be solved 
countBlanks :: Puzzle -> Int
countBlanks puzzle = length (getBlankList puzzle)

-- show all the blanks left that can be solved to the commend line interface
showBlanks :: Puzzle -> IO ()
showBlanks puzzle = putStr $ showBlankPos (getBlankList puzzle) where 
  showBlankPos :: BlankList -> String 
  showBlankPos [] = "\n"
  showBlankPos (x:[]) = "\n"
  showBlankPos (x:y:xs) = if ((fst x) == (fst y)) then show x ++ showBlankPos (y:xs) else show x ++ "\n" ++ showBlankPos (y:xs)



