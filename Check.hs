module Check where
import Board
import Data.Char
import Data.List
import Data.Maybe

-- | block should not have duplicates
hasNoDuplicates :: Block -> Bool
hasNoDuplicates b = hasNoDuplicates' b []
    where
        hasNoDuplicates' :: Block -> Block -> Bool
        hasNoDuplicates' [] _ = True
        hasNoDuplicates' (x:xs) y | (isJust x) && (x `elem` y) = False
                                  | otherwise = hasNoDuplicates' xs (x:y)

createBlocks :: Puzzle -> [Block]
createBlocks s = rows s ++ cols s ++ boxes s 
    where cols s = transpose (rows s)
          boxes s = getBoxes (rows s)

getBoxes :: [Block] -> [Block]
getBoxes [] = []
getBoxes rows = concat (makeBox (take 3 rows) : [getBoxes (drop 3 rows)])


makeBox :: [Block] -> [Block]
makeBox [[],[],[]] = []
makeBox [a,b,c] = concatMap (take 3) [a,b,c] : makeBox (map (drop 3) [a,b,c])

-- | entire puzzle should not have any blocks that have duplicates
allHasNoDuplicates :: Puzzle -> Bool
allHasNoDuplicates p = and [hasNoDuplicates b | b <- (createBlocks p)]


-- | check if we have a valid Sudoku puzzle
-- puzzle should be 9x9 
-- each block, row, block should have nonduplicate 1-9
isPuzzle :: Puzzle -> Bool
isPuzzle (Puzzle p) = length p == 9 && 
                        checkNine p && 
                        checkRange p

checkNine :: [[Maybe Int]] -> Bool
checkNine l = and [(length a == 9) | a<-l]

checkRange :: [[Maybe Int]] -> Bool
checkRange l = and [inRange a | a<-l]

inRange :: [Maybe Int] -> Bool
inRange = all inRange'
   
inRange' :: Maybe Int -> Bool
inRange' Nothing = True
inRange' (Just n) = n `elem` [1..9] 