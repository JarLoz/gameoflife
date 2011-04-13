--type Xcrd = Int
--type Ycrd = Int
--data Status = Alive | Dead deriving (Show)

data Cell = Cell { xCrd :: Int
                 , yCrd :: Int
		 , status :: Bool
		 }deriving (Show,Eq,Ord)

type Grid = [Cell]


--Xcrd (Cell X _ _) = X

makeGrid :: Int -> Int -> Grid
makeGrid x y = map makeCell (makePermutations [1..x] [1..y] [])

makeCell :: (Int,Int) -> Cell
makeCell (x,y) = Cell x y False

makeOnePermutation :: [Int] -> Int -> [(Int,Int)]
makeOnePermutation xs x = [(x, a) | a <- xs]

makePermutations :: [Int] -> [Int] -> [(Int,Int)] -> [(Int,Int)]
makePermutations [] _ result = result
makePermutations (x:xs) ys result = makePermutations xs ys (result++(makeOnePermutation ys x))
