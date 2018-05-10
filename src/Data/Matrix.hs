module Data.Matrix (
  Dim(..),
  matrix,
  fromList,
  toList,
  rows,
  row,
  columns,
  column,
  transpose,
  transposed,
  (|*|),
  (|+|)
) where

newtype Matrix a = Matrix [[a]] deriving (Show, Read, Eq)
newtype Vector a = Vector [a] deriving (Show, Read, Eq)
data Dim = Dim Int Int deriving (Show, Read, Eq)

toList :: Matrix a -> [[a]]
toList m = case m of Matrix a -> a

fromList :: [[a]] -> Matrix a
fromList [] = Matrix [[]]
fromList xs = if all (== (length $ head xs)) (map length xs) then Matrix xs else Matrix [[]]

matrix :: Int -> Int -> (Int -> Int -> a) -> Matrix a
matrix 0 _ _ = Matrix [[]]
matrix _ 0 _ = Matrix [[]]
matrix r c f = Matrix [[f x y | y <- [1..c]] | x <- [1..r]]

rows :: Matrix a -> Int
rows m = length $ toList m

row :: Matrix a -> Int -> [a]
row m n = toList m !! n

columns :: Matrix a -> Int
columns m = length $ head $ toList m

column :: Matrix a -> Int -> [a]
column m n = map (!! n) (toList m)

transpose :: Matrix a -> Matrix a
transpose (Matrix [[]]) = Matrix [[]]
transpose m = Matrix (map (column m) [0..n]) where n = columns m - 1

transposed :: Matrix a -> Matrix a -> Bool
transposed (Matrix [[]]) _ = False
transposed _ (Matrix [[]]) = False
transposed a b = rows a == columns b || rows b == columns a

dim :: Matrix a -> Dim
dim m = Dim (rows m) (columns m)

(*+*) :: Num a => [a] -> [a] -> a
a *+* b = sum $ zipWith (*) a b

(|*|) :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
Matrix [[]] |*| _ = Matrix [[]]
_ |*| Matrix [[]] = Matrix [[]]
a |*| b = if transposed a b then Matrix [[row a n *+* column b t | t <- [0..s]] | n <- [0..l]] else Matrix [[]]
  where
    l = rows a - 1
    s = columns b - 1

(|+|) :: Num a => Matrix a -> Matrix a -> Matrix a
Matrix [[]] |+| _ = Matrix [[]]
_ |+| Matrix [[]] = Matrix [[]]
a |+| b = if dim a == dim b then Matrix $ zipWith (zipWith (+)) (toList a) (toList b) else Matrix [[]]


