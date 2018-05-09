module Matrix (
  matrix, toList, fromList, (|*|)
) where

newtype Matrix a = Matrix [[a]] deriving (Show, Read, Eq)

toList :: Matrix a -> [[a]]
toList m = case m of Matrix a -> a

fromList :: [[a]] -> Matrix a
fromList = Matrix

matrix :: Int -> Int -> (Int -> Int -> a) -> Matrix a
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
transpose m = Matrix (map (column m) [0..n])
  where n = columns m

(*+*) :: Num a => [a] -> [a] -> a
a *+* b = sum $ zipWith (*) a b

(|*|) :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
a |*| b = if rows a /= columns b || rows b /= columns a then Matrix [[]]
  else Matrix [[row a n *+* column b t | t <- [0..s]] | n <- [0..l]]
  where
    l = rows a - 1
    s = rows b - 1
