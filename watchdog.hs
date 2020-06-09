-- stack --system-ghc runghc
{-# LANGUAGE GADTs #-}

module Main where

import           Data.Foldable (foldl')
import           GHC.Generics
import           Text.Parsec
import           Text.Parsec.Char (digit)
import           Text.Parsec.Error

-- =========
-- = TYPES =
-- =========

type Testcase = (Side, [Coord])
type Side = Int
type Coord = (X, Y)
type X = Int
type Y = Int

data Solution = Solved Coord | Poodle

instance Show Solution where
    show (Solved (x, y)) =  show x <> " " <> show y
    show Poodle = "poodle"

type Parser = Parsec String ()

-- ==================
-- = HEAP UTILITIES =
-- ==================

-- | A pairing heap
data Heap a = EmptyHeap | Ord a => Heap (Tree a)
data Tree a = Tree a [Tree a]

extractMin :: Heap a -> Maybe (a, Heap a)
extractMin cs = (,) <$> findMin cs <*> deleteMin cs

findMin :: Heap a -> Maybe a
findMin EmptyHeap = Nothing
findMin (Heap (Tree a _)) = Just a

deleteMin :: Heap a -> Maybe (Heap a)
deleteMin EmptyHeap = Nothing
deleteMin (Heap (Tree _ ts)) = Just $ mergePairs ts

mergePairs :: Ord a => [Tree a] -> Heap a
mergePairs [] = EmptyHeap
mergePairs [x] = Heap x
mergePairs (t1:t2:ts) = meld (meld (Heap t1) (Heap t2)) (mergePairs ts)

meld :: Ord a => Heap a -> Heap a -> Heap a
meld EmptyHeap h = h
meld h EmptyHeap = h
meld (Heap h1@(Tree a1 ts1)) (Heap h2@(Tree a2 ts2)) | a1 < a2 = Heap (Tree a1 (h2 : ts1))
                                                     | otherwise = Heap (Tree a2 (h1 : ts2))

insert :: Ord a => a -> Heap a -> Heap a
insert e h = meld (Heap (Tree e [])) h

-- ========
-- = MAIN =
-- ========

main :: IO ()
main = do
    s <- getContents
    ts <- case parse inputP "" s of
                   Left t    -> error $ show t
                   Right res -> return res

    -- | Verify parsing
    -- putStrLn $ show ts

    -- | Verify solution
    let solutions = solve <$> ts
    putStrLn $ unlines $ show <$> solutions

-- ===========
-- = Parsers =
-- ===========

inputP :: Parsec String () [Testcase]
inputP = do
    -- | Number of testcases
    n <- int
    -- | List of testcases
    ts <- count n (newline *> testcaseP)
    return ts      
  where
    testcaseP :: Parsec String () Testcase
    testcaseP = do
        side <- int -- ^ Length of side
        space
        n <- int -- ^ Number of coordinates
        coords <- count n (newline *> pCoord) -- ^ Coordinates
        return (side, coords)
      where
        pCoord = do
            x <- int
            space
            y <- int
            return (x, y)

    newline :: Parsec String () Char
    newline = satisfy (== '\n')

    int :: Parsec String () Int
    int = read <$> many1 digit

solve :: Testcase -> Solution
solve (_, []) = Poodle -- ^ This shouldn't happen, length cs > 0
solve (s, (h:hs)) = go s hs (fromList $ genInitial h s)
  where
    -- | If we have checked all the hatches, use the hatch with the minimum coordinates
    go side [] leashPositions =
        case findMin leashPositions of
            Nothing -> Poodle
            Just c -> Solved c
    go side (hatch:hs') leashPositions = go side cs' (filterHeap (isValid hatch side) leashPositions)

    -- | Get possible leash positions from the first hatch coordinates
    genInitial hatch side = filter (isValid hatch side) [(x, y) | x <- [0..side], y <- [0..side]]

    -- | Check for a given (hx, hy), s, whether we can use (x, y) as the leash position
    -- 1) We set the radius of the leash to be equal to the distance between leash position and the nearest side
    -- 2) If distance between hatch, leash position <= radius, it is valid
    isValid (hx, hy) side (x, y) = distanceBetween (hx, hy) (x, y) <= radius
      where
        distanceBetween (x1, y1) (x2, y2) = ceiling $ sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
        radius = minimum [side - x, side - (side - x), side - y, side - (side - y)]

fromList :: Ord a => [a] -> Heap a
fromList [] = EmptyHeap
fromList (x:xs) = insert x $ fromList xs
