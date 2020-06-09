-- stack --system-ghc runghc
-- Problem set: https://open.kattis.com/problems/watchdog/submit
-- Status: ACCEPTED

{-# LANGUAGE GADTs #-}

module Main where

import           Data.Foldable (foldl')
import           Data.List((\\))
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
solve (s, hatches@(h:hs)) = go s hs (genInitial h s \\ hatches)
  where
    -- | If we have checked all the hatches, use the hatch with the minimum coordinates
    go side [] [] = Poodle
    go _ [] leashPositions = Solved $ minimum leashPositions
    go side (hatch:hs') leashPositions = go side hs' (filter (isValid hatch side) leashPositions)

    -- | Get possible leash positions from the first hatch coordinates
    genInitial hatch side = filter (isValid hatch side) grid
      where
        grid = [(x, y) | x <- [0..side], y <- [0..side]]

    -- | Check for a given (hx, hy), s, whether we can use (x, y) as the leash position
    -- 1) We set the radius of the leash to be equal to the distance between leash position and the nearest side
    -- 2) If distance between hatch, leash position <= radius, it is valid
    isValid (hx, hy) side (x, y) = distanceBetween (hx, hy) (x, y) <= radius
      where
        distanceBetween (x1, y1) (x2, y2) = ceiling $ sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
        radius = minimum [side - x, side - (side - x), side - y, side - (side - y)]
