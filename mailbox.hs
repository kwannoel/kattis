-- stack --system-ghc runghc
-- Problem set: https://open.kattis.com/problems/mailbox/submit
-- Status:


module Main where

import           Text.Parsec

-- ===========
-- = DETAILS =
-- ===========

-- Given variables
-- Number of mailbox prototypes, k: 1 <= k <= 10
-- Cracker capacity of each prototypes, m: 1 <= m <= 100

-- Figure out
-- How many crackers his mailboxes can withstand

type Testcase = (NumMailbox, CrackerCapacity)
type NumMailbox = Int
type CrackerCapacity = Int

type Solution = NumCrackers -- ^ Number of crackers needed
type NumCrackers = Int

inputP :: Parsec String () [Testcase]
inputP = do
    -- | Number of testcases
    n <- int
    -- | List of testcases
    count n (newline *> testcaseP)
  where
    testcaseP :: Parsec String () Testcase
    testcaseP = do
        k <- int -- ^ Number of mailbox prototypes
        _ <- space
        m <- int -- ^ Cracker capacity
        return (k, m)

    int :: Parsec String () Int
    int = read <$> many1 digit

solve :: Testcase -> Solution
solve (k, m) | k < 1 || k > 10 || m < 1 || m > 100 = error "Invalid input"
             | otherwise = maximum $ partition k 1 m

-- =========
-- = STEPS =
-- =========
-- 1. Find the midpoint for the sum of crackers needed
--    E.g. 1, 2, 3, 4, 5
--    Total Sum is 15, midpoint is 3
--    1 + 2 + 3 = 6
--    4 + 5 = 9
--    These are closest we can get to evenly splitting 1, 2, 3, 4, 5 at 7.5
--    E.g. 1, 2, 3, 4, 5, 6
--    1 + 2 + 3 + 4 = 10
--    5 + 6 = 11
--    Even split is 10.5
--    E.g. 1, 2, 3, 4, 5, 6, 7
--    1 + 2 + 3 + 4 + 5 = 15
--    6 + 7 = 13
--    Even split is 14
-- 2. Partition on the side with the larger amount
-- 3. Repeat 1, 2 recursively until we are left with one mailbox
--    When that happens, do not run step 1, 2, just return the number of crackers

partition :: NumMailbox      -- ^ Number of mailboxes we have left
          -> Int             -- ^ start
          -> Int             -- ^ end (Initial value will be the capacity)
          -> [NumCrackers]   -- ^ Number of crackers we will need
partition k start end = case k of
    1 -> [arithmeticSum end - arithmeticSum (start - 1)]
    _ ->    partition (k - 1) start midpoint
         <> partition (k - 1) (midpoint + 1) end
    where
      midpoint = midpointSolver start end

-- takes in a start and end
-- equation: m^2 / 2 + m / 2 - midpart = 0
midpointSolver :: Int -> Int -> Int
midpointSolver start end = m
  where
    m = round $ max r1 r2
    r1, r2 :: Double
    (r1, r2) = (-b + det, -b - det)
    a = 1.0 / 2.0 :: Double
    b = 1.0 / 2.0 :: Double
    c = midpart
    det = sqrt (b ^ (2 :: Integer) + 4 * a * c)
    midpart = fromIntegral (arithmeticSum end + arithmeticSum (start - 1)) / 2

arithmeticSum :: Int -> Int
arithmeticSum m = m * (m + 1) `div` 2

main :: IO ()
main = do
    s <- getContents
    ts <- case parse inputP "" s of
                   Left t    -> error $ show t
                   Right res -> return res

    putStrLn $ unlines $ show . solve <$> ts
