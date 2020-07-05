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

-- Test maximum mailbox
--

main :: IO ()
main = do
    s <- getContents
    ts <- case parse inputP "" s of
                   Left t    -> error $ show t
                   Right res -> return res

    putStrLn $ unlines $ show . solve <$> ts
