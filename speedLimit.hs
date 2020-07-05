-- stack --system-ghc runghc
-- Problem set: https://open.kattis.com/problems/speedlimit/submit
-- Status: ACCEPTED

module Main where

import           Data.Foldable (traverse_)
import           Text.Parsec

type Testcase = [(Speed, Time)]
type Speed = Int
type Time = Int

type Solution = Miles
type Miles = Int

-- Compute total distance travelled
main :: IO ()
main = do
    s <- getContents
    testcases <- case parse inputP "" s of
        Left e           -> error $ show e
        Right testcases' -> return testcases'
    let solutions = solve <$> testcases
    traverse_ (\solution -> putStrLn $ show solution ++ " miles") solutions

solve :: Testcase -> Solution
solve t = fst $ foldl (\(s, t1) (v, t2) -> (s + (v * (t2 - t1)), t2)) (0, 0) t

inputP :: Parsec String () [Testcase]
inputP = do
    -- | List of testcases
    ts <- many1 (testcaseP <* newline)
    _ <- string "-1"
    return ts
  where
    testcaseP :: Parsec String () Testcase
    testcaseP = do
        n <- int
        count n (newline *> logRowP)

    logRowP :: Parsec String () (Speed, Time)
    logRowP = do
        speed <- int -- ^ Speed
        _ <- space
        timeElapsed <- int -- ^ Time elapsed
        return (speed, timeElapsed)

    int :: Parsec String () Int
    int = read <$> many1 digit
