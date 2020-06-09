module InputParser where

import           Data.Foldable (foldl')
import           Text.Parsec
import           Text.Parsec.Char (digit)
import           Text.Parsec.Error

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
