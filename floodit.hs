-- stack --system-ghc runghc
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{- |
Result of pair programming with [bumbleblym](https://github.com/bumbleblym)

He implemented most of the code below and suggested using parsers from base
rather than @getLine@ & @read@ to parse the input.
This increases code modularity and ease of refactoring.

The most interesting part would be the @countSepBy@ function.

__Original problem__

https://open.kattis.com/problems/floodit

You'll have to look through this to get a rough idea of what we're parsing


__Script below doesn't actually solve the problem, just parses inputs__

It is interesting to see how one can bootstrap a parser using built-ins to __base__.

__test parsing with:__
> cat input | stack floodit.hs

__sample input__ (copy this to a file to pipe it in)
4
6
123423
334521
433123
543621
324343
234156
5
12121
21212
12121
21212
12121
5
12345
12345
12345
12345
12345
5
11131
12211
31311
21111
11111

__sample output__
(6,["123423","334521","433123","543621","324343","234156"])
(5,["12121","21212","12121","21212","12121"])
(5,["12345","12345","12345","12345","12345"])
(5,["11131","12211","31311","21111","11111"])

-}
module Main 
    ( main
    ) where

import Control.Monad (replicateM, void)
import qualified Data.Char as Char
import Data.Foldable (foldl')
import Data.Kind (Type) 
import GHC.TypeNats
import Text.ParserCombinators.ReadP

data TestCase = TestCase 
    { getSize :: Int
    , getGrid :: Grid
    } deriving (Show)

type Solution = TestCase

type Color = Char
type Grid = [[Color]]

data SomeVector a where
    SomeVector :: KnownNat n => Vector n a -> SomeVector a

newtype Vector (n :: Nat) (a :: Type) = Vector 
    { unVector :: [a]
    }

-- | Parse the input to a list of test cases
parse :: String -> [TestCase]
parse s = fst $ head $ readP_to_S inputP s
    where
        -- Parse test cases
        inputP :: ReadP [TestCase]
        inputP = do
            -- Parse number of test cases
            numTestCases <- numTestCasesP <* newline

            -- Parse test cases
            countSepBy numTestCases newline $ do
                size <- sizeP <* newline
                grid <- gridP size
                pure $ TestCase size grid

        numTestCasesP = int
        sizeP = int

        gridP :: Int -> ReadP Grid
        gridP n = countSepBy n newline rowP
            where
                rowP = count n $ satisfy (`elem` "123456")

        -- | @countSepBy n sep p@ parses @n@ occurrences of @p@ in sequence, separated by @sep@. A list of results is returned. 
        countSepBy :: Int -> ReadP sep -> ReadP a -> ReadP [a]
        countSepBy n sep p = (:) <$> p <*> replicateM (n - 1) (sep *> p)

        newline :: ReadP Char
        newline = satisfy (== '\n')

        int :: ReadP Int
        int = f <$> munch1 Char.isDigit
            where
                f :: String -> Int
                f = foldl' (\n x -> n * 10 + fromEnum x - fromEnum '0') 0

solve :: TestCase -> Solution
solve = id

printSolution :: Solution -> IO ()
printSolution = print

main :: IO ()
main = do
    inp <- getContents
    mapM_ (printSolution . solve) $ parse inp