-- stack --system-ghc runghc
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Applicative
import Control.Monad (replicateM, void)
import qualified Data.Char as Char
import Data.Foldable (foldl')
import Data.Kind (Type) 
import Data.Proxy
import Data.Typeable
import GHC.TypeNats
import Text.ParserCombinators.ReadP

data TestCase = TestCase 
    { getSize :: Int
    , getGrid :: Grid
    } deriving (Show)

type Solution = TestCase

type Color = Char
type Grid = [[Color]]

data SomeGrid a where
    SomeGrid :: KnownNat n => Vector n (Vector n a) -> SomeGrid a

data SomeVector a where
    SomeVector :: KnownNat n => Vector n a -> SomeVector a

newtype Vector (n :: Nat) (a :: Type) = Vector 
    { unVector :: [a]
    }

mkSomeGrid :: KnownNat n => [Vector n a] -> Maybe (SomeGrid a)
mkSomeGrid xs = case mkSomeVector xs of
    SomeVector (v :: Vector m (Vector n a)) -> case sameNat (Proxy @m) (Proxy @n) of
        Nothing -> Nothing
        Just Refl -> Just $ SomeGrid v

mkSomeVector :: forall a . [a] -> SomeVector a
mkSomeVector xs = case someNatVal (fromIntegral $ length xs) of 
    SomeNat (_ :: Proxy n) -> SomeVector (Vector xs :: Vector n a)

someGridP :: Int -> ReadP (SomeGrid Color)
someGridP n = case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy n) -> 
        let rowP :: ReadP (Vector n Color)
            rowP = do
                xs <- count n $ satisfy (`elem` "123456")
                case mkSomeVector xs of
                    SomeVector (v :: Vector m a) -> case sameNat (Proxy @m) (Proxy @n) of
                        Nothing -> empty
                        Just Refl -> pure v

            gridP :: ReadP (SomeGrid Color)
            -- gridP = SomeVector <$> (undefined :: Vector n (Vector n Color))
            gridP = undefined
        
        in gridP

class Functor f => Applicative f where
    pure :: a -> f a
    pure x = const x <$> unit
    (<*>) :: f (a -> b) -> f a -> f b
    f <*> a = (\(f, a) -> f a :: (a -> b, a) -> b) <$> (fzip f a :: f (a -> b, a))

type a * b = (a, b) -- unit = ()
type a + b = Either a b -- unit = Void

class Functor f where
    map :: (a -> b) -> (f a -> f b)

    fmap :: (a -> b) -> (f a -> f b)
    fmap = map

class Functor f => Apply f where
    product :: f a * f b -> f (a * b)

    (<*>) :: f (a -> b) -> f a -> f b
    f <*> a = uncurry ($) <$> product (f, a)

class Apply f => Applicative f where
    one :: f ()

    pure :: a -> f a
    pure x = const x <$> one

class Functor f => Alt f where
    sum :: f a * f b -> f (a + b)

    (<|>) :: f a -> f a -> f a
    x <|> y = either id id <$> sum (x, y)

class Alt f => Plus f where
    zero :: f Void

    empty :: f a
    empty = absurd <$> zero

class (Applicative f, Plus f) => Alternative f

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