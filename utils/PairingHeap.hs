{-# LANGUAGE GADTs #-}
module PairingHeap where

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
