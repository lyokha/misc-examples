-- Problem: build longest subsequences of not repeating elements in a list;
--          see also my blog post (in Russian):
--          http://lin-techdet.blogspot.ru/2015/01/haskell-maybe.html

{-# OPTIONS_HADDOCK prune, ignore-exports #-}
{-# LANGUAGE TupleSections #-}

import Data.List (mapAccumL)
import Data.Maybe (isNothing, fromJust)

-- | Builds longest sublists of not repeating elements in a list
-- 
-- For example list @[1,2,3,3,3,4,4,5,6]@ is divided onto @[[1,2,3],[4,5,6]]@
-- longest sublists of not repeating elements.
--
-- This algorithm is especially useful for increasing sequences that may have
-- repeating elements.
--
buildLongerSubseqs :: Eq a => [a] -> [[a]]
buildLongerSubseqs = filter (not . null) . build . splitAtEqualBounds
    where build []       = [[]]
          build (x : xs) = snd $ mapAccumL meltShorterAndSwap x $ xs ++ [[]]
          meltShorterAndSwap x []  = ([], x)
          meltShorterAndSwap x y
              | x `isNotShorter` y = (tail y, x) 
              | otherwise          = (y, init x)
          isNotShorter _        []       = True
          isNotShorter []       _        = False
          isNotShorter (_ : xs) (_ : ys) = isNotShorter xs ys

-- NB: we can use ([[]], N) where N is any number as an accumulator in foldr
-- because when x /= N value (x : []) : [] evaluates to just [x] and when
-- x == N the empty last element in value [x] : [[]] will be filtered later
-- by function ((>1) . length . take 2)
--
splitAtEqualBounds' :: (Num a, Eq a) => [a] -> [[a]]
splitAtEqualBounds' = filter ((>1) . length . take 2) . fst .
    foldr (\x (y@(z : zs), a) ->
        if x == a then ([x] : y, x) else ((x : z) : zs, x)) ([[]], 0)

splitAtEqualBounds'' :: Eq a => [a] -> [[a]]
splitAtEqualBounds'' [] = []
splitAtEqualBounds'' x@(z : _) = filter ((>1) . length . take 2) . fst .
    foldr (\x (y@(z : zs), a) ->
        if x == a then ([x] : y, x) else ((x : z) : zs, x)) ([[]], z) $ x

splitAtEqualBounds :: Eq a => [a] -> [[a]]
splitAtEqualBounds = filter ((>1) . length . take 2) . fst .
    foldr (\x (y@(z : zs), a) -> (,Just x) $
        if isNothing a || x == fromJust a
            then [x] : y
            else (x : z) : zs) ([[]], Nothing)

main :: IO ()
main = do
    let seqs = [[1, 2, 3, 3, 3, 4, 4, 5, 6],
                [1, 2, 3, 3, 4, 4, 5, 6],
                [1, 2, 3, 3, 4, 4, 5, 5, 6, 7],
                [1, 2, 3, 3, 4, 5, 5, 6, 7],
                [1, 1, 1, 2, 3, 3, 4, 5, 5, 6, 7],
                [1, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 8, 9]]
    mapM_ (\x -> putStr ">>> " >> print x >> putStr "    " >>
        print (buildLongerSubseqs x)) seqs

