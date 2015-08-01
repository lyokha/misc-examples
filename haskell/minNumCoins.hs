-- compile hint: ghc --make -O2 -fforce-recomp -fllvm -optlo-O3 minNumCoins.hs
-- for criterion benchmarking use option -b

import           Data.Function.Memoize       (memoFix2)
import           Data.IntMap.Strict          (singleton, insert, split, (!))
import           Data.List                   (mapAccumL, sortBy, delete)
import           Data.Array.IO
import           Data.Array.Base             (unsafeRead, unsafeWrite)
import           Control.Monad               (foldM, foldM_, liftM)
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as VI
import           Criterion.Main
import           System.Environment

minNumCoinsPlain :: [Int] -> Int -> Int
minNumCoinsPlain c m = map (mnc c) [0 .. m] !! m
    where mnc c m = foldl (\a x -> min (minNumCoinsPlain c (m - x) + 1) a) m $
                    filter (<= m) c

minNumCoins_ :: [Int] -> Int -> Int
minNumCoins_ c = (map (mnc c) [0 ..] !!)
    where mnc _ 0 = 0
          mnc c m = foldl (\a x -> min (minNumCoins_ c (m - x) + 1) a) m $
                    filter (<= m) c

minNumCoins :: [Int] -> Int -> Int
minNumCoins c =
    let minNumCoinsMemo = (map (mnc c) [0 ..] !!)
        mnc _ 0 = 0
        mnc c m = foldl (\a x -> min (minNumCoinsMemo (m - x) + 1) a) m $
                  filter (<= m) c
    in minNumCoinsMemo

minNumCoinsMemo :: [Int] -> Int -> Int
minNumCoinsMemo = memoFix2 mnc
    where mnc _ _ 0 = 0
          mnc f c m = foldl (\a x -> min (f c (m - x) + 1) a) m $
                      filter (<= m) c

minNumCoinsMap :: [Int] -> Int -> Int
minNumCoinsMap c m = snd (mapAccumL (mnc c) (singleton 0 0) [0 .. m]) !! m
    where mnc c s m = foldl step (s, m) $ filter (<= m) c
            where step a x = let cur = min (s ! (m - x) + 1) $ snd a
                             in (insert m cur (snd $ split (m - mc) s), cur)
                    where mc = maximum c

minNumCoinsArray :: [Int] -> Int -> IO Int
minNumCoinsArray c m = do
    a <- newArray (0, m) 0 :: IO (IOUArray Int Int)
    mapM_ (mnc cs a) [1 .. m]
    unsafeRead a m
    where cs = sortBy (flip compare) c
          mnc :: [Int] -> IOUArray Int Int -> Int -> IO ()
          mnc c a m = foldM (step a) m (dropWhile (> m) c) >>= unsafeWrite a m
            where step a b x = liftM (min b . succ) $ unsafeRead a $ m - x

minNumCoinsVector :: [Int] -> Int -> IO Int
minNumCoinsVector c m = do
    v <- V.new $ m + 1
    V.unsafeWrite v 0 0
    mapM_ (mnc cs v) [1 .. m]
    V.unsafeRead v m
    where cs = sortBy (flip compare) c
          mnc c v m = foldM (step v) m (dropWhile (> m) c) >>= V.unsafeWrite v m
            where step v a x = liftM (min a . succ) $ V.unsafeRead v $ m - x

minNumCoinsVectorBasic :: VI.Vector Int -> Int -> IO Int
minNumCoinsVectorBasic c m = do
    v <- V.new $ m + 1
    V.unsafeWrite v 0 0
    mapM_ (mnc c v) [1 .. m]
    V.unsafeRead v m
    where mnc c v m = VI.foldM_ (step v) m c
            where step v a x =
                    if x > m
                        then return a
                        else do y <- V.unsafeRead v $ m - x
                                let cur = y + 1
                                if cur < a
                                    then do V.unsafeWrite v m cur
                                            return cur
                                    else return a

main = do
    args <- getArgs
    if benchArg `elem` args
        then withArgs (delete benchArg args) $ defaultMain
             [
                 bench ("map " ++ show m2)          $ whnf
                     (minNumCoinsMap        coins) m2,
                 bench ("array " ++ show m2)        $ whnfIO $
                     minNumCoinsArray       coins  m2,
                 bench ("vector " ++ show m2)       $ whnfIO $
                     minNumCoinsVector      coins  m2,
                 bench ("vector basic " ++ show m2) $ whnfIO $
                     minNumCoinsVectorBasic vcoins m2
             ]
        else {-print $ minNumCoinsPlain [1, 2, 5] 20   -- m = 50 will hang it!-}
             {-print $ minNumCoins_     [1, 2, 5] 20   -- m = 50 will hang it!-}
             {-print $ minNumCoins      [1, 2, 5] 10002-}
             {-print $ minNumCoinsMemo  [1, 2, 5] 10001-}
             {-print $ minNumCoinsMap   [1, 2, 5] 10001-}
             {-print $ minNumCoins      coins m1-}
             {-print $ minNumCoinsMap   coins m1-}
             {-print $ minNumCoinsMap   coins m2-}
             {-minNumCoinsArray         coins m2 >>= print-}
             minNumCoinsVectorBasic   vcoins m2 >>= print
    where benchArg = "-b"
          coins    = [24, 18, 14, 11, 10, 8, 5, 3, 1]
          vcoins   = VI.fromList $ sortBy (flip compare) coins
          m1       = 16659
          m2       = 1665900

