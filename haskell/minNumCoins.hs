import Data.Function.Memoize (memoFix2)
import Data.IntMap.Strict (singleton, insert, (!))
import Data.List (mapAccumL)

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
minNumCoinsMap c m = (snd $ mapAccumL (mnc c) (singleton 0 0) [0 .. m]) !! m
    where mnc c s m = foldl step (s, m) $ filter (<= m) c
            where step a x = let cur = min (s ! (m - x) + 1) $ snd a
                             in (insert m cur s, cur)

main = do
    {-print $ minNumCoinsPlain [1, 2, 5] 20       -- m = 50 will hang it!-}
    {-print $ minNumCoins_     [1, 2, 5] 20       -- m = 50 will hang it!-}
    {-print $ minNumCoins      [1, 2, 5] 10002-}
    {-print $ minNumCoinsMemo  [1, 2, 5] 10001-}
    {-print $ minNumCoinsMap   [1, 2, 5] 10001-}
    {-print $ minNumCoins      [24,18,14,11,10,8,5,3,1] 16659-}
    {-print $ minNumCoinsMap   [24,18,14,11,10,8,5,3,1] 16659-}
    print $ minNumCoinsMap   [24,18,14,11,10,8,5,3,1] 1665900

