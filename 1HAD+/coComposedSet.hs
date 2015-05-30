-- Problem:  http://lpaste.net/845208190432837632
-- Solution: http://lpaste.net/129860

import Data.List
import Data.Numbers.Primes

coComposedSet :: Integer -> [[Integer]]
coComposedSet s =
    [r | a <- [1 .. ], let r = [a .. a + s - 1],
     (== s) $ genericLength $ group [x | x <- r, y <- r, y /= x, gcd x y > 1]]

-- get all possible layouts of numbers with common factor m inside
-- an arbitrary contiguous range of s numbers; each layout must contain
-- at least 2 elements
coComposedLayouts :: Int -> Int -> [[Int]]
coComposedLayouts m s =
    filter ((> 1) . length . take 2) $
    map (takeWhile (<= s) . iterate (+ m)) [1 .. m]

-- get union of all possible combinations of numbers with common prime factors
-- 2, 3, 5, 7, 11 and 13 inside an arbitrary contiguous range of s numbers
coComposedPrimeLayouts13 :: Int -> [[Int]]
coComposedPrimeLayouts13 s =
    map (sort . concat)
    [a:b:c:d:e:[f] | a <- coComposedLayouts 2  s, b <- coComposedLayouts 3  s,
                     c <- coComposedLayouts 5  s, d <- coComposedLayouts 7  s,
                     e <- coComposedLayouts 11 s, f <- coComposedLayouts 13 s]

-- test if an arbitrary contiguous range of s numbers may have full co-composed
-- set of numbers, maximum tested prime layout is 13: this is enough for ranges
-- up to 17 numbers
hasCoComposedSet13 :: Int -> Bool
hasCoComposedSet13 s =
    elem s $ map (length . group) $ coComposedPrimeLayouts s

-- generalized coComposedPrimeLayouts13 with run-time calculation of
-- prime factors, requires import Data.Numbers.Primes
coComposedPrimeLayouts :: Int -> [[Int]]
coComposedPrimeLayouts s =
    map (sort . concat) $ mapM (`coComposedLayouts` s) $ takeWhile (< s) primes

--- same as hasCoComposedSet13 but uses generalized coComposedPrimeLayouts
hasCoComposedSet :: Int -> Bool
hasCoComposedSet s =
    elem s $ map (length . group) $ coComposedPrimeLayouts s

main :: IO ()
main = do
    print $ take 10 $ coComposedSet 17
    -- test arbitrary number ranges of sizes 2 .. 17 for possible
    -- co-composed sets
    print $ map hasCoComposedSet [2 .. 17]

