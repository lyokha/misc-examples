-- Problem:  http://lpaste.net/6911025218794291200
-- Solution: http://lpaste.net/123691

import Data.List (mapAccumL, find)
import Data.Maybe (fromJust)

-- (digits, (low, high))
type ChampernownesBounds = (Integer, (Integer, Integer))

-- calculate positional [low, high] bounds for 1-digit, 2-digits, 3-digits, etc.
-- numbers in Champernowne's sequence [1, 2, 3, ...)
champernownesBounds :: [ChampernownesBounds]
champernownesBounds = snd $
    mapAccumL (\(n, s) x -> ((n + 1, x), (n + 1, (s + 1, x)))) (0, 0) capacities
    where capacities = snd $
            mapAccumL (\(n, s) x -> let y = n * x + s in ((n * 10, y), y))
                                                                (9, 0) [1 ..]

-- calculate Champernowne's number and digit position at given position in
-- Champernowne's sequence
champernownesNumber :: Integer -> (Integer, Integer)
champernownesNumber n = (advance `div` digits + 10 ^ (digits - 1), 
                         advance `mod` digits)
    where bounds = fromJust $
            find (\(_, (low, high)) -> n >= low && n <= high)
                                                        champernownesBounds
          digits = fst bounds
          advance = n - fst (snd bounds)

-- calculate Champernowne's digit at given position in Champernowne's sequence
champernownesDigit :: Integer -> Integer
champernownesDigit = getNthDigit . champernownesNumber
    where getNthDigit x = read [show (fst x) !! fromIntegral (snd x)] :: Integer


champernownesPart :: Integer
champernownesPart = product $ map (\n -> champernownesDigit $ 10 ^ n) [0 .. 6]

main = do
    putStrLn ">>> Champernowne's bounds"
    print $ take 8 champernownesBounds
    putStrLn ">>> Champernowne's number and digit position at given positions"
    mapM_ (\n -> print $ champernownesNumber $ 10 ^ n) [0 .. 6]
    putStrLn ">>> The answer is"
    print champernownesPart

