-- Problem:  http://leetcode.com/problems/product-of-array-except-self/

import Data.List
import Control.Arrow

prods :: Num a => [a] -> [a]
prods = prod mapAccumL &&& prod mapAccumR >>> uncurry (zipWith (*))
    where prod fac = snd . fac (\a x -> (x * a, a)) 1

main = print $ prods [1 .. 4]

