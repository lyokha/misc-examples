-- Problem:  http://lpaste.net/6861042906254278656
-- Solution: http://lpaste.net/131309

import Data.List
import Data.Function (on)

groupOperandsOf :: (Num a, Eq a, Ord a) => (a -> a -> a) -> [a] -> [[(a, a)]]
groupOperandsOf op xs =
    map (map snd) . groupBy ((==) `on` fst) . sort $
        [(x `op` y, (x, y)) | x <- xs, y <- xs, x <= y]

solution :: (Num a, Ord a) => [a] -> [(a, a)]
solution range =
    let allSums           = groupOperandsOf (+) range
        allProducts       = groupOperandsOf (*) range
            -- Mr.P - I do not know these numbers:
        notUniqueProducts = filter ((> 1) . length . take 2) allProducts
            -- Mr.S - I know that you do not know them:
        sumsGivingAllNotUniqueProducts =
            [x | x <- allSums, all (`elem` concat notUniqueProducts) x]
            -- Mr.P - then I know these numbers:
        productsSinglets  =
            singlets notUniqueProducts $ concat sumsGivingAllNotUniqueProducts
            -- Mr.S - then I know them too:
        sumsSinglets      =
            singlets allSums $ concat productsSinglets
    in concat sumsSinglets
    where singlets a b = [y | x <- a, let y = b `intersect` x, length y == 1]

main :: IO ()
main = print $ solution [2 .. 99]

