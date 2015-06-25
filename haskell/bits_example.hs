--
--       Filename:  bits_example.hs
--
--    Description:  count numbers that differ by only 1 bit in
--                  an arbitrary list of numbers
--
--        Version:  1.0
--        Created:  22.09.2013
--
--         Author:  Alexey Radkov (), 
--        Company:  
--

module BitsExample where
import Data.Bits
import Data.List


{- simple algorithm, partition by number of set bits;
 - O(n^2);
 - applied to values with an arbitrary number of bits -}

partitionBySetBits' :: Bits a => Int -> [a] -> [[a]]
partitionBySetBits' (-1) _  = [[]]
partitionBySetBits' n xs    = fst x : partitionBySetBits' (n - 1) (snd x)
    where x = partition (\z -> popCount z == n) xs

partitionBySetBits n = tail . reverse . partitionBySetBits' n

adjacentPairs :: Bits a => ([a], [a]) -> [(a, a)]
adjacentPairs (xs, ys) =
    [(x, y) | x <- xs, y <- ys, (popCount $ x `xor` y) == 1]

nAdjacentNumbers :: (Bits a, Num a) => Int -> [a] -> Int
nAdjacentNumbers n xs =
    let adjacentSets = zip partition $ tail partition
    in foldr (+) 0 (map (length . adjacentPairs) adjacentSets)
    where partition = partitionBySetBits n xs


{- partition by number of set bits counted separately in the left and the right
 - parts of the value;
 - O(n^2);
 - applied to 8 bit values only (due to adjacentSets implementation) -}

partitionBySetBits'' :: (Bits a, Num a) => Int -> Int -> Int -> [a] -> [[a]]
partitionBySetBits'' _ _ (-1) _   = [[]]
partitionBySetBits'' k l n xs     =
    let xs' = partition (\z -> popCount (vpart k l z) == n) xs
    in fst xs' : partitionBySetBits'' k l (n - 1) (snd xs')
    where vpart k l z = z .&. (1 `rotate` l - 1) `shift` k

partitionBySetBits2' :: (Bits a, Num a) => Int -> [a] -> [[[a]]]
partitionBySetBits2' (-1) _  = [[[]]]
partitionBySetBits2' n xs    =
    let xs1 = partitionBySetBits'' 0 n2 n2 xs
    in map (partitionBySetBits'' n2 n2 n2) xs1
    where n2 = n `quot` 2

partitionBySetBits2 n xs =
    map (tail . reverse) (tail $ reverse $ partitionBySetBits2' n xs)

nAdjacentNumbers2 :: (Bits a, Num a) => [a] -> Int
nAdjacentNumbers2 xs =
    let adjacentSets = [((0, 0), (0, 1)), ((0, 0), (1, 0)), ((0, 1), (0, 2)),
                        ((0, 1), (1, 1)), ((0, 2), (0, 3)), ((0, 2), (1, 2)),
                        ((0, 3), (0, 4)), ((0, 3), (1, 3)), ((0, 4), (1, 4)),
                        ((1, 0), (1, 1)), ((1, 0), (2, 0)), ((1, 1), (1, 2)),
                        ((1, 1), (2, 1)), ((1, 2), (1, 3)), ((1, 2), (2, 2)),
                        ((1, 3), (1, 4)), ((1, 3), (2, 3)), ((1, 4), (2, 4)),
                        ((2, 0), (2, 1)), ((2, 0), (3, 0)), ((2, 1), (2, 2)),
                        ((2, 1), (3, 1)), ((2, 2), (2, 3)), ((2, 2), (3, 2)),
                        ((2, 3), (2, 4)), ((2, 3), (3, 3)), ((2, 4), (3, 4)),
                        ((3, 0), (3, 1)), ((3, 0), (4, 0)), ((3, 1), (3, 2)),
                        ((3, 1), (4, 1)), ((3, 2), (3, 3)), ((3, 2), (4, 2)),
                        ((3, 3), (3, 4)), ((3, 3), (4, 3)), ((3, 4), (4, 4)),
                        ((4, 0), (4, 1)), ((4, 1), (4, 2)), ((4, 2), (4, 3)),
                        ((4, 3), (4, 4))]
    in foldr (+) 0 (map (xyLength) adjacentSets)
    where partition  = partitionBySetBits2 8 xs
          xyLength z = let x  = fst z
                           y  = snd z
                           x1 = fst x
                           x2 = snd x
                           y1 = fst y
                           y2 = snd y
                       in length $ adjacentPairs (partition!!x1!!x2,
                                                  partition!!y1!!y2)


{- partition by number of set bits counted separately in each bit of a
 - 8 bit value;
 - O(n);
 - applied to 8 bit values only (due to adjacentSets and xyLength
 - implementations) -}

partitionBySetBits8' :: (Bits a, Num a) => Int -> [a] -> [[[[[[[[[a]]]]]]]]]
partitionBySetBits8' (-1) _  = [[[[[[[[[]]]]]]]]]
partitionBySetBits8' n xs    =
    let xs1 = partitionBySetBits'' 0 n8 n8 xs
    in let xs2 = map (partitionBySetBits'' n8 n8 n8) xs1
       in let xs3 = map (map (partitionBySetBits'' (n8 * 2) n8 n8)) xs2
          in let xs4 = map (map (map
                                (partitionBySetBits'' (n8 * 3) n8 n8))) xs3
             in let xs5 = map (map (map (map
                                (partitionBySetBits'' (n8 * 4) n8 n8)))) xs4
                in let xs6 = map (map (map (map (map
                                (partitionBySetBits'' (n8 * 5) n8 n8))))) xs5
                   in let xs7 = map (map (map (map (map (map
                                (partitionBySetBits'' (n8 * 6) n8 n8)))))) xs6
             in map (map (map (map (map (map (map
                                (partitionBySetBits'' (n8 * 7) n8 n8))))))) xs7
    where n8 = n `quot` 8

partitionBySetBits8 n xs    =
    let xs1 = partitionBySetBits8' n xs
    in let xs2 = map (tail . reverse) xs1
       in let xs3 = map (map (tail . reverse)) xs2
          in let xs4 = map (map (map (tail . reverse))) xs3
             in let xs5 = map (map (map (map (tail . reverse)))) xs4
                in let xs6 = map (map (map (map (map (tail . reverse))))) xs5
                   in let xs7 = map (map (map (map (map (map
                                                    (tail . reverse)))))) xs6
                      in tail $ reverse $ map (map (map (map (map (map (map
                                                    (tail . reverse))))))) xs7

nAdjacentNumbers8 :: (Bits a, Num a) => [a] -> Int
nAdjacentNumbers8 xs =
    let adjacentSets = [(x, z) | x <- [0..254] :: [Int],
                       y <- [1, 2, 4, 8, 16, 32, 64, 128],
                       let z = x .|. y, z /= x]
    in foldr (+) 0 (map (xyLength) adjacentSets)
    where partition  = partitionBySetBits8 8 xs
          xyLength z = let x              = fst z
                           y              = snd z
                           nonzeroToOne 0 = 0
                           nonzeroToOne _ = 1
                           x1             = nonzeroToOne $ x .&. 1
                           x2             = nonzeroToOne $ x .&. 2
                           x3             = nonzeroToOne $ x .&. 4
                           x4             = nonzeroToOne $ x .&. 8
                           x5             = nonzeroToOne $ x .&. 16
                           x6             = nonzeroToOne $ x .&. 32
                           x7             = nonzeroToOne $ x .&. 64
                           x8             = nonzeroToOne $ x .&. 128
                           y1             = nonzeroToOne $ y .&. 1
                           y2             = nonzeroToOne $ y .&. 2
                           y3             = nonzeroToOne $ y .&. 4
                           y4             = nonzeroToOne $ y .&. 8
                           y5             = nonzeroToOne $ y .&. 16
                           y6             = nonzeroToOne $ y .&. 32
                           y7             = nonzeroToOne $ y .&. 64
                           y8             = nonzeroToOne $ y .&. 128
                       in (length $ partition!!x1!!x2!!x3!!x4!!x5!!x6!!x7!!x8) *
                          (length $ partition!!y1!!y2!!y3!!y4!!y5!!y6!!y7!!y8)

