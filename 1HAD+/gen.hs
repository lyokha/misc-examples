-- Problem: find most common k-mer with given number of possible mutations;
--          see also my blog posts:
--          http://lin-techdet.blogspot.ru/2014/11/haskell.html
--          http://lin-techdet.blogspot.ru/2014/11/haskell_10.html
--
-- Compile (for parallel computation and collecting run data for threadscope):
--      ghc --make -O2 -rtsopts -threaded -eventlog -feager-blackholing \
--      -fforce-recomp gen
--
-- Run (for best results):
--      ./gen +RTS -N4 -A128M -qa -lf

import qualified Data.List as L
import qualified Data.List.Ordered as LO
import Data.Ord (comparing)
import Control.Arrow
import qualified Data.HashTable.IO as H
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Control.Parallel.Strategies
import Data.List.Split (chunksOf)
import Data.Function (on)

alphabet :: [Char]
alphabet = "ACGT"

subSeqs :: Int -> String -> [String]
subSeqs n = takeWhile ((==n) . length) . map (take n) . L.tails

subSeqsPar :: Int -> Int -> String -> [String]
subSeqsPar m n s = subSeqs n s `using` parListChunk (length s `div` m) rseq

nearSeqs :: Int -> String -> [String]
nearSeqs n = LO.nubSort . nearSeqs' n
    where nearSeqs' 0 s = [s]
          nearSeqs' n s =
            concatMap (\(a, b) -> map (a++) b) $
                concatMap (\m -> [(z ++ [x], nearSeqs' (n - 1) $ tail z') |
                                  x <- alphabet, let (z, z') = splitAt m s])
                [0 .. length s - 1]

nearSeqs'' :: Int -> String -> [String]
nearSeqs'' n s = s : concatMap (`nearSeqs'` s) [1 .. n]
    where nearSeqs' 0 s = [s]
          nearSeqs' n s =
            concatMap (\(a, b) -> map (a++) b) $
                concatMap (\m -> [(z ++ [x], nearSeqs' (n - 1) $ tail z') |
                                  let (z, z') = splitAt m s,
                                  x <- alphabet' $ head z'])
                [0 .. length s - 1]
                where alphabet' 'A' = "CGT"
                      alphabet' 'C' = "AGT"
                      alphabet' 'G' = "ACT"
                      alphabet' 'T' = "ACG"

grpSeqs :: Int -> [String] -> [[String]]
grpSeqs n = L.sortBy (flip $ comparing length) . L.group . L.sort . seqs
    where seqs = concatMap $ nearSeqs n

grpSeqs' :: Int -> [String] -> [[String]]
grpSeqs' n = L.sortBy (flip $ comparing length) . L.group . L.sort . seqs
    where seqs = concatMap (\(s, m) -> concat . replicate m . nearSeqs n $ s) .
                    map (head &&& length) . L.group . L.sort

grpSeqsPar :: Int -> Int -> [String] -> [[String]]
grpSeqsPar m n = L.sortBy (flip $ comparing length) . L.group . sortPar . seqs
    where sortPar = foldr LO.merge [] . parMap rdeepseq L.sort
          seqs = concat' m .
                    map (\(s, m) -> concat . replicate m . nearSeqs n $ s) .
                        map (head &&& length) . L.group . L.sort
          concat' m s = let l = let (/.) = (/) `on` fromIntegral
                                in ceiling $ length s /. m
                        in map concat $ chunksOf l s

grpSeqsPar' :: Int -> Int -> [String] -> [[String]]
grpSeqsPar' m n = L.sortBy (flip $ comparing length) . L.group . L.sort . seqs
    where seqs = concat . parMap rdeepseq
                    (\(s, m) -> concat . replicate m . nearSeqs n $ s) .
                        map (head &&& length) . L.group . L.sort

type HashTable k v = H.CuckooHashTable k v

grpSeqsHt :: Int -> Int -> [String] -> IO (HashTable String Int)
grpSeqsHt m n s = do
    h <- H.newSized m
    let insert' h k = H.lookup h k >>= H.insert h k . (+1) . fromMaybe 0
    mapM_ (mapM_ (insert' h) . nearSeqs n) s
    return h

mostOftenSeqsHt :: HashTable String Int -> IO [(String, Int)]
mostOftenSeqsHt = H.foldM findMax []
    where findMax [] a = return [a]
          findMax c@((_, v') : _) a@(_, v) =
            return $ case v `compare` v' of GT -> [a]; EQ -> a : c; LT -> c

grpSeqsHt' :: Int -> Int -> [String] ->
    IO (HashTable String Int, [(String, Int)])
grpSeqsHt' m n s = do
    h <- H.newSized m
    let insert' h k =
            H.lookup h k >>=
                (\v -> H.insert h k v >> return (k, v)) . (+1) . fromMaybe 0
        findMax [] a = return [a]
        findMax c@((_, v') : _) a@(_, v) =
            return $ case v `compare` v' of GT -> [a]; EQ -> a : c; LT -> c
    c <- foldM (\c s -> foldM
                    (\c k -> insert' h k >>= findMax c) c $ nearSeqs n s) [] s
    return (h, c)

main = do
    let seqs = ["ACGTTGCATGTCGCATGATGCATGAGAGCT",
                "CACAGTAGGCGCCGGCACACACAGCCCCGGGCCCCGGGCCGCCCCGGGCCGGC\
                \GGCCGCCGGCGCCGGCACACCGGCACAGCCGTACCGGCACAGTAGTACCGGCC\
                \GGCCGGCACACCGGCACACCGGGTACACACCGGGGCGCACACACAGGCGGGCG\
                \CCGGGCCCCGGGCCGTACCGGGCCGCCGGCGGCCCACAGGCGCCGGCACAGTA\
                \CCGGCACACACAGTAGCCCACACACAGGCGGGCGGTAGCCGGCGCACACACAC\
                \ACAGTAGGCGCACAGCCGCCCACACACACCGGCCGGCCGGCACAGGCGGGCGG\
                \GCGCACACACACCGGCACAGTAGTAGGCGGCCGGCGCACAGCC"]
    {-print $ nearSeqs 2 "TTTT"-}
    {-mapM_ print' $ take 10 $ grpSeqs 1 $ subSeqs 4 $ head seqs-}
    {-grpSeqsHt 70000 2 (subSeqs 10 $ seqs !! 1) >>= mostOftenSeqsHt >>= print-}
    {-grpSeqsHt' 70000 2 (subSeqs 10 $ seqs !! 1) >>= print . snd-}
    {-print $ length $ L.nub $ subSeqs 10 $ seqs !! 1-}
    {-mapM_ print' $ take 10 $ grpSeqs 2 $ subSeqs 10 $ seqs !! 1-}
    {-mapM_ print' $ take 10 $ grpSeqs' 2 $ subSeqs 10 $ seqs !! 1-}
    {-mapM_ print' $ take 10 $ grpSeqs' 2 $ subSeqsPar 4 10 $ seqs !! 1-}
    {-print $ subSeqs 4 $ head seqs-}
    {-print $ subSeqsPar 4 4 $ head seqs-}
    mapM_ print' $ take 10 $ grpSeqsPar 4 2 $ subSeqs 10 $ seqs !! 1
    where print' = print . (head &&& length)

