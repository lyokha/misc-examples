--
--       Filename:  shortest_path.hs
--
--    Description:  calculate short paths between random 2D points
--
--        Version:  1.0
--        Created:  10.10.2013
--
--         Author:  Alexey Radkov (), 
--        Company:  
--

module ShortestPath where
import Data.List
import Text.Printf


{-Library functions-}

distance :: Floating a =>
    ((a, a), (a, a)) -> a
distance ((x0, y0), (x1, y1)) = sqrt $ (x0 - x1) ^ 2 + (y0 - y1) ^ 2

findCloser :: (Num a, Ord a) =>
    ((a, a), (a, a)) -> ((a, a), (a, a)) -> Ordering
findCloser ((x0, y0), (x1, y1)) ((x2, y2), (x3, y3)) = compare d1 d2
    where d1 = (x0 - x1) ^ 2 + (y0 - y1) ^ 2
          d2 = (x2 - x3) ^ 2 + (y2 - y3) ^ 2

infiniteDistance :: Floating a => a
infiniteDistance = 1e15

findClosest :: (Floating a, Ord a) =>
    [(a, a)] -> [[(a, a)]] -> ([(a, a)], Maybe Int)
findClosest x [] = ([], Just 0)
findClosest x xs = foldr (\x' y' -> findCloser' x' y' x) ([], Just 0) xs
    where findCloser' x y z =
              let zx = z `distance'` x
                  zy = z `distance'` fst y
              in if fst zx < fst zy
                  then (x, snd zx)
                  else (fst y, snd zy)
                  where distance' []     _      = (infiniteDistance, Just 0)
                        distance' _      []     = (infiniteDistance, Just 0)
                        distance' [x]    [y]    = (distance (x, y),  Just 0)
                        distance' (x:xs) [y]    = (d, i)
                            where x' = last xs
                                  zs = [(x', y), (x, y)]
                                  z  = minimumBy findCloser zs
                                  d  = distance z
                                  i  = elemIndex z zs
                        distance' [x]    (y:ys) = (d, i)
                            where y' = last ys
                                  zs = [(x, y), (x, y')]
                                  z  = minimumBy findCloser zs
                                  d  = distance z
                                  i  = elemIndex z zs
                        distance' (x:xs) (y:ys) = (d, i)
                            where x' = last xs
                                  y' = last ys
                                  zs = [(x', y), (x, y'), (x', y'), (x, y)]
                                  z  = minimumBy findCloser zs
                                  d  = distance z
                                  i  = elemIndex z zs

moveClosest :: (Floating a, Ord a) =>
    [(a, a)] -> [[(a, a)]] -> ([(a, a)], [[(a, a)]])
moveClosest x [] = (x, [])
moveClosest x xs
    | i == Just 0 = (x ++ y, delete y xs)
    | i == Just 1 = (y ++ x, delete y xs)
    | i == Just 2 = (x ++ reverse y, delete y xs)
    | otherwise   = (reverse y ++ x, delete y xs)
    where (y, i) = findClosest x xs

shortestPath :: (Floating a, Ord a) =>
     [[(a, a)]] -> [(a, a)]
shortestPath []     = []
shortestPath [x]    = x
shortestPath (x:xs) = fst $ shortestPath' $ moveClosest x xs
    where shortestPath' (x, []) = (x, [])
          shortestPath' (x, y)  = shortestPath' $ moveClosest x y

combineClosest :: (Floating a, Ord a) =>
    [[(a, a)]] -> [[(a, a)]]
combineClosest (x:xs) = moveClosest' x xs []
    where moveClosest' x [] t       = x : t
          moveClosest' x xs@[_] t   = fst (moveClosest x xs) : t
          moveClosest' x xs@(_:_) t = moveClosest' x' y' t'
              where z  = moveClosest x xs
                    x' = head $ snd z
                    y' = tail $ snd z
                    t' = fst z : t

shortestPathP :: (Floating a, Ord a) =>
     [[(a, a)]] -> [(a, a)]
shortestPathP []  = []
shortestPathP [x] = x
shortestPathP xs  = shortestPath' $ combineClosest xs
    where shortestPath' []  = []
          shortestPath' [x] = x
          shortestPath' xs  = shortestPath' $ combineClosest xs'
              where xs' = combineClosest xs

pathDistance :: (Floating a, Ord a) =>
    [(a, a)] -> a
pathDistance []        = 0
pathDistance [_]       = 0
pathDistance zs@(_:xs) = foldr (\x' y' -> distance x' + y') 0 (zip zs xs)


{-IO functions-}

type Transform = ([[(Float, Float)]] -> [[(Float, Float)]])
type Algorithm = ([[(Float, Float)]] -> [(Float, Float)])

calcFromFile' :: String -> Transform -> Algorithm -> IO ()
calcFromFile' file tr alg = do
    content <- readFile file
    let fileData         = map readFloat (lines content)
        path             = alg . tr $ fileData
        distance         = pathDistance path
        printPair (x, y) = printf "(%10.4f, %10.4f)\n" x y
    putStrLn "*** Original set ***"
    mapM_ (mapM_ printPair) fileData
    putStrLn "\n*** Short path ***"
    mapM_ printPair path
    putStrLn "\n*** Distance ***"
    print distance
    putStrLn ""

readFloat :: String -> [(Float, Float)]
readFloat x = [(read y :: Float, read z :: Float)]
    where y:z:_ = words x

calcFromFile :: String -> IO ()
calcFromFile file = calcFromFile' file id shortestPath

calcFromFileR :: String -> IO ()
calcFromFileR file = calcFromFile' file reverse shortestPath

calcFromFileP :: String -> IO ()
calcFromFileP file = calcFromFile' file id shortestPathP

calcFromFilePR :: String -> IO ()
calcFromFilePR file = calcFromFile' file reverse shortestPathP


{-Explicit data sample-}

sampleDataFile :: (Floating a, Ord a) => [[(a, a)]]
sampleDataFile =
    [[(20833.3333, 17100.0000)],
     [(20900.0000, 17066.6667)],
     [(21300.0000, 13016.6667)],
     [(21600.0000, 14150.0000)],
     [(21600.0000, 14966.6667)],
     [(21600.0000, 16500.0000)],
     [(22183.3333, 13133.3333)],
     [(22583.3333, 14300.0000)],
     [(22683.3333, 12716.6667)],
     [(23616.6667, 15866.6667)],
     [(23700.0000, 15933.3333)],
     [(23883.3333, 14533.3333)],
     [(24166.6667, 13250.0000)],
     [(25149.1667, 12365.8333)],
     [(26133.3333, 14500.0000)],
     [(26150.0000, 10550.0000)],
     [(26283.3333, 12766.6667)],
     [(26433.3333, 13433.3333)],
     [(26550.0000, 13850.0000)],
     [(26733.3333, 11683.3333)],
     [(27026.1111, 13051.9444)],
     [(27096.1111, 13415.8333)],
     [(27153.6111, 13203.3333)],
     [(27166.6667,  9833.3333)],
     [(27233.3333, 10450.0000)]]

