import Data.Array
import System.Environment (getArgs)

type Weights = Array (Int, Int) Int

manht :: Int -> Int -> Weights -> Weights -> Weights
manht n m down right =
    let ed = edge n $ \(i, a) -> (succ i, a + down ! (i, 0))
        er = edge m $ \(i, a) -> (succ i, a + right ! (0, i))
        s  = listArray ((0, 0), (n, m)) $ concat $ (0 : er) : map
                (\(i, a) -> a : map (\j -> max (s ! (i - 1, j) + down ! (i, j))
                                               (s ! (i, j - 1) + right ! (i, j))
                                    ) [1 .. m]
                ) (zip [1 ..] ed)
    in s
    where edge len prod = map snd $ take len $ tail $ iterate prod (1, 0)

main = do
    content <- getArgs >>= readFile . head
    let strings = lines content
        [n, m]  = values 2 $ head strings :: [Int]
        down    = listArray ((1, 0), (n, m)) $ concatMap (values $ m + 1) $
                    block 1 n strings :: Weights
        right   = listArray ((0, 1), (n, m)) $ concatMap (values m) $
                    block (n + 2) (n + 1) strings :: Weights
        res     = manht n m down right
    putStrLn "Down   >>> "   >> print down
    putStrLn "\nRight  >>> " >> print right
    putStrLn "\nTrace  >>> " >> print res
    putStrLn "\nAnswer >>> " >> print (res ! (n, m))
    where values len      = map read . take len . words
          block start len = take len . drop start

