{-# LANGUAGE TupleSections, MultiWayIf #-}

import Data.Array
import System.Environment (getArgs)
import Data.Bool (bool)
import Prelude hiding (Right)

data Backtrack  = Down | Right | DownRight deriving (Show, Eq)
type Backtracks = Array (Int, Int) (Int, Maybe Backtrack)

lcsBacktrack :: String -> String -> Backtracks
lcsBacktrack v w =
    let s = listArray ((0, 0), (n, m)) $ concat $
                replicate (m + 1) edgeValue :
                    map (\(i, v) -> edgeValue : map (row s i v) (indexify w))
                        (indexify v)
    in s
    where n                = length v
          m                = length w
          edgeValue        = (0, Nothing)
          row s i v (j, w) =
              let max3    = (max .) . max
                  weights = curry $ fst . (s !)
                  weight  = max3 (weights (i - 1) j)
                                 (weights i (j - 1))
                                 (weights (i - 1) (j - 1) + bool 0 1 (v == w))
              in (,) weight $ if | weight == weights (i - 1) j -> Just Down
                                 | weight == weights i (j - 1) -> Just Right
                                 | otherwise                   -> Just DownRight
          indexify = zip [1 ..]

lcs :: String -> String -> String
lcs v w =
    let bt = lcsBacktrack v w
        lcs' bt v 0 _ = ""
        lcs' bt v _ 0 = ""
        lcs' bt v i j | snd (bt ! (i, j)) == Just Down  = lcs' bt v (i - 1) j
                      | snd (bt ! (i, j)) == Just Right = lcs' bt v i (j - 1)
                      | otherwise = lcs' bt v (i - 1) (j - 1) ++ [v !! (i - 1)]
    in lcs' bt v (length v) (length w)

main = do
    content <- getArgs >>= readFile . head
    let [v, w] = take 2 $ lines content
    putStrLn "Sequence 1   >>>"   >> putStrLn v
    putStrLn "\nSequence 2   >>>" >> putStrLn w
    putStrLn "\nLCS (s1, s2) >>>" >> putStrLn (lcs v w)

