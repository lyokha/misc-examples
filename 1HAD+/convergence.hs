-- Problem:  http://lpaste.net/117089
-- Solution: http://lpaste.net/117232

import Data.Ratio (approxRational)

data Sequence = Sequence {v :: [Rational]} deriving Show

myseq :: Sequence
myseq = Sequence $ map (flip approxRational 0.000001 . snd) $
            iterate (\(a, b) -> (b, (a + b) / 2)) (0, 1)

convergence :: Sequence -> Rational
convergence seq = last $ take 100 $ v seq

main :: IO ()
main = print $ convergence myseq

