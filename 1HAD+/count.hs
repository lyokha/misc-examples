-- Problem:  http://lpaste.net/419764621470072832
-- Solution: http://lpaste.net/133511

import Control.Arrow

count :: (Int -> Int -> [Int]) -> Int -> Int -> IO ()
count f  from to = mapM_ print $ f from to

cheating from to = [from .. to]

simple   from to = takeWhile (<= to) . iterate succ $ from

arrowed  from to = uncurry (flip takeWhile) .
                     curry (iterate succ *** (>=)) from $ to

arrowedPointfree = curry $ iterate succ *** (>=) >>> uncurry (flip takeWhile)

main = do
    putStrLn "Cheating ..."          >> count cheating         1 10
    putStrLn "Simple ..."            >> count simple           1 10
    putStrLn "Arrowed ..."           >> count arrowed          1 10
    putStrLn "Arrowed pointfree ..." >> count arrowedPointfree 1 10

