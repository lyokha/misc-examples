module IntersperseOpsWithBrcEmul ((.+.), (.-.), (.*.), (./.), main) where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import Data.List.Split
import Control.Monad

-- Task:
--   "3333333" must be interspersed by operators (+), (-), (*), (/) and
--   possibly brackets in such a way that the resulting expression would
--   evaluate to 2019 like in 3+(3+3)*(3+333). (This is really one of the
--   answers.)

-- Compile:
--   ghc --make -main-is IntersperseOpsWithBrcEmul intersperseOpsWithBrcEmul.hs

-- Remarks on implementation:
--   additional dotted operators can emulate rank-1 associativity only, i.e.
--   they cannot emulate deeply nested brackets like (3 / (3 / (3 / 3))), which
--   produce different results when using with non-associative operators like
--   (-) and (/)

-- Hint:
--   run
--     sed 's/\(\([0-9]\+\..\.\)\+[0-9]\+\)/(\1)/g; s/\.//g'
--   to transform results to a bracketed form

(.+.) :: Num a => a -> a -> a
(.+.) = (+)
infixl 8 .+.

(.-.) :: Num a => a -> a -> a
(.-.) = (-)
infixl 8 .-.

(.*.) :: Num a => a -> a -> a
(.*.) = (*)
infixl 9 .*.

(./.) :: Fractional a => a -> a -> a
(./.) = (/)
infixl 9 ./.

-- intersperse list elems with all permutations of binary operators ops
allExprs :: Show a => [a] -> [String] -> [String]
allExprs elems ops =
    [concat $ zipWith (flip (++) . show) elems $ [] : opsList
    | opsList <- perms opsLen ops]
    where opsLen      = length elems - 1
          perms 0 _   = [[]]
          perms n ops = [cur : next | cur <- ops, next <- perms (n - 1) ops]

-- interpret stringized map of rational exprs
interpretRationalExprs :: String -> IO [Double]
interpretRationalExprs exprs = do
    eint <- runInterpreter $ do
        set [searchPath := ["."]]
        loadModules["intersperseOpsWithBrcEmul.hs"]
        setImports["Prelude", "IntersperseOpsWithBrcEmul"]
        unsafeInterpret exprs "[Double]"
    return $ either (const []) id eint

processOneChunk :: Bool -> Double -> [String] -> IO ()
processOneChunk verb res es = do
    exprsE <- interpretRationalExprs $ filter (/= '"') $ show es
    let v = map fst $ filter ((\v -> v > res - 0.1 && v < res + 0.1) . snd) $
                zip es exprsE
    if verb
        then do
            putStrLn "\nExprs evaluated  >>>" >> print exprsE
            putStr "\nExprs that evaluate to " >> putStr (show res)
            putStrLn "  >>>" >> print v
        else unless (null v) $ mapM_ putStrLn v

main = do
    putStrLn "Exprs permuted  >>>" >> print exprs
    putStrLn $ "(of length " ++ show (length exprs) ++ ")"
    mapM_ (processOneChunk False res) $ chunksOf 1000 exprs
    where nums  = [3, 3, 3, 3, 3, 3, 3, 3]
          ops   = ["+", "-", "*", "/", ".+.", ".-.", ".*.", "./.", ""]
          res   = 2019
          exprs = allExprs nums ops

