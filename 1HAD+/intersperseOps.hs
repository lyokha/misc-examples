import Language.Haskell.Interpreter

-- intersperse list elems with all permutations of binary operators ops
allExprs :: Show a => [a] -> [String] -> [String]
allExprs elems ops =
    [concat $ zipWith (flip (++) . show) elems $ [] : opsList
    | opsList <- perms opsLen ops]
    where opsLen      = length elems - 1
          perms 0 _   = [[]]
          perms n ops = [cur : next | cur <- ops, next <- perms (n - 1) ops]

-- interpret stringized map of rational exprs
interpretRationalExprs :: String -> IO [Rational]
interpretRationalExprs exprs = do
    let interp = interpret exprs (as :: [Rational]) :: Interpreter [Rational]
    eint <- runInterpreter $ setImports ["Prelude", "Data.Ratio"] >> interp
    return $ either (const []) id eint

main = do
    putStrLn "Exprs permuted  >>>" >> print exprs
    exprsE <- interpretRationalExprs exprsS
    putStrLn "\nExprs evaluated  >>>" >> print (map fromRational exprsE)
    putStr "\nExprs that evaluate to " >> putStr (show $ fromRational res)
    putStrLn "  >>>"
    print $ map fst $ filter ((== res) . snd) $ zip exprs exprsE
    where nums   = [1, 2, 3, 4, 5, 8]
          ops    = ["+", "-", "*", "/"]
          res    = 4
          exprs  = allExprs nums ops
          exprsS = filter (/= '"') $ show exprs

