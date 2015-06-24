--
--       Filename:  cf.hs
--
--    Description:  grammar test module
--
--        Version:  1.0
--        Created:  14.11.2010
--
--         Author:  Alexey Radkov (), 
--        Company:  
--

module Main where
import CfLexer ( alexScanTokens )
import CfGrammar ( cf_parse, printResult )
import qualified Control.Exception as E


processLine :: String -> IO ()
processLine s = do
    putStrLn $ "Parsing of '" ++ s ++ "'"
    ( printResult . cf_parse . alexScanTokens ) s `E.catch`
            \( e :: E.SomeException ) -> putStrLn $ show e
    putStrLn ""

main = mapM_ processLine [
    "keep",
    "delete tpt if (5.1 * 6) / var1[ 1 ] * var2",
    "delete tpt if 5.1 * 6 + var1 * var2[ 1, 10 ]",
    "delete tpt if some_var*6*2.9",
    "delete tpt if 5.1 + 6 * Sum(var1) + var2",
    "delete tpt if 5.1 + 6 * Sum(var1 + var2)",
    "delete tpt if 5.1 + 6 * (var1 + var2)",
    "delete tpt if 5.1 + 6",
    "delete tpt if 5.1 + -6 + var1",
    "delete tpt if (1 + 2) * (3 / (4 + -5) * 5 + 6)",
    "delete tpt if (3 / (4 + -5) * 5 + 6) * (1 + 2)",
    "delete tpt if 1 + 2 * (3 / (4 + -5) * 5 + 6)",
    "keep if a > b & a | -(7 + 8) >= -a & a[1,20]",
    "keep if -Sum(2) != 8 + 9 * (5 + 6) + ! 1 * 7",
    "keep if -Sum(2) != 8 + 9 * ((5 + 6) > ! (1 * 7))",
    "delete edt",
    "delete tpt",
    "delete tpt if 20 * 2.3 > 5",
    "delete tpt if 46. > 5",
    "delete tpt if 2 < 10",
    "delete tpt if 1 & 1",
    "delete tpt if 20 * 2.3 < 5 & 2 < aa",
    "delete tpt if 7.0",
    "delete tpt if Sqrt(Sqr(7-1))",
    "delete tpt if 8 * 4 / 2 * 4 * 6 + 8 - 4 / 3",
    "delete tpt if 8 * f(4 / 2)",
    "delete tpt if 8 * f(4) / 2",
    "delete tpt if 2 * 8 * (4 - 8 / 4) / (2 - 4) * 6 / 3",
    "delete tpt if c + 1 & 2 * 8 * (4 - 8 / 4) / (2 - 4) * 6 / 3 & d",
    "delete tpt if 6 / 8 * ((4 / 3) / 2) * 4 / f(3 * 10 - 4 / 5 ) / 5 - 1",
    "delete tpt if (8/2) * (4/2) * 6 * (2/2) " ]

