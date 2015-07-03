-- Problem:  http://lpaste.net/3051751310949875712
-- Solution: http://lpaste.net/135722

import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.Char (isPunctuation)
import Data.Text.ICU (toLower, brkBreak, breaks, breakWord, LocaleName(..))
import Data.Maybe

linearizeWord :: String -> String
linearizeWord =
    -- taking head (using its total analog fromMaybe [] . listToMaybe) with
    -- assumption that there are no punctuation signs inside a word!
    fromMaybe [] . listToMaybe . map (unpack . toLower Current) .
    dropWhile (isPunctuation . T.head) .
    map brkBreak . breaks (breakWord Current) . pack

main :: IO ()
main =
    mapM_ (putStrLn . linearizeWord)
        ["pERforMance:.", ".:?", ",,,хВост,", "Ärzte"]
    -- will output
    --
    -- performance
    --
    -- хвост
    -- ärzte

