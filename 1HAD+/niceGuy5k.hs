-- Problem:  http://lpaste.net/6680706931826360320
-- Solution: http://lpaste.net/130803

{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.Char

niceGuy5k :: FilePath -> IO ()
niceGuy5k geophfWritesTooManyWordsInOneJournalEntry = do
    content <- readFile geophfWritesTooManyWordsInOneJournalEntry
    let part1 = dropWhile (\(_, fwords, _) -> fwords < 5001) $
                scanl (\(fpos, fwords, wasNotSpace)
                        (not . isSpace -> isNotSpace) ->
                        (fpos + 1,
                         fwords + fromEnum (isNotSpace && not wasNotSpace),
                         isNotSpace
                        )
                      ) (0, 0, False) content
    case part1 of
        []               -> putStrLn $
                                geophfWritesTooManyWordsInOneJournalEntry ++
                                    " does not exceed 5000 words"
        (fpos, _, _) : _ -> do
                              let (a, b)          = splitAt (fpos - 1) content
                                  fst5000FileName =
                                      geophfWritesTooManyWordsInOneJournalEntry
                                      ++ ".5000"
                                  restFileName    = "morethen.txt"
                              writeFile fst5000FileName a
                              writeFile restFileName    b

main :: IO ()
main = niceGuy5k "some_long_file"

