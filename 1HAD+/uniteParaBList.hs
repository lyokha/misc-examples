-- An example of a simple pandoc filter taken from comments on article at
-- http://lin-techdet.blogspot.ru/2014/08/pandoc.html
-- Published at http://lpaste.net/121331

import Text.Pandoc.JSON
import Data.List (mapAccumR)

-- Pandoc filter to move a paragraph before a bullet list inside the bullet
-- list as a plain first item. Implemented for the document root only but may
-- be extended by walk'ing inside bs in the (Pandoc m bs) pattern.
--
-- ### Compile:
--   ghc --make uniteParaBList.hs
--
-- ### Example to test:
-- This is the first para.
--
-- This is the second para.
--
-- * First item
-- * Second item
--
-- This is the last para.
--
-- ### Command to test:
--   pandoc -thtml tmp.md -F ./uniteParaBList

uniteParaBList :: Maybe Format -> Pandoc -> IO Pandoc
uniteParaBList _ (Pandoc m bs) =
    return $ Pandoc m $ filter (/= Null) $ putFirst $
        mapAccumR fwdPara Nothing bs
    where fwdPara Nothing  b@(BulletList _)       = (Just b, Null)
          fwdPara (Just (BulletList bs)) (Para p) =
                            (Nothing, BulletList $ [Plain p] : bs)
          fwdPara (Just a) b                      = (Just b, a)
          fwdPara Nothing  b                      = (Nothing, b)
          putFirst (Nothing, bs) = bs
          putFirst (Just a,  bs) = a : bs

main :: IO ()
main = toJSONFilter uniteParaBList

