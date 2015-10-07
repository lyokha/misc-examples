module Numeric.MathFunctions.SuperscriptAliases
    ( sin², sin³
    , cos², cos³
    , tan², tan³
    , pi²
    ) where

f *^ x =  (^ x) . f

sin² = sin      *^ 2
sin³ = sin      *^ 3
cos² = cos      *^ 2
cos³ = cos      *^ 3
tan² = tan      *^ 2
tan³ = tan      *^ 3
pi²  = const pi *^ 2 $ Nothing  -- can also be defined as simple as pi ^ 2

