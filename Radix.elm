module Radix (base,uBase) where

{-| Convert among textual representations of numbers.
Sometimes it's useful to represent a number using a different base or radix.  In computer sciencce,
This is often used for converting numbers among decimal, binary, and hexidecimal formats.

# Conversions
@docs uBase,base

# Convenience functions
@docs hex,dec,oct,bin

-}

import List as L
import Result as R
import String as S

{-| "Unsafe" Base Conversion
Convert a string to a base n integer, suppressing errors. Potentially surprising results!
    uBase 16 "100" -- 256
    uBase 16 "1ff" -- 256
-}
uBase : Int -> String -> Int
uBase n str =
    let
        len = S.length str
        chars = L.filterMap (R.toMaybe << S.toInt << toString) <| S.toList str
        weights = L.indexedMap (\x v -> (n ^ (len - x)) * v) chars
    in
       L.foldr (+) 0 weights

{-| Generic Conversion
Convert the given string `str` to an integer based on its characters' positions in the alphabet `alph`
    base "012" "111" -- Ok 13
-}
base : String -> String -> Int
base alph str = -- TODO
    1

-- Common alphabets
aHex = "0123456789abcdef"
aDec = "0123456789"
aOct = "01234567"
aBin = "01"

-- some interesting Base-58 alphabets
-- bitcoin = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
-- ripple  = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
-- flickr  = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

