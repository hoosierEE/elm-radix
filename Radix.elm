module Radix (uBase) where

{-| Convert between different textual representations of numbers.

# Conversions
@docs uBase,base

-}

import List as L
import Result as R
import String as S

{-| "Unsafe" base conversion.
Turn any string into a base n integer, suppressing errors with potentially surprising results.
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

-- Common alphabets
hex = "0123456789abcdef"
dec = "0123456789"
oct = "01234567"
bin = "01"

-- some interesting Base-58 alphabets
-- bitcoin = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
-- ripple  = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
-- flickr  = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

