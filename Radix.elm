module Radix (base,uBase) where

{-| Convert among textual representations of numbers.
Sometimes it's useful to represent a number using a different base or radix.
Some common bases include 16 (hexadecimal), 2 (binary) and 10 (decimal), but others exist.

# Conversions
@docs uBase, base

# Convenience Functions
@docs hex, dec, oct, bin

# Interesting Alphabets
@docs bitcoin, flickr, ripple

-}

import List as L
import Result as R
import String as S

-- PRIMARY FUNCTIONS --

{-| "Unsafe" Conversion
Convert a string to a base n integer, suppressing errors. Potentially disastrous results; use with caution!
    uBase 16 "100" -- 256
    uBase 16 "1ff" -- 1
-}
uBase : Int -> String -> Int
uBase n str =
    let
        chars = L.filterMap (R.toMaybe << S.toInt << S.fromChar) <| S.toList str
        len = L.length chars
        weights = L.indexedMap (\x v -> (n ^ (len - 1 - x)) * v) chars
    in
       L.foldr (+) 0 weights

{-| Generic (safe) Conversion
Given an alphabet `a` and a string `s`, convert `s` to a number determined by `a`.
    base "0123456789abcdef" "ff" -- Ok 255
    base "012" "111" -- Ok 13
-}
base : String -> String -> Int
base alph str = -- TODO
    1


-- CONVENIENCE FUNCTIONS --

-- Commonly used alphabets.
{-| Convert a hexadecimal-formatted string into an Elm Int -}
hex str = base "0123456789abcdef" str
{-| Convert a decimal-formatted string into an Elm Int.
Since Elm Ints are decimal-formatted already, this is mainly useful as a sanity check
-}
dec str = base "0123456789" str
{-| Convert an octal-formatted string into an Elm Int -}
oct str = base "01234567" str
{-| Convert a binary-formatted string into an Elm Int -}
bin str = base "01" str

-- Interesting Base-58 alphabets
{-| Convert a bitcoin-formatted string into an Elm Int -}
bitcoin str = base "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" str
{-| Convert a ripple-formatted string into an Elm Int -}
ripple  str = base "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz" str
{-| Convert a flickr-formatted string into an Elm Int -}
flickr  str = base "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" str

