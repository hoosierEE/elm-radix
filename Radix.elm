module Radix (Base, intFromBase) where

{-| Convert between different textual representations of numbers.

# Conversions
@docs intFromBase,stringFromBase

# Common specializations
@docs intFromBin,intFromHex,intFromOct,intFromDec

-}

import String as S
import List as L


type alias Base = { base: Int, alphabet: String }


intFromBase : Base -> String -> Int
intFromBase base value =
    let
        bb = base.base
        bs = base.alphabet
        cs = L.reverse <| S.toList value
        match = L.head << flip S.indexes bs << S.fromChar
        xs = L.filterMap match cs
        pwrd = L.indexedMap (\x v -> (bb ^ x) * (v % bb)) xs
    in
       L.foldr (+) 0 pwrd


stringFromBase : Base -> Int -> String
stringFromBase base value =
    let
        bb = base.base
        bs = base.alphabet
    in
       "fixme"


-- example bases
bin = { base =  2, alphabet = "01" }
hex = { base = 16, alphabet = "0123456789ABCDEF0123456789abcdef" }
oct = { base =  8, alphabet = "01234567" }
dec = { base = 10, alphabet = "0123456789" }

bitcoin = { base = 58, alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" }
ripple  = { base = 58, alphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz" }
flickr  = { base = 58, alphabet = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" }

-- specialize intFromBase to use these particular examples:
intFromBin str = intFromBase bin str
intFromHex str = intFromBase hex str
intFromOct str = intFromBase oct str
intFromDec str = intFromBase dec str

intFromBitcoin str = intFromBase bitcoin str
intFromFlickr str  = intFromBase flickr str
intFromRipple str  = intFromBase ripple str

