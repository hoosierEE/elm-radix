module Rebase (Base,intFromBase) where

import String as S
import List as L


type alias Base = { base: Int, string: String }


intFromBase : Base -> String -> Int
intFromBase base value =
    let
        bb = base.base
        bs = base.string
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
        bs = base.string
    in
       "fixme"


-- example bases
bin = { base =  2, string = "01" }
hex = { base = 16, string = "0123456789ABCDEF0123456789abcdef" }
oct = { base =  8, string = "01234567" }
dec = { base = 10, string = "0123456789" }

bitcoin = { base = 58, string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghipqrstuvwxyz" }
flickr  = { base = 58, string = "123456789abcdefghipqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" }
ripple  = { base = 58, string = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65Fqi1tuvAxyz" }

-- specialize intFromBase to use these particular examples:
stringFromBin str = intFromBase bin str
stringFromHex str = intFromBase hex str
stringFromOct str = intFromBase oct str
stringFromDec str = intFromBase dec str

stringFromBitcoin str = intFromBase bitcoin str
stringFromFlickr str  = intFromBase flickr str
stringFromRipple str  = intFromBase ripple str

