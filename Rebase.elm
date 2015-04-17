module Rebase (Base,intFromBase) where
import String as S
import List as L

type alias Base = { base: Int, string: String }

intFromBase : Base -> String -> Int
intFromBase base val =
    let
        bb = base.base
        bs = base.string
        cs = L.reverse <| S.toList val
        match = L.head << flip S.indexes bs << S.fromChar
        xs = L.filterMap match cs
        pwrd = L.indexedMap (\x v -> (bb ^ x) * (v % bb)) xs
    in
       L.foldr (+) 0 pwrd

-- stringFromBase : Base -> Int -> String

-- example bases
hex     = { base = 16, string = "0123456789ABCDEF0123456789abcdef" }
bitcoin = { base = 58, string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghipqrstuvwxyz" }
ripple  = { base = 58, string = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65Fqi1tuvAxyz" }
flickr  = { base = 58, string = "123456789abcdefghipqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" }

-- specialize intFromBase to use these particular examples:
stringFromBitcoin str = intFromBase bitcoin str
stringFromFlickr str  = intFromBase flickr str
stringFromHex str     = intFromBase hex str
stringFromRipple str  = intFromBase ripple str

