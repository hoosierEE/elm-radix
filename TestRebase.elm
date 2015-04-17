module TestRebase where

import Rebase exposing (..)
import Graphics.Element as E
import List as L
import String as S

vals = L.map toString [0..20]
chrs = L.map toString <| S.toList "0123456789abcdefghij"
dec = E.show <| L.map intFromDec vals
hex = E.show <| L.map intFromHex vals
bin = E.show <| L.map intFromBin vals
oct = E.show <| L.map intFromOct vals
dec' = E.show <| L.map intFromDec chrs
hex' = E.show <| L.map intFromHex chrs
bin' = E.show <| L.map intFromBin chrs
oct' = E.show <| L.map intFromOct chrs

bin'' = E.show <| intFromBin "2103" -- becomes "_10_" or just "10", which in binary is 2
hex'' = E.show <| intFromHex "fffffffff0101"
bc = E.show <| intFromBitcoin "z"

main = E.flow E.down
    [ bin
    , bin'
    , oct
    , oct'
    , hex
    , hex'
    , dec
    , dec'
    , bin''
    , hex''
    , bc
    ]
