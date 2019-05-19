module Lib
(
    Color(..)
    , firstColor
    , reverseColorOrder
    , paintMix
) where

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)


firstColor :: Color
firstColor = minBound :: Color

reverseColorOrder :: [Color]
reverseColorOrder = reverse [minBound :: Color .. maxBound :: Color]

paintMix :: Color -> Color -> Color
paintMix c1 c2 = colorRange !! quot (length colorRange) 2
  where colorRange = [c1 .. c2]

