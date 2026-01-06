module Report.Modifiers.Rating where

import Prelude

import Data.Array (replicate) as Array
-- import Data.Number (floor) as Number
import Data.Int (floor, toNumber) as Int
import Data.String (joinWith) as String

import Yoga.JSON (class ReadForeign, class WriteForeign)


newtype Rating = Rating Number
derive newtype instance ReadForeign  Rating
derive newtype instance WriteForeign Rating


toStars :: Rating -> String
toStars (Rating n) =
    let fullStars = Int.floor n :: Int
        halfStar = if n - Int.toNumber fullStars >= 0.5 then 1 else 0
        emptyStars = 5 - fullStars - halfStar
    in  (  Array.replicate fullStars "★"
        <> Array.replicate halfStar "⯨"
        <> Array.replicate emptyStars "✩"
        ) # String.joinWith ""


maxValue :: Number
maxValue = 5.0


toNumber :: Rating -> Number
toNumber (Rating n) = n


fromNumber :: Number -> Rating
fromNumber = Rating