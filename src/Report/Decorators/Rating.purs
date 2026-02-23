module Report.Decorators.Rating where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (replicate) as Array
import Data.Number (fromString) as Number
import Data.Int (floor, fromString, toNumber) as Int
import Data.String (joinWith, split, Pattern(..)) as String

import Yoga.JSON (class ReadForeign, class WriteForeign)


newtype Rating
    = Rating { max :: Int, value :: Number } -- min is always 0


derive instance Eq Rating
derive instance Ord Rating
derive newtype instance ReadForeign  Rating
derive newtype instance WriteForeign Rating


-- get value between 0.0 and 1.0, where 1.0 is max
relValue :: Rating -> Number
relValue (Rating r) = (r.value / Int.toNumber r.max)


toStars :: Rating -> String
toStars (Rating r) =
    if r.max <= 10 then
        let fullStars = Int.floor r.value :: Int
            halfStar = if r.value - Int.toNumber fullStars >= 0.5 then 1 else 0
            emptyStars = r.max - fullStars - halfStar
        in  (  Array.replicate fullStars "★"
            <> Array.replicate halfStar "✫"
            <> Array.replicate emptyStars "✩"
            ) # String.joinWith ""
    else
        let maxStars = 10
            lerpedValue = (r.value / Int.toNumber r.max) * Int.toNumber maxStars
            fullStars = Int.floor lerpedValue :: Int
            halfStar = if lerpedValue - Int.toNumber fullStars >= 0.5 then 1 else 0
            emptyStars = maxStars - fullStars - halfStar
        in  (  Array.replicate fullStars "★"
            <> Array.replicate halfStar "✫"
            <> Array.replicate emptyStars "✩"
            ) # String.joinWith ""


maxValue :: Rating -> Int
maxValue (Rating r) = r.max


toNumber :: Rating -> Number
toNumber (Rating r) = r.value


from :: { max :: Int, value :: Number } -> Rating
from = Rating


_from :: Int -> Number -> Rating
_from max value = Rating { max, value }


toString :: Rating -> String
toString (Rating r) = show r.value <> "/" <> show r.max


fromString :: String -> Maybe Rating
fromString str =
    case String.split (String.Pattern "/") str of
        [ valueStr, maxStr ] ->
            (\max v -> Rating { max, value: v })
                <$> Int.fromString maxStr
                <*> Number.fromString valueStr
        _ -> Nothing