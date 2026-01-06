module Report.Convert.Text.Modifiers.Stats where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Int (fromString) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)
import Data.String (split, Pattern(..)) as String

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Modifiers.Stats (Stats(..))
import Report.Modifiers.Progress (PValueTag)
import Report.Convert.Text.Modifiers.Progress (decodeProgress, encodeProgress)


encodeStats :: Stats -> Maybe PValueTag /\ CT.EncodedValue
encodeStats stats = case stats of
    SGotTotal { got, total } -> Nothing /\ (CT.EncodedValue $ show got <> "/" <> show total)
    SWithProgress { got, total, onTheWay } -> Nothing /\ (CT.EncodedValue $ show got <> "/" <> show onTheWay <> "/" <> show total)
    SFromProgress progress -> lmap pure $ encodeProgress progress
    SNotRelevant -> Nothing /\ CT.EncodedValue "SNR"
    SYetUnknown -> Nothing /\ CT.EncodedValue "SUNK"


decodeStats :: Maybe PValueTag -> CT.EncodedValue -> Maybe Stats
decodeStats mbPTag (CT.EncodedValue evStr) =
    case evStr of
        "SNR" -> Just SNotRelevant
        "SUNK" -> Just SYetUnknown
        _ ->
            case String.split (String.Pattern "/") evStr of
                [ gotStr, totalStr ] ->
                    (\got total -> SGotTotal { got, total })
                    <$> Int.fromString gotStr
                    <*> Int.fromString totalStr
                [ gotStr, onTheWayStr, totalStr ] ->
                    (\got onTheWay total -> SWithProgress { got, onTheWay, total })
                    <$> Int.fromString gotStr
                    <*> Int.fromString onTheWayStr
                    <*> Int.fromString totalStr
                _ ->
                    mbPTag
                        >>= \pTag -> decodeProgress pTag (CT.EncodedValue evStr)
                        <#> SFromProgress