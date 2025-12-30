module Report.Encoding.Modifiers.Rating where

import Prelude

import Data.Maybe (Maybe)
import Data.Number (fromString) as Number

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Modifiers.Rating (Rating)
import Report.Modifiers.Rating (fromNumber, toNumber) as Rating


encodeRating :: Rating -> CT.EncodedValue
encodeRating = CT.EncodedValue <<< show <<< Rating.toNumber


decodeRating :: CT.EncodedValue -> Maybe Rating
decodeRating (CT.EncodedValue evStr) = Rating.fromNumber <$> Number.fromString evStr