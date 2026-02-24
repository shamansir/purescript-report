module Report.Convert.Text.Decorators.Rating where

import Prelude

import Data.Maybe (Maybe)
import Data.Number (fromString) as Number

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Decorators.Rating (Rating)
import Report.Decorators.Rating (toString, fromString) as Rating


encodeRating :: Rating -> CT.EncodedValue
encodeRating = CT.EncodedValue <<< Rating.toString


decodeRating :: CT.EncodedValue -> Maybe Rating
decodeRating (CT.EncodedValue evStr) = Rating.fromString evStr