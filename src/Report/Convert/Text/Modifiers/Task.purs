module Report.Convert.Text.Modifiers.Task where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Modifiers.Task (TaskP, taskPFromString, taskPToString)


encodeTask :: TaskP -> CT.EncodedValue
encodeTask = CT.EncodedValue <<< taskPToString


decodeTask :: CT.EncodedValue -> Maybe TaskP
decodeTask (CT.EncodedValue evStr) = Just $ taskPFromString evStr