module Report.Encoding.Modifiers.Priority where

import Prelude

import Data.Maybe (Maybe)

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Modifiers.Priority (Priority, priorityChar, fromString)


encodePriority :: Priority -> CT.EncodedValue
encodePriority = CT.EncodedValue <<< priorityChar


decodePriority :: CT.EncodedValue -> Maybe Priority
decodePriority (CT.EncodedValue evStr) = fromString evStr