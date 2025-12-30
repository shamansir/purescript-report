module Report.Encoding.Prefix where

import Prelude


import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Class (class IsTag)
import Report.GroupPath as GP
import Report.Prefix (Prefix(..), Key(..))
import Report.Encoding.Modifiers.Rating as REnc
import Report.Encoding.Modifiers.Priority as PEnc
import Report.Encoding.Modifiers.Task as TEnc


encodePrefix :: Prefix -> Key /\ CT.EncodedValue
encodePrefix s = case s of
    PRating rating ->
        KRating /\ REnc.encodeRating rating
    PPriority priority ->
        KPriority /\ PEnc.encodePriority priority
    PTask task ->
        KTask /\ TEnc.encodeTask task


decodePrefix :: Key -> CT.EncodedValue -> Maybe Prefix
decodePrefix  key encVal = case key of
    KRating ->
        REnc.decodeRating encVal
            <#> PRating
    KPriority ->
        PEnc.decodePriority encVal
            <#> PPriority
    KTask ->
        TEnc.decodeTask encVal
            <#> PTask