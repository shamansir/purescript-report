module Report.Convert.Text.Decorator where

import Prelude

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Report.Core.Logic (EncodedValue(..)) as CT
import Report.GroupPath as GP
import Report.Decorator (Decorator(..), Key(..))
import Report.Convert.Text.Decorators.Progress as PEnc
import Report.Convert.Text.Decorators.Rating as REnc
import Report.Convert.Text.Decorators.Priority as PrEnc
import Report.Convert.Text.Decorators.Task as TEnc


encodeDecorator :: Decorator -> Key /\ CT.EncodedValue
encodeDecorator s = case s of
    PRating rating ->
        KRating /\ REnc.encodeRating rating
    PPriority priority ->
        KPriority /\ PrEnc.encodePriority priority
    PTask task ->
        KTask /\ TEnc.encodeTask task
    SProgress progress -> case PEnc.encodeProgress progress of
        pkey /\ ev -> KProgress pkey /\ ev
    SEarnedAt sdate ->
        KEarnedAt /\ (CT.EncodedValue $ PEnc.encodeDate sdate)
    SDescription desc ->
        KDescription /\ CT.EncodedValue desc
    SReference gpath ->
        KReference /\ (CT.EncodedValue $ GP.encode gpath)


decodeDecorator :: Key -> CT.EncodedValue -> Maybe Decorator
decodeDecorator key encVal@(CT.EncodedValue evStr) = case key of
    KRating ->
        REnc.decodeRating encVal
            <#> PRating
    KPriority ->
        PrEnc.decodePriority encVal
            <#> PPriority
    KTask ->
        TEnc.decodeTask encVal
            <#> PTask
    KProgress pTag ->
        PEnc.decodeProgress pTag encVal
            <#> SProgress
    KEarnedAt ->
        PEnc.decodeDate evStr
            <#> SEarnedAt
    KDescription ->
        Just $ SDescription evStr
    KReference ->
        GP.decode evStr <#> SReference
