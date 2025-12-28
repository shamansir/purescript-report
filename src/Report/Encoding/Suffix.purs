module Report.Encoding.Suffix where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Report.Core as CT
import Report.Class (class IsTag, tagContent)
import Report.GroupPath as GP
import Report.Suffix (Suffix(..), Key(..))
import Report.Modifiers.Tags (Tags)
import Report.Encoding.Modifiers.Progress as PEnc
import Report.Encoding.Modifiers.Tags as TEnc


encodeSuffix :: forall t. IsTag t => Suffix t -> Key /\ CT.EncodedValue
encodeSuffix s = case s of
    SProgress progress -> case PEnc.encodeProgress progress of
        pkey /\ ev -> KProgress pkey /\ ev
    SEarnedAt sdate ->
        KEarnedAt /\ (CT.EncodedValue $ PEnc.encodeDate sdate)
    SDescription desc ->
        KDescription /\ CT.EncodedValue desc
    SReference gpath ->
        KReference /\ (CT.EncodedValue $ GP.encode gpath)
    STags tags ->
        KTags /\ (CT.EncodedValue $ TEnc.encodeTags tags)


decodeSuffix :: forall t. IsTag t =>Key -> CT.EncodedValue -> Maybe (Suffix t)
decodeSuffix key (CT.EncodedValue evStr) = case key of
    KProgress pTag ->
        PEnc.decodeProgress pTag (CT.EncodedValue evStr)
            <#> SProgress
    KEarnedAt ->
        PEnc.decodeDate evStr
            <#> SEarnedAt
    KDescription ->
        Just $ SDescription evStr
    KReference ->
        GP.decode evStr <#> SReference
    KTags ->
        Just $ STags $ TEnc.decodeTags evStr