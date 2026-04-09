module Report.Convert.Text.Decorators.Tags where

import Prelude

import Data.Maybe (Maybe)
import Data.Array (catMaybes) as Array
import Data.Array.NonEmpty (toArray) as NEA
import Data.String (split, joinWith, Pattern(..)) as String
import Data.Newtype (unwrap, wrap)


import Report.Core.Logic as CT
import Report.Chain (Chain)
import Report.Chain as Chain
import Report.Class (class ConvertTo, class ConvertFrom, encodeTo, decodeFrom)
import Report.Decorators.Tags (Tags(..), RawTag(..))


encodeTags :: forall t. ConvertTo (Chain String) t => Tags t -> CT.EncodedValue
encodeTags (Tags ts) = CT.EncodedValue $ String.joinWith "," $ Chain.toString <$> encodeTo <$> ts


decodeTags :: forall t. ConvertFrom (Chain String) t => CT.EncodedValue -> Tags t
decodeTags = unwrap >>> String.split (String.Pattern ",") >>> map Chain.fromString >>> Array.catMaybes >>> map decodeFrom >>> Array.catMaybes >>> Tags


rawifyTag :: forall @t. ConvertTo (Chain String) t => t -> RawTag
rawifyTag = encodeTo >>> Chain.toNEArray >>> RawTag


derawifyTag :: forall @t. ConvertFrom (Chain String) t => RawTag -> Maybe t
derawifyTag = unwrap >>> Chain.fromNEArray >>> decodeFrom


rawifyTags :: forall t. ConvertTo (Chain String) t => Tags t -> Tags RawTag
rawifyTags = unwrap >>> map rawifyTag >>> wrap


derawifyTags :: forall t. ConvertFrom (Chain String) t => Tags RawTag -> Tags t
derawifyTags = unwrap >>> map derawifyTag >>> Array.catMaybes >>> wrap


toArray :: forall t. ConvertTo (Chain String) t => Tags t -> Array String
toArray = unwrap >>> map encodeTo >>> map Chain.toString


fromArray :: forall t. ConvertFrom (Chain String) t => Array String -> Tags t
fromArray = map Chain.fromString >>> Array.catMaybes >>> map decodeFrom >>> Array.catMaybes >>> Tags