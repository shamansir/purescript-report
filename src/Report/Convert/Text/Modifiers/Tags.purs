module Report.Convert.Text.Modifiers.Tags where

import Prelude

import Data.Array (catMaybes) as Array
import Data.String (split, joinWith, Pattern(..)) as String
import Data.Newtype (unwrap)


import Report.Core.Logic as CT
import Report.Chain as MbW
import Report.Class (class IsTag, tagContent, decodeTag)
import Report.Modifiers.Tags (Tags(..))


encodeTags :: forall t. IsTag t => Tags t -> CT.EncodedValue
encodeTags (Tags ts) = CT.EncodedValue $ String.joinWith "," $ MbW.toString <$> tagContent <$> ts


decodeTags :: forall t. IsTag t => CT.EncodedValue -> Tags t
decodeTags = unwrap >>> String.split (String.Pattern ",") >>> map decodeTag >>> Array.catMaybes >>> Tags -- FIXME: doesn't do Chain reconstruction