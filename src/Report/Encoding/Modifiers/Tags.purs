module Report.Encoding.Modifiers.Tags where

import Prelude


import Data.Array (catMaybes) as Array
import Data.String (split, joinWith, Pattern(..)) as String


import Report.Class (class IsTag, tagContent, decodeTag)
import Report.Modifiers.Tags (Tags(..))


encodeTags :: forall t. IsTag t => Tags t -> String
encodeTags (Tags ts) = String.joinWith "," $ tagContent <$> ts


decodeTags :: forall t. IsTag t => String -> Tags t
decodeTags = String.split (String.Pattern ",") >>> map decodeTag >>> Array.catMaybes >>> Tags