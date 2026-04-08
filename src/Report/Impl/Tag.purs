module Report.Impl.Tag where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String

import Foreign (F, Foreign, fail, ForeignError(..))

import Yoga.JSON (readImpl, writeImpl)

import Report.Decorators.Tags (RawTag)
import Report.Class (TagColors)
import Report.Chain as MbW


type Tag = RawTag


altDefaultColors :: TagColors
altDefaultColors =
    { text: "#000000"
    , background: "#adceffff"
    , border: "#000000"
    }


{-
defaultWriteImpl :: forall tag. IsTag tag => tag -> Foreign
defaultWriteImpl = tagContent >>> MbW.toArray >>> String.joinWith "::" >>> writeImpl


defaultReadImpl :: forall tag. IsTag tag => Foreign -> F tag
defaultReadImpl frgn = do
    str <- readImpl frgn
    case decodeTag @tag str of -- FIXME: doesn't do Chain reconstruction
        Just tag -> pure tag
        Nothing  -> fail $ ForeignError $ "failed to decode tag from " <> str
-}