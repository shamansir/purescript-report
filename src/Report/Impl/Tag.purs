module Report.Impl.Tag where

import Prelude

import Data.Maybe (Maybe(..))

import Foreign (F, Foreign, fail, ForeignError(..))

import Yoga.JSON (readImpl, writeImpl)

import Report.Class (class IsTag, TagColors, decodeTag, tagContent)




defaultColors :: TagColors
defaultColors =
    { text: "#000000"
    , background: "#adceffff"
    , border: "#000000"
    }


defaultWriteImpl :: forall tag. IsTag tag => tag -> Foreign
defaultWriteImpl = tagContent >>> writeImpl


defaultReadImpl :: forall tag. IsTag tag => Foreign -> F tag
defaultReadImpl frgn = do
    str <- readImpl frgn
    case decodeTag @tag str of
        Just tag -> pure tag
        Nothing  -> fail $ ForeignError $ "failed to decode tag from " <> str