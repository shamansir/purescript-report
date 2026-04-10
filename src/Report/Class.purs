module Report.Class where

import Prelude

import Foreign (Foreign, F, fail, ForeignError(..))

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Array (catMaybes) as Array
import Data.Array.NonEmpty (toArray) as NEA
import Data.Newtype (unwrap, wrap)

import Report.Chain (Chain(..))
import Report.Chain (fromNEArray, toNEArray, toString, fromString, last) as Chain
import Report.GroupPath (GroupPath) as S
import Report.Tabular (Tabular)
import Report.Decorator (Decorators)
import Report.Decorators.Stats (Stats) as S
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Decorators.Tags (Tags, RawTag(..))

import Yoga.JSON (readImpl, writeImpl)


class HasTags t a where
    i_tags :: a -> Array t


class HasDecorators a where
    i_decorators :: a -> Decorators


class HasTabular a where
    i_tabular :: a -> Tabular TabularValue


class HasStats a where
    i_stats :: a -> S.Stats


class IsItem a where
    i_title :: a -> String


class IsGroup a where
    g_title :: a -> String
    g_path :: a -> S.GroupPath


class IsSubjectId i a where
    s_id :: a -> i
    s_unique :: i -> String -- FIXME: use `ConvertTo`
    s_decode :: String -> Maybe i -- FIXME: use `ConvertFrom`


class IsSubjectId i a <= IsSubject i a where
    s_name :: a -> String


type TagColors =
    { text :: String
    , background :: String
    , border :: String
    }


-- newtype GChain g = GChain (Chain g)


class IsGroup g <= IsGroupable g t where
    t_group :: t -> Maybe (Chain g)


class LimitedSet t where
    values :: Array t


class ConvertTo trg src where
    convertTo :: src -> trg -- LAW: convertFrom (convertTo src) === Just src


class ConvertFrom trg src where
    convertFrom :: trg -> Maybe src -- LAW: convertFrom (convertTo src) === Just src


{-
class ChainContent a t where
    chainContent :: t -> Chain a
-}


instance ConvertTo (Chain String) Unit where
    convertTo _ = End "."


instance ConvertFrom (Chain String) Unit where
    convertFrom s = if s == End "." then Just unit else Nothing


class Same k where -- alternative to `Eq`, not strict equality, but "same kind of thing", e.g. same tag type, same rating type, same platform type, etc.
    same :: k -> k -> Boolean


instance Same Unit where same = eq


-- used for tags, so when we sort items by a tag, we can find the "same kind" tag on each item, i.e. rating or platform
-- and if they are the same, sort by their `Ord` instance
class Same k <= IsSortable k t where
    kindOf :: t -> k -- FIXME: ConvertTo k t
    -- defaultFor :: k -> Maybe t  -- FIXME: ConvertFrom k t
    {-
    kindContent :: t -> Chain String
    kindId :: t -> String -- TODO: kind as another type var, kind can be `IsTag`.... as well
    fromKindId :: String -> Maybe t -- TODO: kind as another type var, kind can be `IsTag`.... as well
    -}


class IsTag t where
    tagColors :: t -> TagColors
    tagContent :: t -> Chain String -- different from encoding / decoding since could contain formatted text and any unicode



defaultTagColors :: TagColors
defaultTagColors =
    { text: "#000000"
    , background: "#FFFFFF"
    , border: "#CCCCCC"
    }


mkChainEncode :: forall trg. (trg -> String) -> (trg -> Chain String)
mkChainEncode toStr = toStr >>> End


mkChainEncode' :: forall trg. ConvertTo String trg => (trg -> Chain String)
mkChainEncode' = mkChainEncode convertTo


mkChainDecode :: forall trg. (String -> Maybe trg) -> (Chain String -> Maybe trg)
mkChainDecode fromStr = Chain.last >>> fromStr


mkChainDecode' :: forall trg. ConvertFrom String trg => (Chain String -> Maybe trg)
mkChainDecode' = mkChainDecode convertFrom


defaultWriteImpl :: forall trg. ConvertTo (Chain String) trg => trg -> Foreign
defaultWriteImpl = convertTo >>> Chain.toString >>> writeImpl


defaultReadImpl :: forall trg. ConvertFrom (Chain String) trg => Foreign -> F trg
defaultReadImpl frgn = do
    str <- readImpl frgn
    case Chain.fromString str of
        Just strChain -> case convertFrom strChain of
            Just trg -> pure trg
            Nothing -> fail $ ForeignError $ "failed to decode target from " <> str
        Nothing  -> fail $ ForeignError $ "failed to decode target from " <> str


instance IsTag Unit where
    tagColors _ = defaultTagColors
    tagContent _ = End "."
    -- encodeTag = const "."
    -- decodeTag _ = Just unit
    -- allTags = [unit]


instance IsTag String where
    tagColors _ = defaultTagColors
    tagContent s = End s


instance ConvertTo String Unit where convertTo _ = "."
instance ConvertFrom String Unit where convertFrom s = if s == "." then Just unit else Nothing
instance ConvertTo String String where convertTo = identity
instance ConvertFrom String String where convertFrom = Just


instance IsTag RawTag where
    tagColors _ = defaultTagColors
    tagContent (RawTag rtags) = Chain.fromNEArray rtags


instance ConvertTo String RawTag where convertTo (RawTag rtags) = Chain.fromNEArray rtags # Chain.toString
instance ConvertFrom String RawTag where convertFrom = Chain.fromString >>> map Chain.toNEArray >>> map RawTag
instance ConvertTo (Chain String) RawTag where convertTo (RawTag rtags) = Chain.fromNEArray rtags
instance ConvertFrom (Chain String) RawTag where convertFrom = Chain.toNEArray >>> RawTag >>> Just


instance LimitedSet Unit where
    values = [ unit ]


{-
rawifyTag :: forall @t. IsTag t => t -> RawTag
rawifyTag = tagContent >>> Chain.toNEArray >>> RawTag
-}


{-
derawifyTag :: forall @t. Convert String t => RawTag -> Maybe t
derawifyTag (RawTag rtags) = convertFrom $ String.joinWith "::" $ NEA.toArray rtags
-}


{-
rawifyTags :: forall @t. IsTag t => Tags t -> Tags RawTag
rawifyTags = unwrap >>> map rawifyTag >>> wrap
-}


{-
derawifyTags :: forall @t. Convert String t => Tags RawTag -> Tags t
derawifyTags = unwrap >>> map derawifyTag >>> Array.catMaybes >>> wrap
-}