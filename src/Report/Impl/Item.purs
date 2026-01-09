module Report.Impl.Item where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Class
import Report.Prefix (Prefixes)
import Report.Prefix (empty) as Prefix
import Report.Suffix (Suffixes)
import Report.Suffix (empty, getTags) as Suffix
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Modifiers.Tags (toArray) as Tags
import Report.Modifiers.Tabular.TabularValue (TabularValue)

import Yoga.JSON (class WriteForeign, class ReadForeign)


type ItemRec t =
    { title :: String
    , prefixes :: Prefixes
    , suffixes :: Suffixes t
    , tabular :: Tabular TabularValue
    , locked :: Boolean
    }


newtype Item t = Item (ItemRec t)
derive instance Newtype (Item t) _


instance IsItem (Item t) where
    i_name = _.title <<< unwrap
    i_mbTitle item =
        let title = i_name item
        in  if title == "" then Nothing else Just title
    i_locked = _.locked <<< unwrap


instance HasPrefixes (Item t) where
    i_prefixes = _.prefixes <<< unwrap


instance HasSuffixes t (Item t) where
    i_suffixes = _.suffixes <<< unwrap


instance HasTabular (Item t) where
    i_tabular = _.tabular <<< unwrap


instance HasModifiers t (Item t)


instance HasTags t (Item t) where
    i_tags = unwrap >>> _.suffixes >>> Suffix.getTags >>> maybe [] Tags.toArray


init :: forall t. String -> Item t
init name =
    Item
        { title: name
        , prefixes: Prefix.empty
        , suffixes: Suffix.empty
        , tabular: Tabular.empty
        , locked: false
        }


from :: forall a t.
    IsItem a =>
    HasPrefixes a =>
    HasSuffixes t a =>
    HasTabular a =>
    a ->
    Item t
from item =
    Item
        { title: i_name item
        , prefixes: i_prefixes item
        , suffixes: i_suffixes item
        , tabular: i_tabular item
        , locked: i_locked item
        }


-- derive newtype instance (ReadForeign item_tag)  => ReadForeign  (Item item_tag)
-- derive newtype instance (WriteForeign item_tag) => WriteForeign (Item item_tag)