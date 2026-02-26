module Report.Impl.Item where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Class
import Report.Decorator (Decorator, Key)
import Report.Decorator (empty, getTags) as Decorator
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Modify
import Report.Decorators.Tags (toArray) as Tags
import Report.Decorators.Tabular.TabularValue (TabularValue)

import Yoga.JSON (class WriteForeign, class ReadForeign)


type ItemRec item_tag =
    { title :: String
    , decorators :: Decorator item_tag
    , tabular :: Tabular TabularValue
    , locked :: Boolean
    }


newtype Item item_tag = Item (ItemRec item_tag)
derive instance Newtype (Item item_tag) _


instance IsItem (Item item_tag) where
    i_name = _.title <<< unwrap
    i_mbTitle item =
        let title = i_name item
        in  if title == "" then Nothing else Just title
    i_locked = _.locked <<< unwrap


instance HasDecorators item_tag (Item item_tag) where
    i_decorators = _.decorators <<< unwrap


instance HasTabular (Item item_tag) where
    i_tabular = _.tabular <<< unwrap


instance HasModifiers item_tag (Item item_tag)


instance HasTags item_tag (Item item_tag) where
    i_tags = unwrap >>> _.decorators >>> Decorator.getTags >>> maybe [] Tags.toArray


instance ItemModify (Item item_tag) where
    setItemName newName =
        unwrap >>> _ { title = newName } >>> wrap


instance SuffixesModify item_tag (Item item_tag) where
    updateSuffixes nextSuffixes =
        unwrap >>> _ { suffixes = nextSuffixes } >>> wrap


instance PrefixesModify (Item item_tag) where
    updatePrefixes nextPrefixes =
        unwrap >>> _ { prefixes = nextPrefixes } >>> wrap


init :: forall item_tag. String -> Item item_tag
init name =
    Item
        { title: name
        , prefixes: Prefix.empty
        , suffixes: Suffix.empty
        , tabular: Tabular.empty
        , locked: false
        }


from :: forall item item_tag.
    IsItem item =>
    HasPrefixes item =>
    HasSuffixes item_tag item =>
    HasTabular item =>
    item ->
    Item item_tag
from item =
    Item
        { title: i_name item
        , prefixes: i_prefixes item
        , suffixes: i_suffixes item
        , tabular: i_tabular item
        , locked: i_locked item
        }


derive newtype instance (ReadForeign item_tag)  => ReadForeign  (Item item_tag)
derive newtype instance (WriteForeign item_tag) => WriteForeign (Item item_tag)