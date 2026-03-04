module Report.Impl.Item where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Class
import Report.Decorator (Decorators, Decorator, Key)
import Report.Decorator (empty, getTags) as Decorators
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Modify
import Report.Decorators.Tags (toArray, RawTag) as Tags
import Report.Decorators.Tabular.TabularValue (TabularValue)

import Yoga.JSON (class WriteForeign, class ReadForeign)


type ItemRec item_tag =
    { title :: String
    , decorators :: Decorators item_tag
    , tabular :: Tabular TabularValue
    }


newtype Item item_tag = Item (ItemRec item_tag)
derive instance Newtype (Item item_tag) _


instance IsItem (Item item_tag) where
    i_title = _.title <<< unwrap


instance HasDecorators item_tag (Item item_tag) where
    i_decorators = unwrap >>> _.decorators


instance HasTabular (Item item_tag) where
    i_tabular = _.tabular <<< unwrap


instance HasTags item_tag (Item item_tag) where
    i_tags = unwrap >>> _.decorators >>> Decorators.getTags >>> maybe [] Tags.toArray


instance ItemModify (Item item_tag) where
    setItemName newName =
        unwrap >>> _ { title = newName } >>> wrap


instance DecoratorsModify item_tag (Item item_tag) where
    updateDecorators nextDecorators =
        unwrap >>> _ { decorators = nextDecorators } >>> wrap


init :: forall item_tag. String -> Item item_tag
init name =
    Item
        { title: name
        , decorators: Decorators.empty
        , tabular: Tabular.empty
        }


from :: forall item item_tag.
    IsItem item =>
    HasDecorators item_tag item =>
    HasTabular item =>
    item ->
    Item item_tag
from item =
    Item
        { title: i_title item
        , decorators: i_decorators item
        , tabular: i_tabular item
        }


derive newtype instance ReadForeign  (Item Tags.RawTag)
derive newtype instance WriteForeign (Item Tags.RawTag)