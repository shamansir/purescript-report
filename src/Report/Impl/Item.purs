module Report.Impl.Item where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Class
import Report.Decorator (Decorators, Decorator, Key)
import Report.Decorator (empty) as Decorators
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Modify
import Report.Decorators.Tags (Tags)
import Report.Decorators.Tags (fromArray, toArray, RawTag, empty, catMaybes) as Tags
import Report.Decorators.Tabular.TabularValue (TabularValue)

import Yoga.JSON (class WriteForeign, class ReadForeign)


type ItemRec item_tag =
    { title :: String
    , decorators :: Decorators
    , tabular :: Tabular TabularValue
    , tags :: Tags item_tag
    }


newtype Item item_tag = Item (ItemRec item_tag)
derive instance Newtype (Item item_tag) _


instance Functor Item where
    map = mapTags


instance IsItem (Item item_tag) where
    i_title = _.title <<< unwrap


instance HasDecorators (Item item_tag) where
    i_decorators = unwrap >>> _.decorators


instance HasTabular (Item item_tag) where
    i_tabular = _.tabular <<< unwrap


instance HasTags item_tag (Item item_tag) where
    i_tags = unwrap >>> _.tags >>> Tags.toArray


instance ItemModify (Item item_tag) where
    setItemName newName =
        unwrap >>> _ { title = newName } >>> wrap


instance DecoratorsModify (Item item_tag) where
    updateDecorators nextDecorators =
        unwrap >>> _ { decorators = nextDecorators } >>> wrap


instance TagsModify item_tag (Item item_tag) where
    updateTags nextTags =
        unwrap >>> _ { tags = nextTags } >>> wrap


init :: forall item_tag. String -> Item item_tag
init name =
    Item
        { title: name
        , decorators: Decorators.empty
        , tabular: Tabular.empty
        , tags : Tags.empty
        }


from :: forall item item_tag.
    IsItem item =>
    HasDecorators item =>
    HasTags item_tag item =>
    HasTabular item =>
    item ->
    Item item_tag
from item =
    Item
        { title: i_title item
        , decorators: i_decorators item
        , tabular: i_tabular item
        , tags: Tags.fromArray $ i_tags item
        }


mapTags :: forall item_tag_a item_tag_b. (item_tag_a -> item_tag_b) -> Item item_tag_a -> Item item_tag_b
mapTags mapF (Item itemRec) =
    Item $ itemRec { tags = mapF <$> itemRec.tags }


catMaybesOfTags :: forall item_tag. Item (Maybe item_tag) -> Item item_tag
catMaybesOfTags (Item itemRec) =
    Item $ itemRec { tags = Tags.catMaybes itemRec.tags }


derive newtype instance ReadForeign  (Item Tags.RawTag)
derive newtype instance WriteForeign (Item Tags.RawTag)