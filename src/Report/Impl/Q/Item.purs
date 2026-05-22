module Report.Impl.Q.Item where

import Prelude

import Data.Newtype (wrap, unwrap)
import Data.Tuple.Nested ((/\), type (/\))


import Report.Decorator (Decorators(..))
import Report.Decorators.Tags (Tags)
import Report.Impl.Item
import Report.Tabular as Tabular
import Report.Decorators.Tabular.TabularValue


mk :: forall item_tag. String -> Item item_tag
mk = init


tags :: forall item_tag. Array item_tag -> Item item_tag -> Item item_tag
tags nextTags = unwrap >>> _ { tags = wrap nextTags } >>> wrap


decs :: forall item_tag. Decorators -> Item item_tag -> Item item_tag
decs nextDecs = unwrap >>> _ { decorators = nextDecs } >>> wrap


tabs :: forall item_tag. Array (String /\ TabularValue) -> Item item_tag -> Item item_tag
tabs nextTabs = unwrap >>> _ { tabular = Tabular.fromArray nextTabs } >>> wrap