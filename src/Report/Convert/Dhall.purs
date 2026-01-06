module Report.Convert.Dhall where

import Prelude

import Foreign (Foreign, F)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (uncurry) as Tuple
import Data.String (joinWith) as String
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)

import Yoga.JSON (class WriteForeign, writePrettyJSON, class ReadForeign, readImpl)

import Report (Report)
import Report.Core as CT
import Report.Group (Group)
import Report.GroupPath (GroupPath)
import Report.Class (class IsGroup, class IsItem, class IsSubject, class IsTag, tagContent)
import Report.Modifiers.Class.ValueModify (class EncodableKey, decodeKey)
import Report.Convert.Types
import Report.Convert.Generic (class ToExport, toExport) as Report

import Report.Modifiers.Progress (Progress(..), PValueTag(..))
import Report.Modifiers.Task (TaskP(..))
import Report.Modifiers.Tags (Tags)
import Report.Prefix (Key(..)) as P
import Report.Suffix (Key(..)) as S

import Dodo
import Dodo as D

import Report.Convert.Dhall.Export as Export


toDhall
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => ReadForeign item_tag
    => IsTag item_tag
    => Report subj group item
    -> String
toDhall =
    Export.toDhall @x @subj_id @subj_tag @item_tag