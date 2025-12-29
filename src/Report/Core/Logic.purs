module Report.Core.Logic where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)


data ViewOrEdit a
    = View a
    | Edit EncodedValue a


derive instance Functor ViewOrEdit


loadViewOrEdit :: forall a. ViewOrEdit a -> a
loadViewOrEdit = case _ of
    View a -> a
    Edit _ a -> a


isViewing :: forall a. ViewOrEdit a -> Boolean
isViewing = case _ of
    View _ -> true
    Edit _ _ -> false


isEditing :: forall a. ViewOrEdit a -> Boolean
isEditing = case _ of
    View _ -> false
    Edit _ _ -> true


view :: forall a. a -> ViewOrEdit a
view = View


edit :: forall a. EncodedValue -> a -> ViewOrEdit a
edit = Edit


voeFromMaybe :: forall a. Maybe EncodedValue -> a -> ViewOrEdit a
voeFromMaybe = maybe view edit


newtype EncodedValue = EncodedValue String
derive instance Newtype EncodedValue _
derive newtype instance Eq EncodedValue