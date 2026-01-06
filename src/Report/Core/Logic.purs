module Report.Core.Logic where

import Prelude

import Foreign (F)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (hush)
import Data.Newtype (class Newtype)


data ViewOrEdit a
    = View a
    | Edit EncodedValue a


derive instance Functor ViewOrEdit


loadViewOrEdit :: forall a. ViewOrEdit a -> a
loadViewOrEdit = case _ of
    View a -> a
    Edit _ a -> a


loadEncoded :: forall a. ViewOrEdit a -> Maybe EncodedValue
loadEncoded = case _ of
    View _ -> Nothing
    Edit ev _ -> Just ev


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


foreignToMaybe :: forall a. F a -> Maybe a
foreignToMaybe = runExcept >>> hush