module Report.Core.Logic where

import Prelude

import Data.Newtype (class Newtype)


data ViewOrEdit a
    = View a
    | Edit a


loadViewOrEdit :: forall a. ViewOrEdit a -> a
loadViewOrEdit = case _ of
    View a -> a
    Edit a -> a


newtype EncodedValue = EncodedValue String
derive instance Newtype EncodedValue _
derive newtype instance Eq EncodedValue