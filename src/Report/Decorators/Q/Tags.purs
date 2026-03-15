module Report.Decorators.Q.Tags where

import Prelude

import Report.Decorators.Tags (Tags(..))


tags :: forall t. Array t -> Tags t
tags = Tags


tag :: forall t. t -> Tags t
tag = Tags <<< pure