module Report.Impl.Group where

import Prelude


import Report.Group as Core

import Data.Newtype (class Newtype, unwrap, wrap)

import Report.Modifiers.Stats (Stats)
import Report.GroupPath (GroupPath)


type Group = Core.Group


rootGroup ∷ Stats → Group
rootGroup = Core.rootGroup


isGroupAt ∷ GroupPath → Group → Boolean
isGroupAt = Core.isGroupAt


pathOf ∷ Group → GroupPath
pathOf = Core.pathOf


setStats ∷ Stats → Group → Group
setStats = Core.setStats