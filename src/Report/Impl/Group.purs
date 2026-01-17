module Report.Impl.Group where

import Prelude


import Report.Group as Core

import Data.Newtype (class Newtype, unwrap, wrap)

import Report.Modifiers.Stats (Stats)
import Report.GroupPath (GroupPath, PathSegment)


type Group = Core.Group


rootGroup ∷ Group
rootGroup = Core.rootGroup


rootGroupWith ∷ Stats → Group
rootGroupWith = Core.rootGroupWith


mkGroup ∷ Array PathSegment → String → Group
mkGroup = Core.mkGroup


mkGroupWith ∷ Array PathSegment → String → Stats -> Group
mkGroupWith = Core.mkGroupWith


isGroupAt ∷ GroupPath → Group → Boolean
isGroupAt = Core.isGroupAt


pathOf ∷ Group → GroupPath
pathOf = Core.pathOf


setStats ∷ Stats → Group → Group
setStats = Core.setStats