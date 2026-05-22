module Report.Impl.Q.Group where

import Prelude

import Report.GroupPath as GP
import Report.Decorators.Stats (Stats)
import Report.Impl.Group


mk :: Array String → String → Group
mk = mkGroup <<< map GP.PathSegment


stats :: Stats -> Group -> Group
stats = setStats