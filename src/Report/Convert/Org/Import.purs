module Report.Convert.Org.Import where

import Prelude

import Foreign (F, Foreign)

import Control.Alt ((<|>))

import Data.Maybe (Maybe)

import Report.Convert.Types (SubjectId)
import Report.Modifiers.Progress (DateRec)
import Report.Tabular (Tabular)
import Report.Modifiers.Tabular.TabularValue (TabularValue)


import Yoga.JSON (class ReadForeign, readImpl)


foo = 42