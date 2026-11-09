module Report.Decorators.Tabular.Logbook where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Yoga.JSON (class ReadForeign, class WriteForeign)

import Report.Core (SDateRec)
import Report.Decorators.Task (TaskP)


{- Logbook: a history of the item, such as clocked work periods or task state changes -}


type LogbookEntry =
    { name  :: String
    , task  :: TaskP
    , start :: Maybe SDateRec
    , end   :: Maybe SDateRec
    }


newtype Logbook = Logbook (Array LogbookEntry)


derive instance Newtype Logbook _
derive newtype instance Eq Logbook
derive newtype instance ReadForeign Logbook
derive newtype instance WriteForeign Logbook


entries :: Logbook -> Array LogbookEntry
entries (Logbook theEntries) = theEntries
