module Report.Convert.Smos.Types where

import Prelude

import Foreign (F, Foreign)
import Foreign.Index as FI

import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Array (null) as Array

import Yoga.JSON (class ReadForeign, readImpl)
import Foreign.Object (Object)


type SmosFileRec =
    { version :: Int
    , value   :: Array SmosTree
    }


newtype SmosFile = SmosFile SmosFileRec
derive newtype instance ReadForeign SmosFile


newtype SmosTree = SmosTree
    { entry  :: SmosEntry
    , forest :: Maybe (Array SmosTree)
    }


instance ReadForeign SmosTree where
    readImpl f = do
        entry  <- FI.readProp "entry"  f >>= readImpl
        forest <- FI.readProp "forest" f >>= readImpl
        pure $ SmosTree { entry, forest }


isLeafTree :: SmosTree -> Boolean
isLeafTree (SmosTree { forest }) =
    isNothing forest || Array.null (fromMaybe [] forest)


type SmosStateEntryRec =
    { state :: Maybe String
    , time  :: String
    }


newtype SmosStateEntry = SmosStateEntry SmosStateEntryRec
derive newtype instance ReadForeign SmosStateEntry


type SmosLogbookEntryRec =
    { start :: String
    , end   :: Maybe String
    }


newtype SmosLogbookEntry = SmosLogbookEntry SmosLogbookEntryRec
derive newtype instance ReadForeign SmosLogbookEntry


newtype SmosEntry = SmosEntry
    { header       :: String
    , contents     :: Maybe String
    , timestamps   :: Maybe (Object String)
    , properties   :: Maybe (Object String)
    , stateHistory :: Maybe (Array SmosStateEntry)
    , tags         :: Maybe (Array String)
    , logbook      :: Maybe (Array SmosLogbookEntry)
    }


instance ReadForeign SmosEntry where
    readImpl f = do
        header       <- FI.readProp "header"        f >>= readImpl
        contents     <- FI.readProp "contents"      f >>= readImpl
        timestamps   <- FI.readProp "timestamps"    f >>= readImpl
        properties   <- FI.readProp "properties"    f >>= readImpl
        stateHistory <- FI.readProp "state-history" f >>= readImpl
        tags         <- FI.readProp "tags"          f >>= readImpl
        logbook      <- FI.readProp "logbook"       f >>= readImpl
        pure $ SmosEntry { header, contents, timestamps, properties, stateHistory, tags, logbook }
