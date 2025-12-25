module Report.Task where

import Prelude

import Foreign (F, Foreign)

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)


data TaskP
    = TTodo
    | TDone
    | TDoing
    | TWait
    | TCanceled
    | TNow
    | TLater


instance Show TaskP where
    show = case _ of
        TTodo -> "TODO"
        TDone -> "DONE"
        TDoing -> "DOING"
        TWait -> "WAIT"
        TCanceled -> "CANCELED"
        TNow -> "NOW"
        TLater -> "LATER"
derive instance Eq TaskP


instance ReadForeign TaskP where
    readImpl f = (readImpl f :: F String) <#> taskPFromString

instance WriteForeign TaskP where
    writeImpl = writeImpl <<< taskPToString


taskPFromString :: String -> TaskP
taskPFromString = case _ of
    "TODO" -> TTodo
    "DOING" -> TDoing
    "DONE" -> TDone
    "WAIT" -> TWait
    "CANCELED" -> TCanceled
    "NOW" -> TNow
    "LATER" -> TLater
    _ -> TTodo


taskPToString :: TaskP -> String
taskPToString = case _ of
    TTodo -> "TODO"
    TDoing -> "DOING"
    TDone -> "DONE"
    TWait -> "WAIT"
    TCanceled -> "CANCELED"
    TNow -> "NOW"
    TLater -> "LATER"


