module Report.Decorators.Task where

import Prelude

import Foreign (F)

import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)


data TaskP
    = TTodo
    | TDone
    | TDoing
    | TWait
    | TCanceled
    | TNow
    | TLater

-- Feedback
-- Verify
-- Delegated
-- Report
-- Bug
-- KnownCause
-- Fixed


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


instance DecodeJson TaskP where
    decodeJson js = (decodeJson js :: Either JsonDecodeError String) <#> taskPFromString
instance EncodeJson TaskP where
    encodeJson = taskPToString >>> encodeJson


taskPFromString :: String -> TaskP
taskPFromString = mbTaskPFromString >>> fromMaybe TTodo


mbTaskPFromString :: String -> Maybe TaskP
mbTaskPFromString = case _ of
    "TODO" -> Just TTodo
    "DOING" -> Just TDoing
    "DONE" -> Just TDone
    "WAIT" -> Just TWait
    "WAITING" -> Just TWait
    "CANCELED" -> Just TCanceled
    "CANCELLED" -> Just TCanceled
    "NOW" -> Just TNow
    "NEXT" -> Just TNow
    "LATER" -> Just TLater
    _ -> Nothing


taskPToString :: TaskP -> String
taskPToString = case _ of
    TTodo -> "TODO"
    TDoing -> "DOING"
    TDone -> "DONE"
    TWait -> "WAIT"
    TCanceled -> "CANCELED"
    TNow -> "NOW"
    TLater -> "LATER"


isDone :: TaskP -> Boolean
isDone = case _ of
    TDone -> true
    _ -> false


isDoing :: TaskP -> Boolean
isDoing = case _ of
    TDoing -> true
    _ -> false


isCanceled :: TaskP -> Boolean
isCanceled = case _ of
    TCanceled -> true
    _ -> false