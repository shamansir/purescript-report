module Report.Decorators.Priority where

import Prelude

import Foreign (fail, ForeignError(..))

import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Data.Int (fromString, toNumber) as Int

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode (JsonDecodeError(..)) as AD
import Data.Argonaut.Encode (class EncodeJson, encodeJson)


newtype Priority = Priority (Either String Int)
instance ReadForeign Priority where
    readImpl f = do
        (str :: String) <- readImpl f
        case fromString str :: Maybe Priority of
            Just priority  -> pure priority
            Nothing -> fail $ ForeignError $ "Invalid Priority string: " <> str
instance WriteForeign Priority where
    writeImpl = toString >>> writeImpl

instance DecodeJson Priority where
    decodeJson f = do
        (str :: String) <- decodeJson f
        case fromString str :: Maybe Priority of
            Just priority  -> pure priority
            Nothing -> Left $ AD.TypeMismatch $ "Invalid Priority string: " <> str
instance EncodeJson Priority where
    encodeJson = toString >>> encodeJson


derive newtype instance Eq Priority


priorityChar :: Priority -> String
priorityChar (Priority (Left str)) = str
priorityChar (Priority (Right n)) = show n


priorityA = Priority (Left "A") :: Priority
priorityB = Priority (Left "B") :: Priority
priorityC = Priority (Left "C") :: Priority


priorityNum :: Int -> Priority
priorityNum = Priority <<< Right


maxLPriority = 3 :: Int
maxRPriority = 20 :: Int


toString :: Priority -> String
toString = priorityChar


fromString :: String -> Maybe Priority
fromString str =
    case str of
        "A" -> Just priorityA
        "B" -> Just priorityB
        "C" -> Just priorityC
        _   ->
            case Int.fromString str :: Maybe Int of
                Just n  -> Just $ priorityNum n
                Nothing -> Nothing


toValue :: Priority -> Number
toValue (Priority (Left str)) =
    case str of
        "A" -> 3.0 / Int.toNumber maxLPriority
        "B" -> 2.0 / Int.toNumber maxLPriority
        "C" -> 1.0 / Int.toNumber maxLPriority
        _   -> 0.5 / Int.toNumber maxLPriority
        -- FIXME: use Ord of the char?
toValue (Priority (Right n)) =
    Int.toNumber n / Int.toNumber maxRPriority