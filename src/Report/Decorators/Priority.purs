module Report.Decorators.Priority where

import Prelude

import Foreign (fail, ForeignError(..))

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Int (fromString, toNumber) as Int

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)


newtype Priority = Priority (Either String Int)
instance ReadForeign Priority where
    readImpl f = do
        (str :: String) <- readImpl f
        case fromString str :: Maybe Priority of
            Just priority  -> pure priority
            Nothing -> fail $ ForeignError $ "Invalid Priority string: " <> str
instance WriteForeign Priority where
    writeImpl = toString >>> writeImpl


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
toString (Priority (Left str)) = str
toString (Priority (Right n)) = show n


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
        "A" -> 1.0 / Int.toNumber maxLPriority
        "B" -> 2.0 / Int.toNumber maxLPriority
        "C" -> 3.0 / Int.toNumber maxLPriority
        _   -> 1.1
toValue (Priority (Right n)) = Int.toNumber n / Int.toNumber maxRPriority