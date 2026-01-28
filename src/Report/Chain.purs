module Report.Chain where

import Prelude

import Foreign (F, ForeignError(..), fail)

-- import Control.Applicative (class Pure)
import Data.Array ((:))
import Data.Array (uncons) as Array
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith, split, Pattern(..)) as String

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)

{- Chain -}


data Chain a
    = End a
    | More a (Chain a)


derive instance Functor Chain


last :: forall a. Chain a -> a
last =
    case _ of
        End a -> a
        More _ rest -> last rest


next :: forall a. Chain a -> a
next =
    case _ of
        End a -> a
        More a _ -> a


tail :: forall a. Chain a -> Maybe (Chain a)
tail =
    case _ of
        End _ -> Nothing
        More _ rest -> Just rest


singleton :: forall a. a -> Chain a
singleton = End


end :: forall a. a -> Chain a
end = singleton


more :: forall a. a -> Chain a -> Chain a
more = More


toArray :: forall a. Chain a -> Array a
toArray mbw =
    case mbw of
        End a -> [a]
        More a rest -> a : toArray rest


fromArray :: forall a. Array a -> Maybe (Chain a)
fromArray arr =
    case Array.uncons arr of
        Nothing -> Nothing
        Just { head, tail } ->
            case tail of
                [] -> Just $ End head
                mbwRest -> Just $ maybe (End head) (More head) $ fromArray mbwRest


toString :: Chain String -> String
toString mbw = String.joinWith "<>" (toArray mbw)


fromString :: String -> Maybe (Chain String)
fromString str = fromArray (String.split (String.Pattern "<>") str)


instance WriteForeign (Chain String) where
    writeImpl = toArray >>> writeImpl


instance ReadForeign (Chain String) where
    readImpl frg = (readImpl frg :: F (Array String)) >>= \arr ->
        case fromArray arr of
            Just mbw -> pure mbw
            Nothing  -> fail $ ForeignError "Chain: cannot read from empty array"

