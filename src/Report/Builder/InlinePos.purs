module Report.Builder.InlinePos where

import Prelude

import Data.Maybe (Maybe(..))
-- import Data.Newtype

import Report.Decorator as Decorator
import Report.Decorator (Key(..)) as D
import Report.Decorators.Progress as P


data PosKey -- merge with `Decorator.Key`
    = PKRating
    | PKPriority
    | PKTask
    | PKItemName
    | PKProgress (Maybe P.PValueTag)
    | PKEarnedAt
    | PKDescription
    | PKReference
    | PKTags


derive instance Eq PosKey
instance Ord PosKey where
    compare pkA pkB = orderOf pkA `compare` orderOf pkB


{-
data PosLocatedKey
    = PLKRating
    | PLKPriority
    | PLKTask
    | PLKItemName
    | PLKProgress P.PValueTag
    | PLKEarnedAt
    | PLKDescription
    | PLKReference
    | PLKTags


derive instance Eq PosLocatedKey
-}


toPosKey :: Decorator.Key -> PosKey
toPosKey = case _ of
    D.KRating -> PKRating
    D.KPriority -> PKPriority
    D.KTask -> PKTask
    D.KProgress vTag -> PKProgress $ Just vTag -- handle tasks inside progress?
    D.KEarnedAt -> PKEarnedAt
    D.KDescription -> PKDescription
    D.KReference -> PKReference


nextPos :: PosKey -> Maybe PosKey
nextPos = case _ of
    PKRating -> Just PKPriority
    PKPriority -> Just PKTask
    PKTask -> Just PKItemName
    PKItemName -> Just $ PKProgress Nothing
    PKProgress _ -> Just PKEarnedAt
    PKEarnedAt -> Just PKDescription
    PKDescription -> Just PKReference
    PKReference -> Just PKTags
    PKTags -> Nothing


prevPos :: PosKey -> Maybe PosKey
prevPos = case _ of
    PKRating -> Nothing
    PKPriority -> Just PKRating
    PKTask -> Just PKPriority
    PKItemName -> Just PKTask
    PKProgress _ -> Just PKItemName
    PKEarnedAt -> Just $ PKProgress Nothing
    PKDescription -> Just PKEarnedAt
    PKReference -> Just PKDescription
    PKTags -> Just PKReference


orderOf :: PosKey -> Int
orderOf = case _ of
    PKRating -> -3
    PKPriority -> -2
    PKTask -> -1
    PKItemName -> 0
    PKProgress _ -> 1 -- FIXME: order could depend on value
    PKEarnedAt -> 2
    PKDescription -> 3
    PKReference -> 4
    PKTags -> 5


{-
orderOfLocated :: PosLocatedKey -> Int
orderOfLocated = case _ of
    PLKRating -> -3
    PLKPriority -> -2
    PLKTask -> -1
    PLKItemName -> 0
    PLKProgress -> 1
    PLKEarnedAt -> 2
    PLKDescription -> 3
    PLKReference -> 4
    PLKTags -> 5
-}