module Report.Chain where

import Prelude

import Foreign (F, ForeignError(..), fail)

-- import Control.Applicative (class Pure)
import Data.Array ((:))
import Data.Array (uncons, snoc) as Array
import Data.List (List(..))
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (joinWith, split, Pattern(..)) as String
import Data.Tuple.Nested (type (/\), (/\))

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)

{- Chain -}


data Chain a
    = End a
    | More a (Chain a)


derive instance Functor Chain
instance FunctorWithIndex Int Chain where
    mapWithIndex :: forall a b. (Int -> a -> b) -> Chain a -> Chain b
    mapWithIndex mapFn = foldF 0
        where
            foldF idx = case _ of
                End a -> End $ mapFn idx a
                More a rest -> More (mapFn idx a) $ foldF (idx + 1) rest


instance Eq a  => Eq  (Chain a) where eq ca cb      = toArray ca == toArray cb
instance Ord a => Ord (Chain a) where compare ca cb = compare (toArray ca) (toArray cb)


--| Get last link
last :: forall a. Chain a -> a
last =
    case _ of
        End a -> a
        More _ rest -> last rest


--| Get next link, no matter if it's an end or chain continues further
next :: forall a. Chain a -> a
next =
    case _ of
        End a -> a
        More a _ -> a


--| Get all elements but the first
tail :: forall a. Chain a -> Maybe (Chain a)
tail =
    case _ of
        End _ -> Nothing
        More _ rest -> Just rest

--| Get all items before the last and the last one separately.
break :: forall a. Chain a -> Array a /\ a
break = foldF []
    where
        foldF prev = case _ of
            End a -> prev /\ a
            More a rest -> foldF (Array.snoc prev a) rest


--| Call one function for when there are more links further and another function when it's an end of the chain
chain :: forall a b. (a -> Array a -> b) -> (a -> b) -> Chain a -> b
chain onMore onEnd = case _ of
    End a -> onEnd a
    More a rest -> chain (const $ const $ onMore a $ toArray rest) onEnd rest
    -- More a rest -> chain (\a' rest' -> onMore a $ a' : rest') onEnd rest


--| Get all the transformations of chain: `More 1 $ More 2 $ End 3 == [ More 1 $ More 2 $ End 3, More 1 $ End 2, End 1 ]`
-- subchains :: forall a. Chain a -> Array (Chain a)

--| Get all the transformations of chain: `More 1 $ More 2 $ End 3 == [ [ 1, 2, 3 ], [ 1, 2 ], [ 1 ] ]`
-- unwraps :: forall a. Chain a -> Array (Array a)
-- unwraps = foldF []
--     where
--         foldF prev = case _ of
--             End a -> ?wh
--             More a rest -> ?wh


singleton :: forall a. a -> Chain a
singleton = End


--| Create a chain with a single link.
end :: forall a. a -> Chain a
end = singleton


--| Create a chain with first link and continuation a.k.a. cons
more :: forall a. a -> Chain a -> Chain a
more = More


toArray :: Chain ~> Array
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


fromList :: forall a. List a -> Maybe (Chain a)
fromList = case _ of
    Nil -> Nothing
    Cons a Nil -> Just $ End a
    Cons a rest -> Just $ case fromList rest of
        Just restChain -> More a restChain
        Nothing -> End a


toList :: Chain ~> List
toList = case _ of
    End a -> Cons a Nil
    More a rest -> Cons a $ toList rest


instance WriteForeign (Chain String) where
    writeImpl = toArray >>> writeImpl


instance ReadForeign (Chain String) where
    readImpl frg = (readImpl frg :: F (Array String)) >>= \arr ->
        case fromArray arr of
            Just mbw -> pure mbw
            Nothing  -> fail $ ForeignError "Chain: cannot read from empty array"

