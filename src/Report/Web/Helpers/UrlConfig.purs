module Report.Web.Helpers.UrlConfig where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length, joinWith, split, drop, Pattern(..)) as String
import Data.Map (Map)
import Data.Map (toUnfoldable, fromFoldable, empty, insert, lookup) as Map
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))



type ParamMap = Map String String


class UrlConfig a where
    default :: a
    writeToUrl :: a -> ParamMap
    loadFromUrl :: a -> ParamMap -> a



toUrlPairs :: ParamMap -> String
toUrlPairs pairsMap =
    (Map.toUnfoldable pairsMap :: Array _)
        <#> Tuple.uncurry (\key val -> key <> "=" <> val)
         # String.joinWith "&"



fromUrlPairs :: String -> ParamMap
fromUrlPairs opts =
    let
      optPair = case _ of
          [] -> ("" /\ "")
          [n] -> (n /\ "")
          [n, v] -> (n /\ v)
          [n, v, _] -> (n /\ v)
          _ -> ("" /\ "")
    in Map.fromFoldable $
          if String.length opts > 0 then
            optPair <$> String.split (String.Pattern "=") <$> (String.split (String.Pattern "&") $ String.drop 1 opts)
          else
            []


emptyParams :: ParamMap
emptyParams = Map.empty


insert :: String -> String -> ParamMap -> ParamMap
insert = Map.insert


lookup :: String -> ParamMap -> Maybe String
lookup = Map.lookup


insertWhenJust :: String -> Maybe String -> ParamMap -> ParamMap
insertWhenJust = _insertWhenJust


insertIf :: Boolean -> String -> String -> ParamMap -> ParamMap
insertIf = _insertIf


-- insertWhenNonEmpty :: String -> Maybe String -> ParamMap -> ParamMap
-- insertWhenNonEmpty = _insertWhenJust


_insertWhenJust :: forall k v. Ord k => k -> Maybe v -> Map k v -> Map k v
_insertWhenJust k = case _ of
  Just v -> Map.insert k v
  Nothing -> identity


_insertIf :: forall k v. Ord k => Boolean -> k -> v -> Map k v -> Map k v
_insertIf flag k v = if flag then Map.insert k v else identity