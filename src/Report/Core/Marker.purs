module Report.Core.Marker
    ( MarkerFormat
    , class Marker
    , class ValidMarker
    , class IsMarkerFormat, reflectFormat
    , class MarkerArgument, argToMarker, argAllMarkers
    , class GenericMarkers, gToMarker, gAllMarkers
    , toMarker
    , allMarkers
    , markersMap
    ) where

{- Markers are the short textual codes which every kind of export uses to write the type of the value
   (i.e. `TXT` for text in `.rep`, or `TEXTVAL` property in `.org`).

   Every export format is a type of `MarkerFormat` kind with its own module (i.e. `Report.AtomicValue.Marker.Rep`)
   containing the instances of `Marker` for that format. `Marker` has functional dependencies in both directions,
   so the compiler rejects (with `OverlappingInstances`) the same marker used for two different constructors
   or two different markers for the same constructor, in the scope of one format.

   Markers for the constructors are collected using `Generic`, so a constructor which has no marker
   is a compile error too (`No type class instance was found for Marker ...`).

   No checks remain in the compiled code: `Marker` and `ValidMarker` have no members. -}

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))

import Report.Core (ReportFormat)




{- Formats -}


data MarkerFormat


-- | The format of the export which uses the markers.
class IsMarkerFormat :: MarkerFormat -> Constraint
class IsMarkerFormat fmt where
    reflectFormat :: Proxy fmt -> ReportFormat


-- | Constructor name `ctor` (i.e. `"AVTPText"`) has the `marker` (i.e. `"TXT"`) in the format `fmt`,
-- | both are unique in the scope of `fmt`.
class Marker :: MarkerFormat -> Symbol -> Symbol -> Constraint
class ValidMarker fmt marker <= Marker fmt ctor marker | fmt ctor -> marker, fmt marker -> ctor


-- | Additional restrictions to the markers of the format, i.e. their length.
class ValidMarker :: MarkerFormat -> Symbol -> Constraint
class ValidMarker fmt marker


-- | Markers for constructors with an argument are taken from the argument, i.e. `PTAV AtomicValueTag`.
class MarkerArgument :: MarkerFormat -> Type -> Constraint
class MarkerArgument fmt a where
    argToMarker :: Proxy fmt -> a -> String
    argAllMarkers :: Proxy fmt -> Array (String /\ a)


{- Generic -}


class GenericMarkers :: MarkerFormat -> Type -> Constraint
class GenericMarkers fmt rep where
    gToMarker :: Proxy fmt -> rep -> String
    gAllMarkers :: Proxy fmt -> Array (String /\ rep)


instance (Marker fmt ctor marker, IsSymbol marker) => GenericMarkers fmt (Constructor ctor NoArguments) where
    gToMarker _ _ = reflectSymbol (Proxy :: _ marker)
    gAllMarkers _ = [ reflectSymbol (Proxy :: _ marker) /\ Constructor NoArguments ]


instance MarkerArgument fmt a => GenericMarkers fmt (Constructor ctor (Argument a)) where
    gToMarker pfmt (Constructor (Argument a)) = argToMarker pfmt a
    gAllMarkers pfmt = map (Constructor <<< Argument) <$> argAllMarkers pfmt


instance (GenericMarkers fmt a, GenericMarkers fmt b) => GenericMarkers fmt (Sum a b) where
    gToMarker pfmt (Inl a) = gToMarker pfmt a
    gToMarker pfmt (Inr b) = gToMarker pfmt b
    gAllMarkers pfmt = (map Inl <$> gAllMarkers pfmt) <> (map Inr <$> gAllMarkers pfmt)


toMarker :: forall @fmt a rep. Generic a rep => GenericMarkers fmt rep => a -> String
toMarker = from >>> gToMarker (Proxy :: _ fmt)


-- | All the values of `a` with their markers, in the order of constructors.
allMarkers :: forall @fmt a rep. Generic a rep => GenericMarkers fmt rep => Array (String /\ a)
allMarkers = map to <$> gAllMarkers (Proxy :: _ fmt)


-- | To be stored as a constant in the module of the format, so that it's built only once.
markersMap :: forall @fmt a rep. Generic a rep => GenericMarkers fmt rep => Map String a
markersMap = Map.fromFoldable $ allMarkers @fmt
