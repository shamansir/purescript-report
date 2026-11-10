module Report.Decorators.Tabular.Marker.Rep where

{- Markers of the `.rep` format for the tabular values, they have their own namespace (i.e. `; DTR. ...` is a date range there).
   The values which have no special tabular marker are written as decorators and so use `Report.AtomicValue.Marker.Rep`. -}

import Prelude

import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map

import Report.Core (ReportFormat(..)) as CT
import Report.Core.Marker
    ( MarkerFormat, class Marker, class ValidMarker, class IsMarkerFormat, class MarkerArgument
    , toMarker, allMarkers
    )
import Report.AtomicValue.Marker.Rep (TriMarker(..), RepLine, class ShortMarker)
import Report.Decorator (Key) as D
import Report.Decorators.Tabular.TabularValue (TabValTypeKey) as TV


foreign import data RepTabular :: MarkerFormat


instance IsMarkerFormat RepTabular where
    reflectFormat _ = CT.Rep


instance ShortMarker marker => ValidMarker RepTabular marker


instance Marker RepTabular "TVTString" "TXT"
instance Marker RepTabular "TVTInt" "INT"
instance Marker RepTabular "TVTID" "UID"
instance Marker RepTabular "TVTYear" "YER"
instance Marker RepTabular "TVTNumber" "NUM"
instance Marker RepTabular "TVTBoolean" "BOL"
instance Marker RepTabular "TVTTime" "TIM"
instance Marker RepTabular "TVTDate" "DAT"
instance Marker RepTabular "TVTDateRange" "DTR"
instance Marker RepTabular "TVTDateTime" "DTT"
instance Marker RepTabular "TVTTimeRange" "TMR"
instance Marker RepTabular "TVTDateTimeRange" "DMR"
instance Marker RepTabular "TVTTags" "TAG"


-- | `TVTDecorator` has the marker of the decorator, these are not included into `tabularTypeByMarker`
-- | and are resolved using `Report.AtomicValue.Marker.Rep.decoratorKeyByMarker` instead.
instance MarkerArgument RepTabular D.Key where
    argToMarker _ = toMarker @RepLine
    argAllMarkers _ = []


toTabularTriMarker :: TV.TabValTypeKey -> TriMarker
toTabularTriMarker = toMarker @RepTabular >>> TM


tabularTypeByMarker :: Map TriMarker TV.TabValTypeKey
tabularTypeByMarker = Map.fromFoldable $ lmap TM <$> allMarkers @RepTabular
