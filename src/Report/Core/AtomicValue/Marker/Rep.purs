module Report.AtomicValue.Marker.Rep where

{- Markers of the `.rep` format for the decorator lines: atomic values, levels and decorators share the namespace,
   since all of them can appear on the same line (i.e. `: TXT. some text`, `: RAT. 7.5`).
   Every marker is a `TriMarker`: one to three characters. -}

import Prelude

import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Generic.Rep (class Generic)
import Prim.Symbol (class Cons) as Symbol
import Prim.TypeError (class Fail, Text, Quote, Beside)

import Report.Core (ReportFormat(..)) as CT
import Report.AtomicValue.Tag (AtomicValueTag)
import Report.Core.Marker
    ( MarkerFormat
    , class Marker, class ValidMarker, class IsMarkerFormat, class MarkerArgument, class GenericMarkers
    , toMarker, allMarkers
    )
import Report.Decorator (Key) as D
import Report.Decorators.Progress (PValueTag, PTValueTag, _vtagFrom, _vtagTo) as P


newtype TriMarker = TM String -- marker from three letters

derive newtype instance Show TriMarker
derive newtype instance Eq TriMarker
derive newtype instance Ord TriMarker


foreign import data RepLine :: MarkerFormat


instance IsMarkerFormat RepLine where
    reflectFormat _ = CT.Rep


instance ShortMarker marker => ValidMarker RepLine marker


{- Atomic values -}


instance Marker RepLine "AVTNone" "NON"
instance Marker RepLine "AVTUnknown" "UNK"
instance Marker RepLine "AVTPInt" "INT"
instance Marker RepLine "AVTPNumber" "NUM"
instance Marker RepLine "AVTPText" "TXT"
instance Marker RepLine "AVTPBoolean" "BOL"
instance Marker RepLine "AVTPYear" "YER"
instance Marker RepLine "AVTPID" "UID"
instance Marker RepLine "AVTToComplete" "CMP"
instance Marker RepLine "AVTPercentI" "PCI"
instance Marker RepLine "AVTPercentN" "PCN"
instance Marker RepLine "AVTPercentSign" "PCX"
instance Marker RepLine "AVTToGetI" "GTI"
instance Marker RepLine "AVTToGetN" "GTN"
instance Marker RepLine "AVTToGetIX" "GIX"
instance Marker RepLine "AVTToGetNX" "GNX"
instance Marker RepLine "AVTToGetMI" "GIM"
instance Marker RepLine "AVTToGetMN" "GNM"
instance Marker RepLine "AVTToGetMIX" "GIF"
instance Marker RepLine "AVTToGetMNX" "GNF"
instance Marker RepLine "AVTOnTime" "TIM"
instance Marker RepLine "AVTOnDate" "DAT"
instance Marker RepLine "AVTOnDateTime" "DTM"
instance Marker RepLine "AVTPerI" "PPI"
instance Marker RepLine "AVTPerN" "PPN"
instance Marker RepLine "AVTMeasuredI" "MSI"
instance Marker RepLine "AVTMeasuredN" "MSN"
instance Marker RepLine "AVTMeasuredSign" "MSX"
instance Marker RepLine "AVTRangeI" "RGI"
instance Marker RepLine "AVTRangeN" "RGN"
instance Marker RepLine "AVTDateRange" "DRG"
instance Marker RepLine "AVTTimeRange" "TRG"
instance Marker RepLine "AVTDateTimeRange" "DTR"
instance Marker RepLine "AVTPTask" "PRG" -- not to intersect with `TSK`
instance Marker RepLine "AVTRelTime" "REL"
instance Marker RepLine "AVTRelInt" "REI"
instance Marker RepLine "AVTRelNum" "REN"
instance Marker RepLine "AVTPPath" "PTH"
instance Marker RepLine "AVTTags" "TAG"
instance Marker RepLine "AVTPRawTags" "RTG"
instance Marker RepLine "AVTColor" "CLR"
instance Marker RepLine "AVTPRating" "RTN" -- not to intersect with `RAT`
instance Marker RepLine "AVTPPriority" "PRT" -- not to intersect with `PRI`
instance Marker RepLine "AVTError" "XXX"


{- Levels -}


instance Marker RepLine "PTLevelsI" "LVI"
instance Marker RepLine "PTLevelsN" "LVN"
instance Marker RepLine "PTLevelsO" "LVO"
instance Marker RepLine "PTLevelsS" "LVS"
instance Marker RepLine "PTLevelsE" "LVE"
instance Marker RepLine "PTLevelsP" "LVP"
instance Marker RepLine "PTLevelsC" "LVC"


{- Decorators -}


instance Marker RepLine "KRating" "RAT"
instance Marker RepLine "KPriority" "PRI"
instance Marker RepLine "KTask" "TSK"
instance Marker RepLine "KEarnedAt" "ERN"
instance Marker RepLine "KDescription" "DSC"
instance Marker RepLine "KReference" "REF"
instance Marker RepLine "KJoinReference" "JRF"


-- | `KProgress` decorators have the markers of their values
instance MarkerArgument RepLine P.PValueTag where
    argToMarker _ = P._vtagFrom >>> toMarker @RepLine
    argAllMarkers _ = map P._vtagTo <$> allMarkers @RepLine


{- Conversion -}


toTriMarker :: forall a rep. Generic a rep => GenericMarkers RepLine rep => a -> TriMarker
toTriMarker = toMarker @RepLine >>> TM


triMarkersMap :: forall a rep. Generic a rep => GenericMarkers RepLine rep => Map TriMarker a
triMarkersMap = Map.fromFoldable $ lmap TM <$> allMarkers @RepLine


atomicTagByMarker :: Map TriMarker AtomicValueTag
atomicTagByMarker = triMarkersMap


progressTagByMarker :: Map TriMarker P.PTValueTag
progressTagByMarker = triMarkersMap


decoratorKeyByMarker :: Map TriMarker D.Key
decoratorKeyByMarker = triMarkersMap


{- Length check -}


-- | A `.rep` marker has from one to three characters
class ShortMarker :: Symbol -> Constraint
class ShortMarker marker

instance Fail (Text "`.rep` marker should not be empty") => ShortMarker ""
else instance (Symbol.Cons h t marker, ShortMarker2 marker t) => ShortMarker marker


class ShortMarker2 :: Symbol -> Symbol -> Constraint
class ShortMarker2 marker rest

instance ShortMarker2 marker ""
else instance (Symbol.Cons h t rest, ShortMarker1 marker t) => ShortMarker2 marker rest


class ShortMarker1 :: Symbol -> Symbol -> Constraint
class ShortMarker1 marker rest

instance ShortMarker1 marker ""
else instance (Symbol.Cons h t rest, ShortMarker0 marker t) => ShortMarker1 marker rest


class ShortMarker0 :: Symbol -> Symbol -> Constraint
class ShortMarker0 marker rest

instance ShortMarker0 marker ""
else instance Fail (Beside (Text "`.rep` marker is longer than three characters: ") (Quote marker)) => ShortMarker0 marker rest
