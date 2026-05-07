module Report.Convert.Rep.Keys where

import Prelude

import Report.Decorator (Key(..)) as D
import Report.Decorators.Progress (PTValueTag(..), _vtagFrom, _vtagTo) as P
import Report.Decorators.Tabular.TabularValue (TabValTypeKey(..)) as TV


newtype TriMarker = TM String -- marker from three letters
newtype SymMarker = SM String -- marker from a symbol or two symbols

derive newtype instance Show TriMarker
derive newtype instance Eq TriMarker
derive newtype instance Show SymMarker
derive newtype instance Eq SymMarker


subjKW = TM "SBJ" :: TriMarker
groupKW = TM "GRP" :: TriMarker


tabKW = SM "-" :: SymMarker
decKW = SM ":" :: SymMarker
tavKW = SM ";" :: SymMarker
tagKW = SM "#" :: SymMarker
pathKW = SM "//" :: SymMarker
subjIDKW = SM "//" :: SymMarker



tmf :: D.Key -> TriMarker
tmf = triMarkerForDecorator


triMarkerForDecorator :: D.Key -> TriMarker
triMarkerForDecorator = case _ of
   D.KRating ->         TM "RAT"
   D.KPriority ->       TM "PRI"
   D.KTask ->           TM "TSK"
   D.KProgress pvTag -> triMarkerForProgress $ P._vtagFrom pvTag
   D.KEarnedAt ->       TM "ERN"
   D.KDescription ->    TM "DSC"
   D.KReference ->      TM "REF"


tmfp :: P.PTValueTag -> TriMarker
tmfp = triMarkerForProgress


triMarkerForProgress :: P.PTValueTag -> TriMarker
triMarkerForProgress = TM <<< case _ of
    P.PTNone -> "NON"
    P.PTUnknown -> "UNK"
    P.PTInt -> "INT"
    P.PTNumber -> "NUM"
    P.PTText -> "TXT"
    P.PTToComplete -> "CMP"
    P.PTPercentI -> "PCI"
    P.PTPercentN -> "PCN"
    P.PTPercentSign -> "PCX"
    P.PTToGetI -> "GTI"
    P.PTToGetN -> "GTN"
    P.PTOnTime -> "TIM"
    P.PTOnDate -> "DAT"
    P.PTPerI -> "PPI"
    P.PTPerN -> "PPN"
    P.PTMeasuredI -> "MSI"
    P.PTMeasuredN -> "MSN"
    P.PTMeasuredSign -> "MSX"
    P.PTRangeI -> "RGI"
    P.PTRangeN -> "RGN"
    P.PTTask -> "PRG" -- not to intersect with `TSK`
    P.PTLevelsI -> "LVI"
    P.PTLevelsN -> "LVN"
    P.PTLevelsO -> "LVO"
    P.PTLevelsS -> "LVS"
    P.PTLevelsE -> "LVE"
    P.PTLevelsP -> "LVP"
    P.PTLevelsC -> "LVC"
    P.PTRelTime -> "REL"
    P.PTError -> "XXX"


ptftm :: TriMarker -> P.PTValueTag
ptftm = progressTagFromTriMarker


decoratorKeyFromTriMarker :: TriMarker -> D.Key
decoratorKeyFromTriMarker (TM marker) = case marker of
    "RAT" -> D.KRating
    "PRI" -> D.KPriority
    "TSK" -> D.KTask
    "ERN" -> D.KEarnedAt
    "DSC" -> D.KDescription
    "REF" -> D.KReference
    _     -> D.KProgress $ P._vtagTo $ ptftm $ TM marker


progressTagFromTriMarker :: TriMarker -> P.PTValueTag
progressTagFromTriMarker (TM marker) = case marker of
    "NON" -> P.PTNone
    "UNK" -> P.PTUnknown
    "INT" -> P.PTInt
    "NUM" -> P.PTNumber
    "TXT" -> P.PTText
    "CMP" -> P.PTToComplete
    "PCI" -> P.PTPercentI
    "PCN" -> P.PTPercentN
    "PCX" -> P.PTPercentSign
    "GTI" -> P.PTToGetI
    "GTN" -> P.PTToGetN
    "TIM" -> P.PTOnTime
    "DAT" -> P.PTOnDate
    "PPI" -> P.PTPerI
    "PPN" -> P.PTPerN
    "MSI" -> P.PTMeasuredI
    "MSN" -> P.PTMeasuredN
    "MSX" -> P.PTMeasuredSign
    "RGI" -> P.PTRangeI
    "RGN" -> P.PTRangeN
    "PRG" -> P.PTTask -- not to intersect with `TSK`
    "LVI" -> P.PTLevelsI
    "LVN" -> P.PTLevelsN
    "LVO" -> P.PTLevelsO
    "LVS" -> P.PTLevelsS
    "LVE" -> P.PTLevelsE
    "LVP" -> P.PTLevelsP
    "LVC" -> P.PTLevelsC
    "REL" -> P.PTRelTime
    "XXX" -> P.PTError
    _     -> P.PTUnknown


tmft :: TV.TabValTypeKey -> TriMarker
tmft = triMarkerForTabular


triMarkerForTabular :: TV.TabValTypeKey -> TriMarker
triMarkerForTabular = case _ of
    TV.TVTString ->  TM "TXT"
    TV.TVTInt ->     TM "INT"
    TV.TVTID ->      TM "UID"
    TV.TVTYear ->    TM "YER"
    TV.TVTNumber ->  TM "NUM"
    TV.TVTBoolean -> TM "BOL"
    TV.TVTTime ->    TM "TIM"
    TV.TVTDate ->    TM "DAT"
    TV.TVTDateRange ->     TM "DTR"
    TV.TVTDateTime ->      TM "DTT"
    TV.TVTTimeRange ->     TM "TMR"
    TV.TVTDateTimeRange -> TM "DMR"
    TV.TVTTags ->    TM "TAG"
    TV.TVTDecorator decKey -> triMarkerForDecorator decKey


tftm :: TriMarker -> TV.TabValTypeKey
tftm = tabularFromTriMarker


tabularFromTriMarker :: TriMarker -> TV.TabValTypeKey
tabularFromTriMarker (TM marker) = case marker of
    "TXT" -> TV.TVTString
    "INT" -> TV.TVTInt
    "UID" -> TV.TVTID
    "YER" -> TV.TVTYear
    "NUM" -> TV.TVTNumber
    "BOL" -> TV.TVTBoolean
    "TIM" -> TV.TVTTime
    "DAT" -> TV.TVTDate
    "DTR" -> TV.TVTDateRange
    "DTT" -> TV.TVTDateTime
    "TMR" -> TV.TVTTimeRange
    "DMR" -> TV.TVTDateTimeRange
    "TAG" -> TV.TVTTags
    _     -> TV.TVTDecorator $ decoratorKeyFromTriMarker $ TM marker