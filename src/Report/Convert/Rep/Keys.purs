module Report.Convert.Rep.Keys where

import Prelude

import Report.AtomicValue as AV
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
   D.KJoinReference ->  TM "JRF"


tmfp :: P.PTValueTag -> TriMarker
tmfp = triMarkerForProgress


triMarkerForProgress :: P.PTValueTag -> TriMarker
triMarkerForProgress = TM <<< case _ of
    P.PTAV AV.AVTNone -> "NON"
    P.PTAV AV.AVTUnknown -> "UNK"
    P.PTAV AV.AVTPInt -> "INT"
    P.PTAV AV.AVTPNumber -> "NUM"
    P.PTAV AV.AVTPText -> "TXT"
    P.PTAV AV.AVTPBoolean -> "BOL"
    P.PTAV AV.AVTPYear -> "YER"
    P.PTAV AV.AVTPID -> "UID"
    P.PTAV AV.AVTToComplete -> "CMP"
    P.PTAV AV.AVTPercentI -> "PCI"
    P.PTAV AV.AVTPercentN -> "PCN"
    P.PTAV AV.AVTPercentSign -> "PCX"
    P.PTAV AV.AVTToGetI -> "GTI"
    P.PTAV AV.AVTToGetN -> "GTN"
    P.PTAV AV.AVTToGetIX -> "GIX"
    P.PTAV AV.AVTToGetNX -> "GNX"
    P.PTAV AV.AVTToGetMI -> "GIM"
    P.PTAV AV.AVTToGetMN -> "GNM"
    P.PTAV AV.AVTToGetMIX -> "GIF"
    P.PTAV AV.AVTToGetMNX -> "GNF"
    P.PTAV AV.AVTOnTime -> "TIM"
    P.PTAV AV.AVTOnDate -> "DAT"
    P.PTAV AV.AVTOnDateTime -> "DTM"
    P.PTAV AV.AVTPerI -> "PPI"
    P.PTAV AV.AVTPerN -> "PPN"
    P.PTAV AV.AVTMeasuredI -> "MSI"
    P.PTAV AV.AVTMeasuredN -> "MSN"
    P.PTAV AV.AVTMeasuredSign -> "MSX"
    P.PTAV AV.AVTRangeI -> "RGI"
    P.PTAV AV.AVTRangeN -> "RGN"
    P.PTAV AV.AVTDateRange -> "DRG"
    P.PTAV AV.AVTTimeRange -> "TRG"
    P.PTAV AV.AVTDateTimeRange -> "DTR"
    P.PTAV AV.AVTPTask -> "PRG" -- not to intersect with `TSK`
    P.PTAV AV.AVTRelTime -> "REL"
    P.PTAV AV.AVTRelInt -> "REI"
    P.PTAV AV.AVTRelNum -> "REN"
    P.PTAV AV.AVTPPath -> "PTH"
    P.PTAV AV.AVTTags -> "TAG"
    P.PTAV AV.AVTPRawTags -> "RTG"
    P.PTAV AV.AVTColor -> "CLR"
    P.PTAV AV.AVTPRating -> "RTN" -- not to intersect with `RAT`
    P.PTAV AV.AVTPPriority -> "PRT" -- not to intersect with `PRI`
    P.PTAV AV.AVTError -> "XXX"

    P.PTLevelsI -> "LVI"
    P.PTLevelsN -> "LVN"
    P.PTLevelsO -> "LVO"
    P.PTLevelsS -> "LVS"
    P.PTLevelsE -> "LVE"
    P.PTLevelsP -> "LVP"
    P.PTLevelsC -> "LVC"



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
    "JRF" -> D.KJoinReference
    _     -> D.KProgress $ P._vtagTo $ ptftm $ TM marker


progressTagFromTriMarker :: TriMarker -> P.PTValueTag
progressTagFromTriMarker (TM marker) = case marker of
    "NON" -> P.PTAV AV.AVTNone
    "UNK" -> P.PTAV AV.AVTUnknown
    "INT" -> P.PTAV AV.AVTPInt
    "NUM" -> P.PTAV AV.AVTPNumber
    "TXT" -> P.PTAV AV.AVTPText
    "BOL" -> P.PTAV AV.AVTPBoolean
    "YER" -> P.PTAV AV.AVTPYear
    "UID" -> P.PTAV AV.AVTPID
    "CMP" -> P.PTAV AV.AVTToComplete
    "PCI" -> P.PTAV AV.AVTPercentI
    "PCN" -> P.PTAV AV.AVTPercentN
    "PCX" -> P.PTAV AV.AVTPercentSign
    "GTI" -> P.PTAV AV.AVTToGetI
    "GTN" -> P.PTAV AV.AVTToGetN
    "TIM" -> P.PTAV AV.AVTOnTime
    "DAT" -> P.PTAV AV.AVTOnDate
    "DTM" -> P.PTAV AV.AVTOnDateTime
    "PPI" -> P.PTAV AV.AVTPerI
    "PPN" -> P.PTAV AV.AVTPerN
    "MSI" -> P.PTAV AV.AVTMeasuredI
    "MSN" -> P.PTAV AV.AVTMeasuredN
    "MSX" -> P.PTAV AV.AVTMeasuredSign
    "RGI" -> P.PTAV AV.AVTRangeI
    "RGN" -> P.PTAV AV.AVTRangeN
    "PRG" -> P.PTAV AV.AVTPTask -- not to intersect with `TSK`
    "LVI" -> P.PTLevelsI
    "LVN" -> P.PTLevelsN
    "LVO" -> P.PTLevelsO
    "LVS" -> P.PTLevelsS
    "LVE" -> P.PTLevelsE
    "LVP" -> P.PTLevelsP
    "LVC" -> P.PTLevelsC
    "REL" -> P.PTAV AV.AVTRelTime
    "XXX" -> P.PTAV AV.AVTError
    "DRG" -> P.PTAV AV.AVTDateRange
    "TRG" -> P.PTAV AV.AVTTimeRange
    "DTR" -> P.PTAV AV.AVTDateTimeRange
    "REI" -> P.PTAV AV.AVTRelInt
    "REN" -> P.PTAV AV.AVTRelNum
    "PTH" -> P.PTAV AV.AVTPPath
    "TAG" -> P.PTAV AV.AVTTags
    "RTG" -> P.PTAV AV.AVTPRawTags
    "CLR" -> P.PTAV AV.AVTColor
    "RTN" -> P.PTAV AV.AVTPRating
    "PRT" -> P.PTAV AV.AVTPPriority

    _     -> P.PTAV AV.AVTUnknown


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