module Report.Convert.Rep.Keys where

import Prelude

import Report.Decorator (Key(..)) as D
import Report.Decorators.Progress (PTValueTag(..), _vtagFrom, _vtagTo) as P


newtype TriMarker = TM String -- marker from three letters
newtype SymMarker = SM String -- marker from a symbol or two symbols


subjKW = TM "SBJ" :: TriMarker
groupKW = TM "GRP" :: TriMarker


tabKW = SM "-" :: SymMarker
decKW = SM ":" :: SymMarker
tavKW = SM ";" :: SymMarker
tagKW = SM "#" :: SymMarker
pathKW = SM "//" :: SymMarker



tmf :: D.Key -> TriMarker
tmf = triMarkerFor


triMarkerFor :: D.Key -> TriMarker
triMarkerFor = case _ of
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


mbDecKeyFromTriMarker :: TriMarker -> D.Key
mbDecKeyFromTriMarker (TM marker) = case marker of
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