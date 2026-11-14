module Report.Convert.Rep.Keys
    ( module Report.Convert.Rep.Keys
    , module Exports
    ) where

import Prelude

import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe)

import Report.AtomicValue as AV
import Report.AtomicValue.Marker.Rep (TriMarker(..)) as Exports
import Report.AtomicValue.Marker.Rep (TriMarker(..), toTriMarker, progressTagByMarker, decoratorKeyByMarker)
import Report.Decorators.Tabular.Marker.Rep (toTabularTriMarker, tabularTypeByMarker, toTabularValueTriMarker, tabularValueTagByMarker)
import Report.Decorator (Key(..)) as D
import Report.Decorators.Progress (PTValueTag(..), _vtagTo) as P
import Report.Decorators.Tabular.TabularValue (TabValTypeKey(..), TabularValueTag(..)) as TV


newtype SymMarker = SM String -- marker from a symbol or two symbols

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
contKW = SM ">>>" :: SymMarker -- the next line of a multi-line value
compositeKW = SM "+++" :: SymMarker -- the counts of the parts of a composite tabular value


{- The markers themselves are defined (and checked to be unique at compile time) in `Report.AtomicValue.Marker.Rep` -}


tmf :: D.Key -> TriMarker
tmf = triMarkerForDecorator


triMarkerForDecorator :: D.Key -> TriMarker
triMarkerForDecorator = toTriMarker


tmfp :: P.PTValueTag -> TriMarker
tmfp = triMarkerForProgress


triMarkerForProgress :: P.PTValueTag -> TriMarker
triMarkerForProgress = toTriMarker


ptftm :: TriMarker -> P.PTValueTag
ptftm = progressTagFromTriMarker


decoratorKeyFromTriMarker :: TriMarker -> D.Key
decoratorKeyFromTriMarker marker =
    Map.lookup marker decoratorKeyByMarker
        # fromMaybe (D.KProgress $ P._vtagTo $ P.PTAV AV.AVTUnknown)


progressTagFromTriMarker :: TriMarker -> P.PTValueTag
progressTagFromTriMarker marker =
    Map.lookup marker progressTagByMarker
        # fromMaybe (P.PTAV AV.AVTUnknown)


tmft :: TV.TabValTypeKey -> TriMarker
tmft = triMarkerForTabular


triMarkerForTabular :: TV.TabValTypeKey -> TriMarker
triMarkerForTabular = toTabularTriMarker


tftm :: TriMarker -> TV.TabValTypeKey
tftm = tabularFromTriMarker


tabularFromTriMarker :: TriMarker -> TV.TabValTypeKey
tabularFromTriMarker marker =
    Map.lookup marker tabularTypeByMarker
        # fromMaybe (TV.TVTDecorator $ decoratorKeyFromTriMarker marker)


tmftv :: TV.TabularValueTag -> TriMarker
tmftv = toTabularValueTriMarker


tvtftm :: TriMarker -> TV.TabularValueTag
tvtftm marker =
    Map.lookup marker tabularValueTagByMarker
        # fromMaybe (TV.TVTAtomic $ TV.TVTDecorator $ decoratorKeyFromTriMarker marker)
