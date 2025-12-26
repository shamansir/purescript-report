module Report.Web.Helpers where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Core as CT

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type H w i = HH.HTML w i
type Color = String


pctWidth = 250.0 :: Number
levelsMargin = 30.0 :: Number
lineHeight = 1.9 :: Number
nestMargin = 30.0 :: Number


completeColor = "#333" :: Color
genericColor = "silvergray" :: Color
incompleteColor = "#999" :: Color
progressBarCompleteColor = "forestgreen" :: Color
progressBarIncompleteColor = "silver" :: Color
groupProgressBarCompleteColor = "darkslategray" :: Color -- cadetblue, cornflowerblue, darkcyan, darkslategray
groupProgressBarIncompleteColor = "gainsboro" :: Color -- gainsboro
textColor = "dodgerblue" :: Color
numColor = "green" :: Color
measureColor = "darkgray" :: Color
timeColor = "darkolivegreen" :: Color
splitcolor = "lightgray" :: Color
errorColor = "red" :: Color


formatInt :: Int -> String
formatInt = CT.formatWithCommas
formatNum :: Number -> String
formatNum = CT.formatNumWithCommas { fixedTo : Just 2 }

qcolor :: Color -> _
qcolor color = HP.style $ "color: " <> color <> ";"

qspacerSpan = HH.span [] [ HH.text " " ] :: forall w i. H w i
qthinspacerSpan = HH.span [] [ HH.text " " ] :: forall w i. H w i
qtimesplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "::" ] :: forall w i. H w i
qpathsplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "::" ] :: forall w i. H w i
qpersplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "/" ] :: forall w i. H w i
qrangesplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "-" ] :: forall w i. H w i

qitemmarkerSpan :: forall w i. String -> H w i
qitemmarkerSpan color = HH.span [ qcolor color ] [ HH.text "🞄" {- "●" -}  ]
qlevelmarkerSpan :: forall w i. String -> H w i
qlevelmarkerSpan color = HH.span [ qcolor color ] [ HH.text "▪" ]
qcolorSpan :: forall w i. String -> String -> H w i
qcolorSpan color text =  HH.span [ qcolor color ] [ HH.text text ]
qcompleteCheckbox :: forall w i. Boolean -> H w i
qcompleteCheckbox =
    qcheckbox (\v -> if v then progressBarCompleteColor else progressBarIncompleteColor) $ const ""
    -- [ HH.text $ if done then "" {-"●" -} else "" ]

qcheckbox :: forall w i a. (a -> String) -> (a -> String) -> a -> H w i
qcheckbox toColor toText a =
    HH.span
        [ HP.style $ "display: inline-block;"
                    <> "border: 1px solid darkgray;"
                    <> "border-radius: 3px;"
                    <> "width: 1em;"
                    <> "height: 1em;"
                    <> "color: white;"
                    <> "position: relative; top: 2px; left: -2px;"
                    <> "background-color: " <> toColor a <> ";"
        ]
        [ HH.text $ toText a ]



