module Report.Web.GroupPath where

import Prelude

import Data.Array as Array
import Data.String as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Report.GroupPath as S
import Report.Web.Helpers


renderPath groupPath =
    HH.span
        [ HP.style "font-size: 0.8em;" ]
        $ Array.intersperse qpathsplitSpan $ renderPathSegment <$> S.pathToArray groupPath

renderPathSegment segment =
    HH.span
        [ HP.style "border: 1px solid #ddd; border-radius: 3px; padding: 2px 3px; color: coral;" ]
        [ HH.text segment ]

renderGroupRef groupRef =
    HH.span []
        [ qspacerSpan
        , HH.a [ HP.href $ "#" <> groupPathId groupRef, HP.style "color: cadetblue; text-decoration: none;" ] [ HH.text "#" ]
        ]


groupPathId = String.joinWith "--" <<< S.pathToArray

