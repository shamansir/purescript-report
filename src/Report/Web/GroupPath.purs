module Report.Web.GroupPath where

import Prelude

import Data.Array as Array
import Data.String as String

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.GroupPath as S
import Report.Web.Helpers (H, qpathsplitSpan, qspacerSpan)


renderPath :: forall w i. S.GroupPath -> H w i
renderPath groupPath =
    HH.span
        [ HP.style "font-size: 0.8em;" ]
        $ Array.intersperse qpathsplitSpan $ renderPathSegment <$> S.pathToArray groupPath

renderPathSegment :: forall w i. String -> H w i
renderPathSegment segment =
    HH.span
        [ HP.style "border: 1px solid #ddd; border-radius: 3px; padding: 2px 3px; color: coral;" ]
        [ HH.text segment ]

renderGroupRef :: forall w i. S.GroupPath -> H w i
renderGroupRef groupRef =
    HH.span []
        [ qspacerSpan
        , HH.a [ HP.href $ "#" <> groupPathId groupRef, HP.style "color: cadetblue; text-decoration: none;" ] [ HH.text "#" ]
        ]

groupPathId :: S.GroupPath -> String
groupPathId = String.joinWith "--" <<< S.pathToArray

