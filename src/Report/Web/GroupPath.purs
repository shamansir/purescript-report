module Report.Web.GroupPath where

import Prelude

import Data.Array as Array
import Data.String as String

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.GroupPath as S
import Report.Web.Helpers (H, qpathsplitSpan, qspacerSpan)
import Report.Web.CSS as CSS


renderPath :: forall w i. S.GroupPath -> H w i
renderPath groupPath =
    HH.span
        [ HP.style CSS.groupPathWrapper ]
        $ Array.intersperse qpathsplitSpan $ renderPathSegment <$> S.pathToArray groupPath

renderPathSegment :: forall w i. String -> H w i
renderPathSegment segment =
    HH.span
        [ HP.style CSS.pathSegment ]
        [ HH.text segment ]

renderGroupRef :: forall w i. S.GroupPath -> H w i
renderGroupRef groupRef =
    HH.span_
        [ qspacerSpan
        , HH.a [ HP.href $ "#" <> groupPathId groupRef, HP.style CSS.groupRefLink ] [ HH.text "#" ]
        ]

groupPathId :: S.GroupPath -> String
groupPathId = String.joinWith "--" <<< S.pathToArray

