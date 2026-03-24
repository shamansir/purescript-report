module Report.Web.Decorators.EditInput where

import Prelude

import Data.Maybe (Maybe(..), maybe)

import Report.Core as CT
import Report.Core.Logic as CT
import Report.Web.Helpers (H)
import Report.Web.Decorators.Types (EditableValueEvents)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.KeyboardEvent as KE


mkValueEditInput :: forall a r w i. EditableValueEvents i r -> (CT.EncodedValue -> i) -> CT.ViewOrEdit a -> H w i
mkValueEditInput events onEdit =
        maybe (HH.text "<EDIT>")
            (\(CT.EncodedValue encVal) ->
                    HH.input
                    [ HP.type_ HP.InputText
                    , HP.value encVal
                    , HE.onClick events.onStartEditing
                    , HE.onValueChange (CT.EncodedValue >>> onEdit)
                    , HE.onKeyUp (KE.code >>> -- Debug.spy "key up" >>>
                        \code -> if code == "Escape" || code == "Enter"
                            then events.onCancelEditing
                            else events.noop)
                    , HE.onBlur $ const events.onCancelEditing
                    , HE.onAbort $ const events.onCancelEditing
                    ]
            )
            <<< CT.loadEncoded

