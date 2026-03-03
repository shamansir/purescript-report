module Report.Web.Component.RecalcBehavior where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Modify as Modify


type RecalcBehavior =
    { onEmpty   :: Maybe Modify.RecalculateConfig
    , onEdit    :: Maybe Modify.RecalculateConfig
    , onRegroup :: Maybe Modify.RecalculateConfig
    , onFilter  :: Maybe Modify.RecalculateConfig
    }


onAnyAction :: Modify.RecalculateConfig -> RecalcBehavior
onAnyAction conf = { onEmpty: Just conf, onEdit: Just conf, onRegroup: Just conf, onFilter: Just conf }


noUpdate :: RecalcBehavior
noUpdate = { onEmpty: Nothing, onEdit: Nothing, onRegroup: Nothing, onFilter: Nothing }


onEmpty :: Modify.RecalculateConfig -> RecalcBehavior -> RecalcBehavior
onEmpty conf = _ { onEmpty = Just conf }


onEdit :: Modify.RecalculateConfig -> RecalcBehavior -> RecalcBehavior
onEdit conf = _ { onEdit = Just conf }


onRegroup :: Modify.RecalculateConfig -> RecalcBehavior -> RecalcBehavior
onRegroup conf = _ { onRegroup = Just conf }


onFilter :: Modify.RecalculateConfig -> RecalcBehavior -> RecalcBehavior
onFilter conf = _ { onFilter = Just conf }