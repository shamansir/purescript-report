module Report.Convert.Dhall.Import where

import Prelude

import Foreign (F, Foreign)

import Control.Alt ((<|>))

import Data.Maybe (Maybe)

import Report.Convert.Types (SubjectId)
import Report.Decorators.Progress (DateRec)
import Report.Tabular (Tabular)
import Report.Decorators.Tabular.TabularValue (TabularValue)


import Yoga.JSON (class ReadForeign, readImpl)


type Ref = Array String


type DhallItemRec =
    { key :: Maybe String
    , title :: Maybe String
    , kind :: Maybe String
    , ref :: Ref
    , detailed :: Maybe String
    , selfRef :: Maybe Ref
    , date :: Maybe DateRec
    , tags :: Maybe (Array String)
    , value ::
        Maybe
            { t :: String
            , v :: Foreign
            }
    }


type DhallGroupRec =
    { title :: String
    , kind :: String
    , ref :: Ref
    }


data DhallProperty
    = HoldsGroup DhallGroupRec
    | HoldsItem DhallItemRec


instance ReadForeign DhallProperty where
    readImpl frgn
        =   ((readImpl frgn :: F DhallGroupRec) <#> HoldsGroup)
        <|> ((readImpl frgn :: F DhallItemRec ) <#> HoldsItem)


type DhallSubjectRec =
    { name :: String
    , id :: SubjectId
    , properties :: Array DhallProperty
    , tags :: Array String
    , tabular :: Tabular TabularValue
    }


newtype DhallImport = DhallImport (Array { collection :: Array DhallSubjectRec })

derive newtype instance ReadForeign DhallImport
