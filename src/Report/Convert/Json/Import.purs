module Report.Convert.Json.Import where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes) as Array
import Data.Either (Either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Bifunctor (bimap)

import Yoga.JSON (E, readJSON)

import Report (Report)
import Report.Group (Group)
import Report as Report
import Report.Decorator (Decorator)
import Report.Decorator (decodeKey, decodeWithKey') as Decorator
import Report.Decorator (fromArray') as Decorators
import Report.Tabular as Tabular
import Report.Decorators.Tabular.TabularValue (TabularValue)
-- import Report.Decorators.Tabular.TabularValue as TabV
import Report.Convert.Types
import Report.Convert.Generic

import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Item (Item(..)) as Impl


fromJson :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromJson jsonStr =
    (readJSON jsonStr :: E ReportToImport) # bimap FromJson (unwrap >>> _.subjects >>> mapWithIndex jsonConvertSubjectWGroups >>> Report.build)
    where
        jsonConvertSubjectWGroups :: Int -> SubjectWithGroups -> subj /\ Array (group /\ Array item)
        jsonConvertSubjectWGroups idx (SubjectWithGroups { subject : Subject subjectRec, groups }) =
            jsonConvertSubject idx subjectRec
            /\
            ((\{ group, items } -> jsonConvertGroup group /\ (jsonConvertItem <$> items)) <$> groups)
        jsonConvertSubject :: Int -> SubjectRec -> subj
        jsonConvertSubject idx = jsonAdaptSubject idx >>> convertSubject @subj_id @subj_tag @item_tag @subj @group @item @x
        jsonConvertGroup :: Group -> group
        jsonConvertGroup = convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x
        jsonConvertItem :: ItemRec -> item
        jsonConvertItem = convertItem @subj_id @subj_tag @item_tag @subj @group @item @x <<< jsonAdaptItem
        jsonAdaptSubject :: Int -> SubjectRec -> Impl.Subject subj_id subj_tag
        jsonAdaptSubject idx subjectRec =
            Impl.Subject
                { name    : subjectRec.name
                , id      : convertSubjectId  @subj_id @subj_tag @item_tag @subj @group @item @x idx subjectRec.name $ Just subjectRec.id
                , tags    : convertSubjectTag @subj_id @subj_tag @item_tag @subj @group @item @x <$> subjectRec.tags
                , stats   : subjectRec.stats
                , tabular : Tabular.fromItems $ jsonAdaptTabular <$> subjectRec.tabulars
                }
        jsonAdaptItem :: ItemRec -> Impl.Item item_tag
        jsonAdaptItem itemRec =
            Impl.Item
                { title      : itemRec.title
                , decorators : Decorators.fromArray' $ Array.catMaybes $ jsonAdaptDecorator <$> itemRec.decorators
                , tabular    : Tabular.fromItems     $ jsonAdaptTabular <$> itemRec.tabulars
                , tags       : wrap $ convertItemTag @subj_id @subj_tag @item_tag @subj @group @item @x <$> unwrap itemRec.tags
                }
        jsonAdaptDecorator :: DecoratorRec -> Maybe Decorator
        jsonAdaptDecorator dRec = do
            Decorator.decodeKey dRec.mkey >>= \theKey -> Decorator.decodeWithKey' theKey dRec.fvalue
        jsonAdaptTabular :: TabularRec -> Tabular.Item TabularValue
        jsonAdaptTabular tabularRec =
            Tabular.Item
                { key   : tabularRec.tkey
                , label : tabularRec.tlabel
                , value : tabularRec.value
                }
