module Report.Convert.Rep.Import where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Bifunctor (lmap, rmap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (mapMaybe, catMaybes) as Array

import Report (Report)
import Report (build, mapSubjects, mapGroups, mapItems, toBuilder, fromBuilder) as Report
import Report.Group (Group(..))
import Report.GroupPath as GP
-- import Report.Convert.Keyed (keyOf)
import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Subject (mapTags) as SubjImpl
import Report.Impl.Item (Item(..)) as Impl
import Report.Impl.Item (mapTags) as ItemImpl
-- import Report.Impl.Group (Group) as Impl
-- import Report.Impl.Tag (Tag(..)) as Impl
import Report.Decorator (keyOf) as Decorator
import Report.Decorator (fromArray) as Decorators
import Report.Decorators.Tags (Tags(..))
import Report.Decorators.Stats as Stats
import Report.Convert.Generic (SubjectName, RawReportWith')
import Report.Convert.Generic (class ToImport, toImport') as Report
import Report.Convert.Generic (convertItem, convertGroup, convertSubject, convertItemTag, convertSubjectTag, convertSubjectId) as Import
import Report.Convert.Types (ImportError(..))
import Report.Impl.Subject (SubjectId)
import Report.Convert.Rep.Import.Parser as Parser
import Report.Tabular (Tabular(..))
import Report.Tabular (empty) as Tabular
import Report.Decorators.Tabular.TabularValue as TV


fromRep :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromRep =
    fromRepToImpl
    (Import.convertSubjectId @subj_id @subj_tag @item_tag @subj @group @item @x)
    >>> map (Report.toImport' @x @subj_id @subj_tag @item_tag @subj @group @item)


fromRepP :: String -> Either ImportError (Array Parser.RepSubject)
fromRepP = Parser.fromRep >>> lmap FromParser


fromRepToImpl :: forall @subj_id. (Int -> SubjectName -> Maybe SubjectId -> Maybe subj_id) -> String -> Either ImportError (RawReportWith' subj_id)
fromRepToImpl nameToSubjIdF = fromRepP >>> map (mapWithIndex tryConvertSubj >>> Array.catMaybes >>> Report.build)
  where
    tryConvertSubj idx subjRec =
        nameToSubjIdF idx subjRec.name subjRec.id <#> \subjId ->
        Impl.Subject
        { id : subjId
        , name : subjRec.name
        , stats : Stats.defaultStats
        , tabular : Tabular $ Array.catMaybes $ map (map TV.TVAtomic) <$> _.parsed <$> subjRec.tabulars
        , tags : subjRec.tags
        }
        /\
        (convertGroup subjId <$> subjRec.groups)
    convertGroup subjId groupRec =
        Group
        { path : GP.path groupRec.path
        , stats : Stats.defaultStats
        , title : groupRec.title
        }
        /\
        (convertItem groupRec <$> groupRec.items)
    convertItem groupRec itemRec =
        Impl.Item
        { title : itemRec.title
        , decorators : Decorators.fromArray $ Array.catMaybes $ map convertDecorator <$> _.parsed <$> itemRec.decorators
        , tags : Tags itemRec.tags
        , tabular : Tabular.empty
        }
    convertDecorator dec = Decorator.keyOf dec /\ dec