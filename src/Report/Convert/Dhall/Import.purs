module Report.Convert.Dhall.Import where

import Prelude

import Foreign (F, Foreign)

import Control.Alt ((<|>))

import Data.Array (cons, concat, index, length, elemIndex, findMap, mapMaybe, catMaybes, groupAllBy, singleton) as Array
import Data.Array.Extra (groupExtBy) as ArrayX
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray, head) as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either, hush, blush, isLeft, isRight)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Data.FunctorWithIndex (mapWithIndex)

import Report (Report)
import Report (build) as Report
import Report.Core as CT
import Report.Chain (Chain)
import Report.Chain as Chain
import Report.Convert.Generic
import Report.Convert.Types (SubjectId, ImportError(..))
import Report.Tabular (Tabular(..))
import Report.Tabular (empty, fromArray) as Tabular
import Report.Group (Group(..)) as Impl
import Report.GroupPath (GroupPath)
import Report.GroupPath (pathFromArray) as GP
import Report.Decorator (fromArray) as Decorators
import Report.Decorator (Key(..), keyOf) as Dec
import Report.Decorator (Decorator(..)) as Dec
import Report.Decorators.Progress (Progress(..), DateRec)
import Report.Decorators.Progress (fromJson, rawToProgressJson) as Progress
import Report.Decorators.Tags (Tags(..), RawTag)
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Decorators.Tabular.TabularValue (progress) as TV
import Report.Decorators.Stats (Stats(..)) as Stats
import Report.Convert.Rep.Import.Parser as Parser

import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Subject (mapTags, mapId) as SubjImpl
import Report.Impl.Group (Group(..)) as Impl
import Report.Impl.Item (Item(..)) as Impl
import Report.Impl.Item (mapTags) as ItemImpl
import Report.Impl.Group (Group) as Impl


import Yoga.JSON (E, class ReadForeign, readImpl, readJSON)


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
    { id :: SubjectId
    , name :: String
    , properties :: Array DhallProperty
    , tags :: Array String
    , tabular :: Array DhallTabularRec
    }


type DhallTabularRec =
    { key :: String
    , value ::
        { t :: String
        , v :: Foreign
        }
    }


newtype DhallImport = DhallImport { collection :: Array DhallSubjectRec }


derive newtype instance ReadForeign DhallImport


fromDhall :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromDhall jsonStr =
    let eSubjects =
                ((readJSON jsonStr :: E DhallImport)     <#> \(DhallImport { collection }) -> collection)
            <|> ((readJSON jsonStr :: E DhallSubjectRec) <#> Array.singleton)
    in eSubjects # bimap FromJson (mapWithIndex dhallCollectSubject >>> Report.build)
    where
        dhallCollectSubject :: Int ->  DhallSubjectRec -> subj /\ Array (group /\ Array item)
        dhallCollectSubject idx subjectRec =
            dhallConvertSubject idx subjectRec
            /\ collectGroupsWithItems subjectRec.properties
        collectGroupsWithItems :: Array DhallProperty -> Array (group /\ Array item)
        collectGroupsWithItems = map dhallCollectProperty >>> foldGroups >>> map (bimap dhallConvertGroup $ map dhallConvertItem)
        foldGroups :: Array (Either DhallGroupRec DhallItemRec) -> Array (DhallGroupRec /\ Array DhallItemRec)
        foldGroups = _groupExtByKey (either _.ref _.ref) identity
        dhallCollectProperty :: DhallProperty -> Either DhallGroupRec DhallItemRec
        dhallCollectProperty = case _ of
            HoldsGroup groupRec -> Left  groupRec
            HoldsItem  itemRec  -> Right itemRec
        dhallConvertSubject :: Int -> DhallSubjectRec -> subj
        dhallConvertSubject idx subjectRec =
            let
                subjId = convertSubjectId @subj_id @subj_tag @item_tag @subj @group @item @x idx subjectRec.name (Just subjectRec.id)
            in
                convertSubject @subj_id @subj_tag @item_tag @subj @group @item @x
                    (Impl.Subject
                        { id : subjId
                        , name : subjectRec.name
                        , stats : Stats.SYetUnknown
                        , tabular : TV.progress <$> loadTabular subjectRec.tabular
                        , tags : convertSubjectTag @subj_id @subj_tag @item_tag @subj @group @item @x
                                 <$> (Array.catMaybes $ toRawTag <$> subjectRec.tags)
                        }
                    )
        dhallConvertGroup :: DhallGroupRec -> group
        dhallConvertGroup groupRec =
            convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x
                (Impl.Group
                    { path : pathFromRef groupRec.ref
                    , stats : Stats.SYetUnknown
                    , title : groupRec.title
                    }
                )
        dhallConvertItem :: DhallItemRec -> item
        dhallConvertItem itemRec =
            convertItem @subj_id @subj_tag @item_tag @subj @group @item @x
                (Impl.Item
                    { title : fromMaybe (fromMaybe "???" itemRec.title) itemRec.key
                    , decorators :
                        Decorators.fromArray
                            $ Array.catMaybes
                            $ Array.cons
                                (itemRec.value <#> Progress.rawToProgressJson >>= Progress.fromJson <#> Dec.SProgress <#> (\p -> Dec.keyOf p /\ p))
                                [ itemRec.detailed <#>                 \d -> Dec.KDescription /\ Dec.SDescription d
                                , itemRec.selfRef  <#> pathFromRef <#> \p -> Dec.KReference /\ Dec.SReference p
                                , itemRec.date     <#>                 \d -> Dec.KEarnedAt  /\ Dec.SEarnedAt (CT.dateFromRec d)
                                ]
                    , tabular : Tabular.empty -- FIXME: items contains no tabular. TV.progress <$> loadTabular itemRec.tabular
                    , tags : Tags $
                            ( convertItemTag @subj_id @subj_tag @item_tag @subj @group @item @x
                              <$> (Array.catMaybes $ toRawTag <$> fromMaybe [] itemRec.tags)
                            )
                    }
                )
        toRawTag :: String -> Maybe RawTag
        toRawTag = Parser.parseTag
        pathFromRef :: Ref -> GroupPath
        pathFromRef = GP.pathFromArray
        loadTabular :: Array DhallTabularRec -> Tabular Progress
        loadTabular = Tabular.fromArray <<< map loadProgress
            where
                loadProgress { key, value } =
                    key /\
                        (Progress.rawToProgressJson value
                            # Progress.fromJson
                            # fromMaybe Unknown
                            -- # maybe (TV.TVString "---") (TV.TVSuffix <<< S.SProgress)
                            )


-- a function to find a group in the list of either groups or items
_groupExtByKey :: forall a b c x. Ord x => (a -> x) -> (a -> Either b c) -> Array a -> Array (b /\ Array c)
_groupExtByKey toKey toBorC =
    Array.groupAllBy (\ia ib -> compare (toKey ia) (toKey ib))
    >>> map (NEA.toArray >>> map toBorC) >>> map (\arr ->
        let
            -- when already grouped by `x` key, inside this group
            -- find at least one `b` and consider all other `cs` as belonging to this `b`
            mbB   = Array.findMap blush arr
            allCs = Array.mapMaybe hush arr
        in
            mbB <#> \b -> b /\ allCs
    )
    >>> Array.catMaybes -- all the groups that don't have any `b` are not valid and should be discarded