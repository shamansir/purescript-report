module Report.Convert.Smos.Import where

import Prelude

import Foreign (Foreign, F)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Either (Either)
import Data.Bifunctor (bimap)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (toUpper, take, drop) as String
import Data.String.Common (split, joinWith) as String
import Data.String (Pattern(..), length) as String
import Data.Int (fromString) as Int

import Foreign.Object as Object

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Except (runExcept)

import Data.Argonaut.Core (Json)
import Yoga.JSON (E, readImpl)
import Data.YAML.Foreign.Decode (parseYAMLToJson)

import Report (Report)
import Report (build) as Report
import Report.GroupPath (pathFromArray) as GP
import Report.Decorator (fromArray') as Decorators
import Report.Decorator (Decorator(..)) as Dec
import Report.Decorators.Tags (Tags(..))
import Report.Decorators.Stats (Stats(..)) as Stats
import Report.Decorators.Task (TaskP(..))
import Report.Tabular (empty, fromArray') as Tabular
import Report.Decorators.Tabular.TabularValue as TV
import Report.Convert.Rep.Import.Parser as Parser
import Report.Convert.Types (SubjectId, ImportError(..))
import Report.Convert.Generic
import Report.Core (SDate(..), monthFromInt) as CT

import Report.Impl.Subject (Subject(..)) as Impl
import Report.Group (Group(..))
import Report.Impl.Group as Impl
import Report.Impl.Item (Item(..)) as Impl

import Report.Convert.Smos.Types


fromSmos :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromSmos yamlStr =
    ( parseYAMLToJson yamlStr
        >>= \json -> (readImpl (unsafeCoerce json :: Foreign) :: F SmosFile)
    )
    # runExcept
    # bimap FromJson
        ( \(SmosFile { value: forest }) ->
            Report.build [ collectSubject 0 "Smos" forest ]
        )
    where
        collectSubject :: Int -> String -> Array SmosTree -> subj /\ Array (group /\ Array item)
        collectSubject idx name forest =
            let subjId  = convertSubjectId @subj_id @subj_tag @item_tag @subj @group @item @x idx name Nothing
                subject = convertSubject   @subj_id @subj_tag @item_tag @subj @group @item @x $
                    Impl.Subject
                        { id     : subjId
                        , name   : name
                        , stats  : Stats.SYetUnknown
                        , tabular: Tabular.empty
                        , tags   : []
                        }
            in subject /\ collectForest [] forest

        collectForest :: Array String -> Array SmosTree -> Array (group /\ Array item)
        collectForest parentPath trees =
            let branches  = Array.filter (not <<< isLeafTree) trees
                topLeaves = if Array.null parentPath then Array.filter isLeafTree trees else []
                rootResult =
                    if Array.null topLeaves then []
                    else
                        let g = convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x $
                                    Group
                                        { path  : GP.pathFromArray ["_root"]
                                        , title : "_root"
                                        , stats : Stats.SYetUnknown
                                        }
                        in [ g /\ (collectItem <$> topLeaves) ]
            in rootResult <> Array.concatMap (collectBranch parentPath) branches

        collectBranch :: Array String -> SmosTree -> Array (group /\ Array item)
        collectBranch parentPath (SmosTree { entry: SmosEntry e, forest: mbChildren }) =
            let myPath   = parentPath <> [ e.header ]
                children = fromMaybe [] mbChildren
                leaves   = Array.filter isLeafTree children
                branches = Array.filter (not <<< isLeafTree) children
                g = convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x $
                        Group
                            { path  : GP.pathFromArray myPath
                            , title : e.header
                            , stats : Stats.SYetUnknown
                            }
            in [ g /\ (collectItem <$> leaves) ]
              <> Array.concatMap (collectBranch myPath) branches

        collectItem :: SmosTree -> item
        collectItem (SmosTree { entry: SmosEntry e }) =
            let mbTask = do
                    history <- e.stateHistory
                    first   <- Array.head history
                    let SmosStateEntry se = first
                    state   <- se.state
                    smosStateToTask state
                decorators = Decorators.fromArray' $ Array.catMaybes
                    [ mbTask <#> Dec.PTask ]
                timestamps = fromMaybe Object.empty e.timestamps
                tabular = Tabular.fromArray' $ Array.mapMaybe
                    (\k -> smosTimestampToTabular k $ fromMaybe "" $ Object.lookup k timestamps)
                    (Object.keys timestamps)
                rawTags = Array.catMaybes $ Parser.parseTag <$> fromMaybe [] e.tags
            in convertItem @subj_id @subj_tag @item_tag @subj @group @item @x $
                Impl.Item
                    { title      : e.header
                    , decorators : decorators
                    , tabular    : tabular
                    , tags       : Tags $
                        convertItemTag @subj_id @subj_tag @item_tag @subj @group @item @x
                            <$> rawTags
                    }


smosStateToTask :: String -> Maybe TaskP
smosStateToTask s = case String.toUpper s of
    "TODO"      -> Just TTodo
    "DONE"      -> Just TDone
    "STARTED"   -> Just TDoing
    "DOING"     -> Just TDoing
    "WAITING"   -> Just TWait
    "WAIT"      -> Just TWait
    "CANCELLED" -> Just TCanceled
    "CANCELED"  -> Just TCanceled
    "NEXT"      -> Just TNow
    "NOW"       -> Just TNow
    "LATER"     -> Just TLater
    _           -> Nothing


smosTimestampToTabular
    :: String
    -> String
    -> Maybe { key :: String, label :: String, value :: TV.TabularValue }
smosTimestampToTabular key value =
    let tabKey = String.toUpper key
    in parseOrgDate value <#> \d ->
        { key   : tabKey
        , label : tabKey
        , value : TV.date d
        }


parseOrgDate :: String -> Maybe CT.SDate
parseOrgDate str =
    let s0 = if String.take 1 str == "<" then String.drop 1 str else str
        s1 = let n = String.length s0
             in  if String.take 1 (String.drop (n - 1) s0) == ">" then String.take (n - 1) s0 else s0
    in case String.split (String.Pattern "-") s1 of
        [ yearS, monS, dayS ] ->
            (\year mon day -> CT.SDate { day, month: CT.monthFromInt mon, year })
                <$> Int.fromString yearS
                <*> Int.fromString monS
                <*> Int.fromString dayS
        _ -> Nothing
