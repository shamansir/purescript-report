module Report.Convert.Org.Import where

import Prelude

import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (take, drop, length, contains, trim, indexOf, lastIndexOf, toUpper) as String
import Data.String (Pattern(..)) as String
import Data.String.Common (split) as String

import Foreign.Object (Object)
import Foreign.Object as Object

import Report (Report)
import Report (build) as Report
import Report.GroupPath (pathFromArray) as GP
import Report.Decorator (fromArray') as Decorators
import Report.Decorator (Decorator(..)) as Dec
import Report.Decorators.Tags (Tags(..))
import Report.Decorators.Stats (Stats(..)) as Stats
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Progress (Progress(..))
import Report.Tabular (empty, fromArray') as Tabular
import Report.Decorators.Tabular.TabularValue as TV
import Report.Convert.Rep.Import.Parser as Parser
import Report.Convert.Types (ImportError(..))
import Report.Convert.Generic
import Report.Core (dateToRec) as CT

import Report.Impl.Subject (Subject(..)) as Impl
import Report.Group (Group(..))
import Report.Impl.Group as Impl
import Report.Impl.Item (Item(..)) as Impl

import Report.Convert.Smos.Import (smosStateToTask, parseOrgDate, smosTimestampToTabular)


type OrgLogbookEntry = { start :: String, end :: Maybe String }

type OrgNodeRec =
    { level      :: Int
    , taskState  :: Maybe String
    , title      :: String
    , tags       :: Array String
    , scheduled  :: Maybe String
    , deadline   :: Maybe String
    , logbook    :: Array OrgLogbookEntry
    , properties :: Object String
    , children   :: Array OrgNode
    }

newtype OrgNode = OrgNode OrgNodeRec


isLeafNode :: OrgNode -> Boolean
isLeafNode (OrgNode { children }) = Array.null children


fromOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromOrg orgStr =
    let nodes = buildNodes $ String.split (String.Pattern "\n") orgStr
    in Right $ Report.build $ Array.catMaybes [ collectSubject 0 "Org" nodes ]
  where
    collectSubject :: Int -> String -> Array OrgNode -> Maybe (subj /\ Array (group /\ Array item))
    collectSubject idx name nodes =
        convertSubjectId @subj_id @subj_tag @item_tag @subj @group @item @x idx name Nothing >>= \subjId ->
            convertSubject @subj_id @subj_tag @item_tag @subj @group @item @x
                (Impl.Subject
                    { id     : subjId
                    , name   : name
                    , stats  : Stats.SYetUnknown
                    , tabular: Tabular.empty
                    , tags   : []
                    })
                <#> (_ /\ collectForest [] nodes)

    collectForest :: Array String -> Array OrgNode -> Array (group /\ Array item)
    collectForest parentPath nodes =
        let branches  = Array.filter (not <<< isLeafNode) nodes
            topLeaves = if Array.null parentPath then Array.filter isLeafNode nodes else []
            rootResult =
                if Array.null topLeaves then []
                else Array.catMaybes
                    [ convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x
                          (Group { path: GP.pathFromArray ["_root"], title: "_root", stats: Stats.SYetUnknown })
                          <#> \g -> g /\ Array.catMaybes (collectItem <$> topLeaves)
                    ]
        in rootResult <> Array.concatMap (collectBranch parentPath) branches

    collectBranch :: Array String -> OrgNode -> Array (group /\ Array item)
    collectBranch parentPath (OrgNode n) =
        let myPath   = parentPath <> [ n.title ]
            leaves   = Array.filter isLeafNode n.children
            branches = Array.filter (not <<< isLeafNode) n.children
        in Array.catMaybes
            [ convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x
                  (Group { path: GP.pathFromArray myPath, title: n.title, stats: Stats.SYetUnknown })
                  <#> \g -> g /\ Array.catMaybes (collectItem <$> leaves)
            ]
          <> Array.concatMap (collectBranch myPath) branches

    collectItem :: OrgNode -> Maybe item
    collectItem (OrgNode n) =
        let mbTask     = n.taskState >>= smosStateToTask
            decorators = Decorators.fromArray' $ Array.catMaybes
                [ mbTask <#> Dec.PTask ]
            tsEntries  = Array.catMaybes
                [ n.scheduled >>= smosTimestampToTabular "SCHEDULED"
                , n.deadline  >>= smosTimestampToTabular "DEADLINE"
                ]
            lbEntry    = orgLogbookToTabular n.logbook
            tabular    = Tabular.fromArray' $ tsEntries <> Array.catMaybes [ lbEntry ]
            rawTags    = Array.catMaybes $ Parser.parseTag <$> n.tags
        in convertItem @subj_id @subj_tag @item_tag @subj @group @item @x $
            Impl.Item
                { title      : n.title
                , decorators : decorators
                , tabular    : tabular
                , tags       : Tags $
                    Array.catMaybes $ convertItemTag @subj_id @subj_tag @item_tag @subj @group @item @x
                        <$> rawTags
                }


orgLogbookToTabular
    :: Array OrgLogbookEntry
    -> Maybe { key :: String, label :: String, value :: TV.TabularValue }
orgLogbookToTabular [] = Nothing
orgLogbookToTabular entries =
    let levels = Array.mapWithIndex toLevelP entries
    in Just { key: "logbook", label: "Logbook", value: TV.progress (LevelsP { levels }) }
  where
    toLevelP idx { start, end } =
        { name    : "entry-" <> show (idx + 1)
        , proc    : if isNothing end then TDoing else TDone
        , date    : parseOrgDate (String.take 10 start) <#> CT.dateToRec
        , endDate : end >>= \e -> parseOrgDate (String.take 10 e) <#> CT.dateToRec
        }


-- Build an OrgNode tree from the lines of an Org file
buildNodes :: Array String -> Array OrgNode
buildNodes = go
  where
    go lines =
        case Array.uncons lines of
            Nothing -> []
            Just { head: l, tail: ls } ->
                case headingLevel l of
                    Nothing -> go ls
                    Just n ->
                        let content                       = String.drop (n + 1) l
                            { state, rest }               = peelTaskState content
                            { title, tags }               = peelTags rest
                            { init: scope, rest: follow } = Array.span
                                (\ln -> headingLevel ln # maybe true (_ > n))
                                ls
                            node = OrgNode
                                { level      : n
                                , taskState  : state
                                , title      : String.trim title
                                , tags       : tags
                                , scheduled  : findTimestamp "SCHEDULED" scope
                                , deadline   : findTimestamp "DEADLINE" scope
                                , logbook    : parseLogbookEntries scope
                                , properties : parsePropertiesBlock scope
                                , children   : buildNodes scope
                                }
                        in Array.cons node (go follow)


headingLevel :: String -> Maybe Int
headingLevel line =
    let n = countLeadingChar "*" line
    in if n > 0 && String.take 1 (String.drop n line) == " "
       then Just n
       else Nothing


countLeadingChar :: String -> String -> Int
countLeadingChar c = go 0
  where
    go acc s =
        if String.take 1 s == c
        then go (acc + 1) (String.drop 1 s)
        else acc


orgKeywords :: Array String
orgKeywords =
    [ "TODO", "DONE", "STARTED", "DOING", "WAITING"
    , "WAIT", "CANCELLED", "CANCELED", "NEXT", "NOW", "LATER"
    ]


peelTaskState :: String -> { state :: Maybe String, rest :: String }
peelTaskState line =
    let mbKw = Array.find (\kw ->
            String.take (String.length kw + 1) line == kw <> " ") orgKeywords
    in case mbKw of
        Just kw -> { state: Just kw, rest: String.drop (String.length kw + 1) line }
        Nothing -> { state: Nothing, rest: line }


peelTags :: String -> { title :: String, tags :: Array String }
peelTags line =
    let trimmed = String.trim line
        len     = String.length trimmed
    in if len > 0 && String.take 1 (String.drop (len - 1) trimmed) == ":"
    then
        case String.lastIndexOf (String.Pattern " :") trimmed of
            Nothing  -> { title: trimmed, tags: [] }
            Just idx ->
                let tagsPart = String.drop (idx + 1) trimmed
                    titlePart = String.trim $ String.take idx trimmed
                    inner = String.drop 1 $ String.take (String.length tagsPart - 1) tagsPart
                    tags  = Array.filter (_ /= "") $ String.split (String.Pattern ":") inner
                in { title: titlePart, tags }
    else { title: trimmed, tags: [] }


findTimestamp :: String -> Array String -> Maybe String
findTimestamp key lines =
    let prefix = key <> ": "
    in Array.findMap (\line ->
        let stripped = String.trim line
        in if String.take (String.length prefix) (String.toUpper stripped) == prefix
           then Just $ String.trim $ String.drop (String.length prefix) stripped
           else Nothing) lines


extractDrawer :: String -> Array String -> Maybe (Array String)
extractDrawer name lines =
    let openPattern = String.Pattern $ ":" <> String.toUpper name <> ":"
    in Array.findIndex (String.contains openPattern <<< String.toUpper <<< String.trim) lines >>=
        \startIdx ->
            let after = Array.drop (startIdx + 1) lines
            in case Array.findIndex (\l ->
                    String.contains (String.Pattern ":END:") (String.toUpper (String.trim l))) after of
                Nothing   -> Just after
                Just endIdx -> Just (Array.take endIdx after)


parseLogbookEntries :: Array String -> Array OrgLogbookEntry
parseLogbookEntries lines =
    case extractDrawer "LOGBOOK" lines of
        Nothing          -> []
        Just drawerLines -> Array.mapMaybe parseClockLine drawerLines


parseClockLine :: String -> Maybe OrgLogbookEntry
parseClockLine line =
    let stripped = String.trim line
    in if String.take 7 stripped == "CLOCK: "
       then
           let rest         = String.drop 7 stripped
               startContent = extractBracket "[" "]" rest
               endContent   = case String.indexOf (String.Pattern "--") rest of
                   Nothing  -> Nothing
                   Just idx -> extractBracket "[" "]" (String.drop (idx + 2) rest)
           in startContent <#> \s -> { start: s, end: endContent }
       else Nothing


extractBracket :: String -> String -> String -> Maybe String
extractBracket open close s =
    String.indexOf (String.Pattern open) s >>=
        \startIdx ->
            let inner = String.drop (startIdx + 1) s
            in case String.indexOf (String.Pattern close) inner of
                Nothing     -> Nothing
                Just endIdx -> Just $ String.take endIdx inner


parsePropertiesBlock :: Array String -> Object String
parsePropertiesBlock lines =
    case extractDrawer "PROPERTIES" lines of
        Nothing          -> Object.empty
        Just drawerLines -> Array.foldl parsePropLine Object.empty drawerLines
  where
    parsePropLine acc line =
        let stripped = String.trim line
        in if String.take 1 stripped == ":"
           then
               let rest = String.drop 1 stripped
               in case String.indexOf (String.Pattern ": ") rest of
                   Nothing  -> acc
                   Just idx ->
                       let key   = String.take idx rest
                           value = String.trim $ String.drop (idx + 2) rest
                       in Object.insert key value acc
           else acc
