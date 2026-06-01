module Report.Web.Component where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Map (Map, SemigroupMap(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe (isJust) as Maybe
import Data.Either (Either(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Set as Set
import Data.String (length, contains, toLower, joinWith, split, Pattern(..)) as String
import Data.Tuple (uncurry, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Control.Applicative.Extra (whenJust)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)

import Report as R
import Report.Builder as RB
import Report.Chain (Chain)
import Report.Chain (allIn, fromString, toString, length, last, toArray) as Chain
import Report.Class as R
import Report.Core (ReportFormat(..))

import Report.Core.Logic (EncodedValue(..), view, edit, isEditing, loadViewOrEdit, ViewOrEdit) as CT
import Report.Decorator (get, put, debugNavLabel, prefixes, suffixes) as Decorator
import Report.Decorator (size, keys, collectProgress, hasProgress) as Decorators
import Report.Decorators.Class.ValueModify as VModify
import Report.Decorators.Stats (GotTotal(..), gotTotalFromStats, weightOf, Stats(..)) as R
import Report.Decorators.Stats.Collect as Collect
import Report.Decorators.Tags (TagAction(..), RawTag, RawTagKind)
import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep, startsWithNotEq) as GP
import Report.Impl.Subject (Subject, SubjectId) as Impl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Item (Item) as Impl
import Report.Modify (Location(..), whatKeyOf)
import Report.Modify as Modify

import Report.Convert.Generic (class ToExport, class ToImport, includeOnly, RR) as Report
import Report.Convert.Types (ImportError(..), printImportError)
import Report.Convert.Dhall (toDhall) as Report
import Report.Convert.Json (toJson) as Report
import Report.Convert.Org (toOrg) as Report
import Report.Convert.Text (toText) as Report
import Report.Convert.Rep (toRep, fromRep) as Report
import Report.Convert.Smos (toSmos) as Report
import Report.Convert.Text.Decorator (encodeDecorator) as Decorator

import Report.Web.Component.RecalcBehavior as CRB
import Report.Web.Decorators (renderPrefixes, renderSuffixes, renderTags)
import Report.Web.Decorators.EditInput as EI
import Report.Web.Decorators.Stats (renderGroupStats, renderProgressPlates, gotTotalBadge)
import Report.Web.Decorators.Tags (subjTagBadge, subjTagWrap, itemTagBadge, itemTagKindBadge)
import Report.Web.GroupPath (groupPathId) as GP
import Report.Web.GroupPath (groupPathId, renderPath)
import Report.Web.Helpers (qspacerSpan, qcolorSpan, qitemmarkerSpan, lineHeight, nestMargin, qemptySpan, H)
import Report.Web.Helpers.InlineOrBlock as IoB
import Report.Web.Helpers.UrlConfig as UC
import Report.Web.Helpers.VisualState (selectOne, itemNameColor) as VStates
import Report.Web.Navigation2 (NavigatedTo)
import Report.Web.Navigation2 as Navigation
import Report.Web.Tabular (renderSubjectTabularValues, renderItemTabularValues)

import Web.Event.Event (stopPropagation) as Event
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.Window (toEventTarget, innerWidth, innerHeight, location, history, document) as Window
import Web.HTML.History (URL(..), DocumentTitle(..), state, pushState) as History
import Web.HTML.Location (hash, setHash, search, setSearch) as Location
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent, shiftKey, altKey, metaKey, ctrlKey) as ME
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent (toEvent, code, key, fromEvent) as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET


import Yoga.JSON (writePrettyJSON, class WriteForeign, class ReadForeign)


type Process item_tag_kind item_tag = TagAction item_tag_kind item_tag


type CollapseMap subj_id = Map subj_id (Map GroupPath Boolean)


data SubjectFilter
    = NoSubjectFilter
    | FromUser String
    | FromUrl String


type Flags =
    { readOnlyMode :: Boolean
    , debugEnabled :: Boolean
    , showItemsTags :: Boolean
    , optionsPaneExpanded :: Boolean
    , subjectNavigationExpanded :: Boolean
    , subjectNavigationPinned :: Boolean
    , progressPlatesShown :: Boolean
    , groupNavigationExpanded :: Boolean
    , groupNavigationPinned :: Boolean
    , exportProcessedReport :: Boolean
    , editingExportContent :: Boolean
    }


type State subj_id subj_tag item_tag_kind item_tag report =
    { subjects :: Array subj_id
    , sourceReport :: report
    , processedReport :: report
    , filter :: SubjectFilter
    , tagFilter :: Array subj_tag
    , sortBy :: SubjectSort
    , flags :: Flags
    , mbExportTo :: Maybe ReportFormat
    , importFrom :: Map ReportFormat String
    , navigatedTo :: NavigatedTo subj_id
    , process :: Array (Process item_tag_kind item_tag)
    , collapsed :: CollapseMap subj_id
    }


type Input subj group item =
    R.Report subj group item


data Action subj_id subj_tag item_tag_kind item_tag report
    = Initialize
    | Receive report
    | HandleUrlChange
    | SelectSubject subj_id
    | AddSubjectToSelection subj_id
    | DeselectSubject subj_id
    | DeselectAllSubjects
    | ChangeListFilter String
    | IncludeTag subj_tag
    | ExcludeTag subj_tag
    | ToggleOptionsPane
    | ToggleItemsTagsInOptions
    | NextSort
    | ClearNavigation
    | NavigateByMouseClickTo MouseEvent (Location subj_id)
    | TryNavigateWithKeyboard KeyboardEvent
    | HandleDocumentKey KeyboardEvent
    | ApplyEditAt (Location subj_id) CT.EncodedValue
    | StartEditing MouseEvent
    | CancelEditing
    | ToggleReadOnlyMode
    | ToggleDebugMode
    | ToggleProgressPlates
    | ExpandSubjectNavigation
    | CollapseSubjectNavigation
    | ToggleSubjectNavigationPinned
    | ExpandGroupNavigation
    | CollapseGroupNavigation
    | ToggleGroupNavigationPinned
    | EnableExport ReportFormat
    | DisableExport
    | AddToItemsFilter MouseEvent item_tag
    | CancelProcess MouseEvent (Process item_tag_kind item_tag)
    | SortItemsBy MouseEvent item_tag_kind
    | GroupItemsBy MouseEvent item_tag_kind
    | ResetPostProcess
    | ToggleGroupCollapse MouseEvent subj_id GroupPath
    | TryImportFrom ReportFormat
    | TryImportFromCurrent
    | StoreContentToImport ReportFormat String
    | StartedEditingExportContent
    | FinishedEditingExportContent
    | NoOp


data SubjectSort
    = ByWeight
    | Alpha
    -- | TODO:

derive instance Eq SubjectSort


data SortKey
    = SN Number
    | SS String


derive instance Eq SortKey
derive instance Ord SortKey


type Version = Int


type Config subj_id =
    { preSelected :: Array subj_id
    , recalculate :: CRB.RecalcBehavior
    }


defaultConfig :: forall subj_id. Config subj_id
defaultConfig =
    { preSelected : []
    , recalculate : CRB.noUpdate
    }


class
    ( R.IsItem item
    , R.IsGroup group
    , R.IsSubject subj_id subj
    , R.IsTag subj_tag
    , R.IsTag item_tag
    , R.IsTag item_tag_kind
    , R.LimitedSet subj_tag
    , R.IsSortable item_tag_kind item_tag
    , R.IsGroupable group item_tag
    )
    <= Is subj_id subj_tag item_tag_kind item_tag subj group item (x :: Type)


instance
    ( R.IsItem item
    , R.IsGroup group
    , R.IsSubject subj_id subj
    , R.IsTag subj_tag
    , R.IsTag item_tag
    , R.IsTag item_tag_kind
    , R.LimitedSet subj_tag
    , R.IsSortable item_tag_kind item_tag
    , R.IsGroupable group item_tag
    ) =>
    Is subj_id subj_tag item_tag_kind item_tag subj group item (R.Report subj group item)


class
    ( R.HasTags subj_tag subj
    , R.HasTags item_tag item
    , R.HasStats subj
    , R.HasStats group
    , R.HasDecorators item
    , R.HasTabular item
    , R.HasTabular subj
    , R.HasStats group
    )
    <= Has subj_tag item_tag subj group item (x :: Type)


instance
    ( R.HasTags item_tag item
    , R.HasTags subj_tag subj
    , R.HasStats subj
    , R.HasStats group
    , R.HasDecorators item
    , R.HasTabular item
    , R.HasTabular subj
    ) =>
    Has subj_tag item_tag subj group item (R.Report subj group item)


class
    ( Modify.GroupModify group
    , Modify.StatsModify group
    , Modify.DecoratorsModify item
    , Modify.TagsModify item_tag item
    , Modify.ItemModify item
    )
    <= Modify item_tag group item (x :: Type)


instance
    ( Modify.GroupModify group
    , Modify.StatsModify group
    , Modify.DecoratorsModify item
    , Modify.TagsModify item_tag item
    , Modify.ItemModify item
    ) =>
    Modify item_tag group item (R.Report subj group item)


class
    ( R.ConvertFrom (Chain String) item_tag
    , R.ConvertFrom (Chain String) subj_tag
    , R.ConvertFrom (Chain String) item_tag_kind
    , R.ConvertTo   (Chain String) item_tag
    , R.ConvertTo   (Chain String) subj_tag
    , R.ConvertTo   (Chain String) item_tag_kind
    )
    <= TagsChainConvert item_tag_kind item_tag subj_tag (x :: Type)


instance
    ( R.ConvertFrom (Chain String) item_tag
    , R.ConvertFrom (Chain String) subj_tag
    , R.ConvertFrom (Chain String) item_tag_kind
    , R.ConvertTo   (Chain String) item_tag
    , R.ConvertTo   (Chain String) subj_tag
    , R.ConvertTo   (Chain String) item_tag_kind
    ) =>
    TagsChainConvert item_tag_kind item_tag subj_tag x


instance Is Impl.SubjectId RawTag RawTagKind RawTag (Impl.Subject Impl.SubjectId RawTag) Impl.Group (Impl.Item RawTag) Report.RR
instance Has               RawTag            RawTag (Impl.Subject Impl.SubjectId RawTag) Impl.Group (Impl.Item RawTag) Report.RR
instance Modify                              RawTag                                      Impl.Group (Impl.Item RawTag) Report.RR
-- instance TagsChainConvert         RawTagKind RawTag RawTag Report.RR


type ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item =
    State subj_id subj_tag item_tag_kind item_tag (R.Report subj group item)

type ReportComponentAction subj_id subj_tag item_tag_kind item_tag subj group item =
    Action subj_id subj_tag item_tag_kind item_tag (Input subj group item)

type ReportComponentM subj_id subj_tag item_tag_kind item_tag subj group item output m =
    H.HalogenM
        (ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item)
        (ReportComponentAction subj_id subj_tag item_tag_kind item_tag subj group item)
        ()
        output
        m
        Unit


component
    :: forall @x @subj_id @subj_tag @item_tag_kind @item_tag @subj @group @item query output m
     . MonadEffect m
    => Ord subj_id
    => Ord item_tag
    => Eq subj_tag
    => Eq item_tag_kind
    => Is subj_id subj_tag item_tag_kind item_tag subj group item x
    => Has subj_tag item_tag subj group item x
    => Modify item_tag group item x
    => TagsChainConvert item_tag_kind item_tag subj_tag x
    => Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.ToImport subj_id subj_tag item_tag subj group item x
    => R.ToReport subj group item x
    => Config subj_id
    -> H.Component query x output m
component cfg =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive <<< R.toReport
            }
        }
    where
    initialState :: x -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
    initialState x =
        let srcReport = R.toReport x in
        { subjects : cfg.preSelected
        , sourceReport : srcReport
        , processedReport : processReport cfg.preSelected [] srcReport
        , filter : NoSubjectFilter
        , tagFilter : []
        , sortBy : ByWeight
        , mbExportTo : Nothing
        , importFrom : Map.empty
        , navigatedTo : Navigation.init
        , process : []
        , collapsed : Map.empty
        , flags :
            { optionsPaneExpanded : true
            , showItemsTags : true
            , readOnlyMode : true
            , debugEnabled : false
            , subjectNavigationExpanded : false
            , subjectNavigationPinned : false
            , groupNavigationExpanded : false
            , groupNavigationPinned : false
            , progressPlatesShown : false
            , exportProcessedReport : false
            , editingExportContent : false
            }
        }

    processReport :: Array subj_id -> Array (Process item_tag_kind item_tag) -> R.Report subj group item -> R.Report subj group item
    processReport subjects process report =
        R.toBuilder report
            # RB.filterSubjects (R.s_id >>> flip Array.elem subjects)
            # R.fromBuilder
            # postProcess @item_tag_kind cfg.recalculate process

    processSourceReportUsingState :: ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item -> R.Report subj group item
    processSourceReportUsingState state = processReport state.subjects state.process state.sourceReport

    updateReports :: R.Report subj group item -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
    updateReports srcReport s = let nextState = s { sourceReport = srcReport } in nextState { processedReport = processSourceReportUsingState nextState }

    updateProcessedReport :: ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
    updateProcessedReport s = updateReports s.sourceReport s

    reportAreaRefLabel = H.RefLabel "report-area" :: H.RefLabel
    importExportTextareaRefLabel = H.RefLabel "import-export-textarea" :: H.RefLabel

    render :: ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item -> HH.ComponentHTML (ReportComponentAction subj_id subj_tag item_tag_kind item_tag subj group item) () m
    render state =
        HH.div
            [ HP.style "font-family: \"JetBrains Mono\", sans-serif; display: flex; flex-direction: row;"
            ]
            [ HH.div
                [ HP.style "height: 100vh; min-width: 75%; overflow-y: scroll;outline:none;"
                -- , HE.onClick $ const ClearNavigation
                , HE.onKeyDown TryNavigateWithKeyboard
                , HP.ref reportAreaRefLabel
                , HP.tabIndex 1
                ]
                $
                ( Tuple.uncurry
                    (renderSubject
                        @subj_id @subj_tag @item_tag_kind @item_tag
                        subjRenderOptions
                        state.navigatedTo
                        state.collapsed
                    )
                    <$> selectedSubjects
                )
                <> pure menuButtons
                <> pure subjSelNavigation
                <> pure groupSelNavigation
            , HH.div
                [ HP.style "margin: 0 auto; max-width: 900px; padding: 20px 20px 50px 20px;" ]
                [ subjectsToc state allSubjects ]
            , case state.mbExportTo of
                Just exportTarget ->
                    HH.div
                        [ HP.style $ "position: absolute; right: 25%; top: 43px; width: 25%; max-height: 50%; overflow-y: scroll;"
                        <> "background-color: aliceblue; border-radius: 5px; padding: 5px;"
                        ]
                        [ HH.textarea
                            [ HP.style "white-space: pre-wrap; background: white; border: none; font-size: 0.7em;"
                            , HP.value $ exportTextFor exportTarget, HP.cols 75, HP.rows 30
                            , HP.ref importExportTextareaRefLabel
                            , HE.onFocus $ const StartedEditingExportContent
                            , HE.onBlur  $ const FinishedEditingExportContent
                            , HE.onValueInput $ StoreContentToImport exportTarget
                            ]
                        , HH.div
                            [ HP.style "position: absolute; right: 0; top: 0; padding: 6px 0px;" ]
                            [ if importSupportedFor exportTarget
                                then menuButton
                                    { enabled : true
                                    , label   : "↫" -- if state.flags.instantImportEnabled then "⟺" else "⤄"
                                    , onClick : const $ TryImportFrom exportTarget
                                    }
                                else HH.text ""
                            ]
                        ]
                Nothing -> HH.text ""
            , if state.flags.debugEnabled then navigationHint @subj state.navigatedTo else HH.text ""
            ]
        where

            -- processedReport =
            --     R.toBuilder state.report
            --     # RB.filterSubjects (s_id >>> flip Array.elem state.subjects)
            --     # R.fromBuilder
            --     # postProcess @item_tag_kind cfg.recalculate state.process
            -- processedReport = state.processedReport
            selectedSubjects = state.processedReport # R.unfoldAll
            allSubjects = RB.allSubjects $ R.toBuilder state.sourceReport
            selKeys = Map.fromFoldable $ (\subj -> R.s_id subj /\ subj) <$> allSubjects

            includeRule = Report.includeOnly state.subjects

            subjRenderOptions = { progressPlatesShown : state.flags.progressPlatesShown }

            reportToExport = if not state.flags.exportProcessedReport then state.sourceReport else state.processedReport

            exportTextFor = case _ of
                Json  -> reportToExport # Report.toJson  @x @subj_id @subj_tag @item_tag includeRule
                Dhall -> reportToExport # Report.toDhall @x @subj_id @subj_tag @item_tag includeRule
                Org   -> reportToExport # Report.toOrg   @x @subj_id @subj_tag @item_tag includeRule
                Rep   -> reportToExport # Report.toRep   @x @subj_id @subj_tag @item_tag includeRule
                Text  -> reportToExport # Report.toText  @x @subj_id @subj_tag @item_tag includeRule
                Smos  -> reportToExport # Report.toSmos  @x @subj_id @subj_tag @item_tag includeRule
            exportSelected trg = state.mbExportTo == Just trg

            findSubjName :: subj_id -> Maybe String
            findSubjName subjId = Map.lookup subjId selKeys <#> R.s_name @subj_id

            menuButtons =
                HH.div
                    [ HP.style "position: fixed; right: 25%; top: 0; border-radius: 5px; background: aliceblue; padding: 5px;" ]
                    $ menuButton <$>
                        (if not state.flags.readOnlyMode && Navigation.isEditing state.navigatedTo then
                            [ { label : "✎", onClick : const CancelEditing, enabled : true } ]
                        else
                            [ ]
                        )
                        <>
                        [ { label : if state.flags.readOnlyMode then "🔒" else "🔓",      onClick : const ToggleReadOnlyMode, enabled : state.flags.readOnlyMode }
                        , { label : if state.flags.debugEnabled then "🛠" else "🛠",      onClick : const ToggleDebugMode,    enabled : state.flags.debugEnabled }
                        , { label : if state.flags.progressPlatesShown then "▦" else "▦", onClick : const ToggleProgressPlates, enabled : state.flags.progressPlatesShown }
                        , { label : "JSON",  onClick : const $ if exportSelected Json  then DisableExport else EnableExport Json,  enabled : exportSelected Json  }
                        , { label : "DHALL", onClick : const $ if exportSelected Dhall then DisableExport else EnableExport Dhall, enabled : exportSelected Dhall }
                        , { label : "ORG",   onClick : const $ if exportSelected Org   then DisableExport else EnableExport Org,   enabled : exportSelected Org   }
                        , { label : "REP",   onClick : const $ if exportSelected Rep   then DisableExport else EnableExport Rep,   enabled : exportSelected Rep   }
                        , { label : "SMOS",  onClick : const $ if exportSelected Rep   then DisableExport else EnableExport Smos,  enabled : exportSelected Smos  }
                        , { label : "TEXT",  onClick : const $ if exportSelected Text  then DisableExport else EnableExport Text,  enabled : exportSelected Text  }
                        ]

            menuButton { label, onClick, enabled } =
                HH.button
                    [ HE.onClick onClick
                    , HP.style $ "margin: 0 5px; padding: 3px 8px; cursor: pointer; border-radius: 10px;"
                        <> "background-color: " <> (if enabled then "#d0e7ff" else "white") <> ";"
                        <> (if enabled then "border: 1px solid beige; opacity: 0.8" else "border: 1px solid lightgray; opacity: 0.6;")
                    ]
                    [ HH.text label ]

            subjSelNavigation =
                HH.div
                    [ HP.style $ "position: fixed;right: 25%;top: 3em;border-radius: 5px;background: beige;padding: 5px;flex-direction: column;display: flex;text-align: end;font-size: 0.9em;"
                        -- <> if state.flags.subjectNavigationExpanded then "line-height: 1.6em;" else "line-height : 1.1em;"
                    , HE.onMouseEnter $ const $ if not state.flags.subjectNavigationPinned then ExpandSubjectNavigation   else NoOp
                    , HE.onMouseLeave $ const $ if not state.flags.subjectNavigationPinned then CollapseSubjectNavigation else NoOp
                    ]
                    $ subjNavigationItem <$> state.subjects

            subjNavigationItem subjId =
                let
                    uniqueId = (R.convertTo subjId :: String)
                    subjName = findSubjName subjId # fromMaybe "--"
                in HH.span
                    []
                    [ HH.a
                        [ HP.href $ "#subject-" <> uniqueId, HP.style "color: darkgoldenrod; text-decoration: none;"
                        ]
                        [ if state.flags.subjectNavigationExpanded || state.flags.subjectNavigationPinned
                            then HH.span
                                [ HP.style "color: black; margin-right: 5px; font-size: 0.7em; position: relative; top: -1px; margin-left: 4px; " ]
                                [ HH.text subjName ]
                            else HH.text ""
                        , HH.text "⦿"
                        ]
                    ]

            groupSelNavigation =
                if state.flags.groupNavigationPinned || state.flags.groupNavigationExpanded then
                    let
                        reportBuilder = R.toBuilder state.processedReport
                    in HH.div
                        [ HP.style $ "position: fixed;right: 25%;bottom:0;border-radius: 5px;background: beige;padding: 5px;flex-direction: column;display: flex;text-align: end;font-size: 0.6em; width: 15%; opacity: 0.8;max-height:400px;overflow:scroll;padding:9px;"
                        , HE.onMouseEnter $ const ExpandGroupNavigation
                        , HE.onMouseLeave $ const CollapseGroupNavigation
                        ]
                        $ HH.div [ HP.style "position:absolute;right:0;width:40px;" ]
                            [ groupSelNavigationPinButton ]
                        : (mapWithIndex groupsSelNavigationItems $ (\subjId -> subjId /\ RB.allGroupsOfCX subjId reportBuilder) <$> state.subjects)
                else
                    HH.div
                        [ HP.style $ "position: fixed;right: 25%;bottom:0;border-radius: 5px;background: beige;padding: 5px;flex-direction: column;display: flex;text-align: end;font-size: 0.6em; width: 15%; opacity: 0.5;max-height:2.4em;overflow:scroll;min-height: 2.4em;"
                            -- <> if state.flags.subjectNavigationExpanded then "line-height: 1.6em;" else "line-height : 1.1em;"
                        , HE.onMouseEnter $ const ExpandGroupNavigation
                        ]
                        [ HH.span [ HP.style "position:absolute;right:40px;top:10px;"] [ HH.text "GROUPS" ]
                        , HH.div [ HP.style "display: inline-block;position:absolute;right:0;width:40px;" ]
                            [ groupSelNavigationPinButton ]
                        ]

            groupsSelNavigationItems idx (subjId /\ groupsC) =
                let
                    subjName = findSubjName subjId # fromMaybe "--"
                    padding = if idx == 0 then "padding:0 0 5px 0;" else "padding:15px 0 5px 0;"
                in
                    HH.div
                        [ ]
                        $ HH.div [ HP.style $ "min-width:100%;text-align:start;font-weight:bold;" <> padding ] [ HH.text subjName ]
                        : (groupsSelNavigationChain {- <$> Chain.toArray <$> map R.g_title -} <$> groupsC)

            groupsSelNavigationChain groupC =
                let
                    chainLen = Chain.length groupC
                    -- thePadding = 5 + chainLen * 10
                    curGroupPath = R.g_path $ Chain.last groupC
                    renderGroupRep idx gtitle =
                        if idx == chainLen - 1
                        then HH.span [] [ HH.text gtitle ]
                        else HH.span [ HP.style "display: inline-block; width: 20px;opacity:0.3;" ] [ HH.text ">" ]

                in HH.div
                    [ HP.style $ "min-width:100%;text-align:start;" ]
                    $  (mapWithIndex renderGroupRep {- (HH.text >>> pure >>> HH.span []) -} <$> Chain.toArray $ R.g_title <$> groupC)
                    <> (pure $ HH.a [ HP.style "padding-left: 5px;text-decoration:none;", HP.href $ "#" <> GP.groupPathId curGroupPath ] [ HH.text "#" ])
                -- HH.div $
                --     Chain.toString <$> map R.g_title <$> groupsC
                --     HH.text $ (String.joinWith "-" $ show <$> Chain.length <$> groupsC ) <> ":" <> (String.joinWith "--" $ Chain.toString <$> map R.g_title <$> groupsC)

            groupSelNavigationPinButton =
                menuButton
                    { enabled : state.flags.groupNavigationPinned
                    , label   : if state.flags.groupNavigationPinned then "⚲" else "⌕" -- ⚘ 📌
                    , onClick : const ToggleGroupNavigationPinned
                    }


    subjectsToc state allSubjects =
        let
            filteredSubjects =
                applyFilter state.tagFilter state.sortBy state.filter allSubjects
        in HH.div
            [ HP.style "padding: 9px 9px 0 0; width: 400px;" ]
            $ filterInput state
            : optionsPane state
            : (HH.div
                    [ HP.style $ "overflow-y: scroll; height: 100%; position: absolute;" ]
                    $ subjTocRow state.subjects <$> filteredSubjects)
            : []

    makeTagFilter [] _ = true
    makeTagFilter tags subj = Array.any (flip Array.elem tags) (R.i_tags @subj_tag subj :: Array subj_tag)

    processingCount = _.process >>> Array.length

    loadFilterStr :: SubjectFilter -> Maybe String
    loadFilterStr = case _ of
        NoSubjectFilter -> Nothing
        FromUrl filterStr -> Just filterStr
        FromUser filterStr -> Just filterStr

    applyFilter :: Array subj_tag -> SubjectSort -> SubjectFilter -> Array subj -> Array subj
    applyFilter tagFilter sortBy subjFilter subjects
        = subjects
            # Array.filter (makeTagFilter tagFilter)
            # Array.filter (case loadFilterStr subjFilter of
                Just filterStr -> \subj -> String.contains (String.Pattern $ String.toLower filterStr) $ String.toLower $ R.s_name @subj_id subj
                Nothing -> const true)
            # Array.sortWith (sortSubjects sortBy)
            # (if sortBy == ByWeight then Array.reverse else identity)

    filterInput state =
        HH.div
            [ HP.style "" ]
            [ HH.input
                [ HE.onValueInput ChangeListFilter
                , HP.style "border: 1px solid lightgray; border-radius: 5px; padding: 5px 6px; margin-bottom: 15px; min-width: 250px;"
                , HP.placeholder $ case state.filter of
                    FromUrl str -> str
                    FromUser _ -> ""
                    NoSubjectFilter -> ""
                ]
            , HH.span
                [ HE.onClick $ const ToggleOptionsPane
                , HP.style "padding: 1px 3px 0 5px; cursor: pointer;"
                , HP.title $ if state.flags.optionsPaneExpanded then "Expand tags list" else "Collapse tags list"
                ]
                [ HH.text $ if state.flags.optionsPaneExpanded then "○" else "●" ]
            , HH.span
                [ HE.onClick $ const ToggleItemsTagsInOptions
                , HP.style $ "padding: 1px 3px 0 5px; cursor: pointer;" <> if state.flags.showItemsTags then "" {- }"font-weight: bold;" -} else "opacity: 0.3;"
                , HP.title $ if state.flags.showItemsTags then "Hide items tags" else "Show items tags"
                ]
                [ HH.text $ "I" ]
            , HH.span
                [ HE.onClick $ const NextSort
                , HP.style "padding: 1px 3px; cursor: pointer;"
                , HP.title $ "Sort " <> (case state.sortBy of
                    ByWeight -> "alphabetically"
                    Alpha -> "by weight")
                ]
                [ HH.text $ case state.sortBy of
                    ByWeight -> "W"
                    Alpha -> "A"
                ]
            ]

    optionsPane state =
        HH.div
            [ HP.style ""
            ] $
        if not state.flags.optionsPaneExpanded then
            ( case state.tagFilter of
                [] -> [ HH.text "" ]
                tags ->
                    subjTagWrap <$> tags
            )
            <>
            [ processingButtons state.process ]
        else
            [ processingButtons state.process ]
            <> (if state.flags.showItemsTags then
                    case R.collectItemsTags $ R.leaveOnlyById state.subjects $ state.processedReport of
                        [] -> [ HH.text "" ]
                        tags ->
                            [ HH.span
                                [ HP.style "display: block; padding: 5px 0; max-height: 300px; overflow-y: scroll;" ]
                                $ (\tag -> HH.span_ [ itemTagBadge (makeTagClickEvt tag) tag, HH.wbr [] ]) <$> tags
                            ]
                else []
                )
            <> (subjTagButton <$> subjTagIsOn state.tagFilter <$> (R.values :: Array subj_tag))

    processingButtons = case _ of
        [] -> HH.text ""
        procs -> HH.span [ HP.style "display: block; margin: 7px 4px;" ] $ processButton <$> procs

    processStyle = "background-color: rgb(139, 121, 182); color: white; border-radius: 5px; padding: 3px 5px; margin: 0 3px; cursor: pointer;"

    processButton = case _ of
        FilterBy tag    -> [ HH.text "F", itemTagBadge     (flip CancelProcess $ FilterBy tag)    tag     ]
        SortBy  tagKind -> [ HH.text "S", itemTagKindBadge (flip CancelProcess $ SortBy tagKind)  tagKind ]
        GroupBy tagKind -> [ HH.text "G", itemTagKindBadge (flip CancelProcess $ GroupBy tagKind) tagKind ]
        >>> HH.span [ HP.style processStyle ]

    subjTagIsOn tagFilter tag =
        tag /\ Array.elem tag tagFilter

    subjTagButton (tag /\ tagEnabled) =
        HH.span
            [ HE.onClick $ const $ if tagEnabled then ExcludeTag tag else IncludeTag tag
            , HP.style $ "display: inline-block; cursor: pointer; padding: 3px 0; opacity: " <> show (if tagEnabled then 1.0 else 0.5) <> ";"
            ]
            [ subjTagBadge tag ]

    sortSubjects :: SubjectSort -> subj -> SortKey
    sortSubjects = case _ of
        ByWeight -> R.i_stats >>> R.weightOf >>> SN
        Alpha ->    R.s_name @subj_id >>> SS

    subjTocRow selectedSubjectsIds subj =
        let
            subjId = R.s_id subj
            isSelected = Array.elem subjId selectedSubjectsIds
        in HH.div
            [ HP.style "margin: 5px 0;" ]
            [ HH.span
                [ HE.onClick $ const $ if isSelected then DeselectSubject subjId else AddSubjectToSelection subjId
                , HP.style "cursor: pointer;"
                ]
                [ HH.text $ if isSelected then "(-)" else "(+)"
                ]
            , HH.span
                [ HE.onClick $ const $ SelectSubject subjId
                , HP.style $ "background-color: " <> (if isSelected then "bisque" else "transparent") <> "; cursor: pointer; padding: 5px; margin-left: 5px;"
                ]
                (
                    [ HH.text $ R.s_name @subj_id subj
                    , case R.gotTotalFromStats $ R.i_stats subj of
                        R.Defined gotTotal -> gotTotalBadge gotTotal
                        _ -> HH.text ""
                    ] <>
                    (subjTagBadge <$> R.i_tags @subj_tag subj)
                )

            ]

    nextSort = case _ of
        ByWeight -> Alpha
        Alpha -> ByWeight

    stopMEPropagation :: forall s a sl o. MouseEvent -> H.HalogenM s a o sl m Unit
    stopMEPropagation mEvent = H.liftEffect $ Event.stopPropagation $ ME.toEvent mEvent

    stopKEPropagation :: forall s a sl o. KeyboardEvent -> H.HalogenM s a o sl m Unit
    stopKEPropagation kEvent = H.liftEffect $ Event.stopPropagation $ KE.toEvent kEvent

    clearCurrentActions = _ { navigatedTo = Navigation.clear }
    clearEditing s = s { navigatedTo = Navigation.clearEditing s.navigatedTo }

    focusOn :: forall s a sl o. H.RefLabel -> H.HalogenM s a o sl m Unit
    focusOn refLabel = do
        mElement <- H.getHTMLElementRef refLabel
        for_ mElement \element -> do
            H.liftEffect $ HTMLEvent.focus element

    focusOnReportArea :: forall s a sl o. H.HalogenM s a o sl m Unit
    focusOnReportArea = focusOn reportAreaRefLabel

    focusOnCurrentEditInput :: forall s a sl o. H.HalogenM s a o sl m Unit
    focusOnCurrentEditInput = focusOn EI.refLabelForEdit

    handleActionIfNotEditing :: _
    handleActionIfNotEditing action = do
        H.get >>= \s ->
            when (not s.flags.editingExportContent && (not $ Navigation.isEditing s.navigatedTo)) $
                handleAction action

    nextNavigation :: Location subj_id -> NavigatedTo subj_id
    nextNavigation = case _ of -- FIXME: now we have location stored in the data type, we don't need any proxy methods to case-switch it
        AtSubj subjId ->
                                    Navigation.toSubj subjId
        AtGroup subjId groupPath ->
                                    Navigation.toGroup subjId groupPath
        AtItem subjId groupPath itemIdx ->
                                    Navigation.toItem subjId groupPath itemIdx
        AtDecorator subjId groupPath itemIdx decoratorKey ->
                                    Navigation.toDecorator subjId groupPath itemIdx decoratorKey
        AtTag subjId groupPath itemIdx tagIdx ->
                                    Navigation.toTag subjId groupPath itemIdx tagIdx
        AtTabular subjId groupPath itemIdx tabularIdx ->
                                    Navigation.toTabular subjId groupPath itemIdx tabularIdx
        Nowhere ->
                                    Navigation.clear

    editNavigation :: Location subj_id -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item -> NavigatedTo subj_id
    editNavigation location s = case location of -- FIXME: now we have location stored in the data type, we don't need any proxy methods to case-switch it
        AtSubj subjId ->            Navigation.toSubj subjId
        AtGroup subjId groupPath -> Navigation.editGroupName subjId groupPath
                                    $ maybe (CT.EncodedValue "") (R.g_title >>> CT.EncodedValue)
                                    $ R.findGroup subjId groupPath s.sourceReport
        AtItem subjId groupPath itemIdx ->
                                    Navigation.editItemName subjId groupPath itemIdx
                                    $ maybe (CT.EncodedValue "") (R.i_title >>> CT.EncodedValue)
                                    $ R.findItem subjId groupPath itemIdx s.sourceReport
        AtDecorator subjId groupPath itemIdx decoratorKey ->
                                    Navigation.editDecorator subjId groupPath itemIdx decoratorKey
                                    $ maybe (CT.EncodedValue "") (Decorator.encodeDecorator >>> Tuple.snd)
                                    $ Decorator.get decoratorKey
                                    =<< R.i_decorators
                                    <$> R.findItem subjId groupPath itemIdx s.sourceReport
        AtTag subjId groupPath itemIdx tagIdx ->
                                    Navigation.editTag subjId groupPath itemIdx tagIdx
                                    $ maybe (CT.EncodedValue "") (R.i_tags @item_tag >>> flip Array.index tagIdx >>> map (R.tagContent >>> Chain.toString >>> CT.EncodedValue) >>> fromMaybe (CT.EncodedValue ""))
                                    $ R.findItem subjId groupPath itemIdx s.sourceReport
        AtTabular subjId groupPath itemIdx tabularIdx ->
                                    Navigation.editTag subjId groupPath itemIdx tabularIdx
                                    $ maybe (CT.EncodedValue "") (const $ CT.EncodedValue "") -- FIXME
                                    $ R.findItem subjId groupPath itemIdx s.sourceReport
        Nowhere -> s.navigatedTo

    handleAction
        :: ReportComponentAction subj_id subj_tag item_tag_kind item_tag subj group item
        -> ReportComponentM subj_id subj_tag item_tag_kind item_tag subj group item output m
    handleAction = case _ of
        Initialize -> do
            window <- H.liftEffect $ Web.window
            document <- H.liftEffect $ Window.document window

            handleAction HandleUrlChange
            -- handleAction HandleResize

            -- H.subscribe' \_ ->
            --     eventListener
            --         (E.EventType "resize")
            --         (Window.toEventTarget window)
            --         (E.target >=> (const $ Just HandleResize))

            {- H.subscribe' \_ ->
                eventListener
                    (E.EventType "hashchange")
                    (Window.toEventTarget window)
                    (E.target >=> (const $ Just HandleUrlChange)) -}

            H.subscribe' \_ ->
                eventListener
                    (E.EventType "popstate")
                    (Window.toEventTarget window)
                    (E.target >=> (const $ Just HandleUrlChange))

            H.subscribe' \sid ->
                eventListener
                    KET.keyup
                    (HTMLDocument.toEventTarget document)
                    (map HandleDocumentKey <<< KE.fromEvent)

            focusOnReportArea

            pure unit
        SelectSubject subjId -> H.modify_ _ { subjects = [ subjId ] } *>  H.modify_ updateProcessedReport *> updateUrl
        AddSubjectToSelection subjId -> H.modify_ (\state -> state { subjects = Array.snoc state.subjects subjId }) *> H.modify_ updateProcessedReport *> updateUrl
        DeselectSubject subjId -> H.modify_ (\state -> state { subjects = Array.filter (_ /= subjId) state.subjects }) *> H.modify_ updateProcessedReport *> updateUrl
        DeselectAllSubjects -> H.modify_ _ { subjects = [ ] } *> H.modify_ updateProcessedReport *> updateUrl
        Receive nextReport -> H.modify_ $ updateReports nextReport
        ChangeListFilter filter -> H.modify_ _ { filter = if String.length filter > 0 then FromUser filter else NoSubjectFilter } *> H.modify_ updateProcessedReport *> updateUrl
        IncludeTag subjTag -> H.modify_ (\state -> state { tagFilter = Array.snoc state.tagFilter subjTag }) *> updateUrl
        ExcludeTag subjTag -> H.modify_ (\state -> state { tagFilter = Array.filter (_ /= subjTag) state.tagFilter }) *> updateUrl
        ToggleOptionsPane -> H.modify_ $ withFlags \f -> f { optionsPaneExpanded = not f.optionsPaneExpanded }
        ToggleItemsTagsInOptions -> H.modify_ $ withFlags \f -> f { showItemsTags = not f.showItemsTags }
        NextSort -> H.modify_ \state -> state { sortBy = nextSort state.sortBy }
        ClearNavigation -> H.modify_ clearCurrentActions
        StartedEditingExportContent  -> H.modify_ $ withFlags _ { editingExportContent = true  }
        FinishedEditingExportContent -> H.modify_ $ withFlags _ { editingExportContent = false }

        NavigateByMouseClickTo mevt location ->
            let
                nextNavigation_ = nextNavigation location
                editNavigation_ = editNavigation location
                processedReport s = processingCount s > 0
                justNavigating s = (not $ processedReport s) && (s.flags.readOnlyMode || s.navigatedTo /= nextNavigation_)
                navigateOrEdit s =
                    if Navigation.isEditing s.navigatedTo then s else
                    -- this event is called on every click on any editable item,
                    -- so by checking `s.navigatedTo /= nextNavigation_` we ensure we did change location,
                    -- but if it stayed the same (and we're not in `readOnlyMode`),
                    -- it is a second mouse click on the same editable item, so we can toggle into editing
                    if justNavigating s
                        then s { navigatedTo = nextNavigation_ }
                        else s { navigatedTo = editNavigation_ s }
            in stopMEPropagation mevt <> do
                s <- H.get
                let editingValue = not $ justNavigating s
                H.modify_ navigateOrEdit
                when editingValue focusOnCurrentEditInput

        TryNavigateWithKeyboard kevt ->
            let
                navigateDir dir s =
                    s
                    { navigatedTo =
                        Navigation.withLocation
                            (Modify.move @subj_id @item_tag (s.processedReport :: R.Report subj group item) dir)
                        s.navigatedTo
                    }
                navigateUp = navigateDir Modify.Up
                navigateDown = navigateDir Modify.Down
                navigateLeft = navigateDir Modify.Left
                navigateRight = navigateDir Modify.Right
                keyStr = {- Debug.spy "key" $ -} KE.key kevt
                codeStr = {- Debug.spy "code" $ -} KE.code kevt
                peformNavigation s =
                    (not $ Navigation.isEditing s.navigatedTo)
                    && (keyStr == "ArrowUp" || keyStr == "ArrowDown" || keyStr == "ArrowLeft" || keyStr == "ArrowRight")
                navigateIfNotEditing s =
                    if peformNavigation s then
                        case keyStr of
                            "ArrowUp" -> navigateUp s
                            "ArrowDown" -> navigateDown s
                            "ArrowLeft" -> navigateLeft s
                            "ArrowRight" -> navigateRight s
                            _ -> s
                    else s
            in H.modify_ navigateIfNotEditing {- <> (H.get >>= \s -> when (peformNavigation s) $ stopKEPropagation kevt) -}

        HandleDocumentKey kevt | KE.key kevt == "e" -> do
            H.modify_ \s ->
                if s.flags.readOnlyMode || Navigation.isEditing s.navigatedTo then s
                else
                    let location = Navigation.toLocation s.navigatedTo in
                    s { navigatedTo = editNavigation location s }
            focusOnCurrentEditInput

        HandleDocumentKey kevt | KE.key kevt == "r" ->
            handleActionIfNotEditing ToggleReadOnlyMode

        HandleDocumentKey kevt | KE.key kevt == "d" ->
            handleActionIfNotEditing ToggleDebugMode

        HandleDocumentKey kevt | KE.key kevt == "p" ->
            handleActionIfNotEditing ToggleProgressPlates

        HandleDocumentKey kevt | KE.key kevt == "x" ->
            handleActionIfNotEditing $ EnableExport Rep

        HandleDocumentKey kevt | KE.key kevt == "f" ->
            handleActionIfNotEditing $ EnableExport Text

        HandleDocumentKey kevt | KE.key kevt == "q" ->
            handleActionIfNotEditing DisableExport

        HandleDocumentKey kevt | KE.key kevt == "s" ->
            handleActionIfNotEditing ToggleSubjectNavigationPinned

        HandleDocumentKey kevt | KE.key kevt == "g" ->
            handleActionIfNotEditing ToggleGroupNavigationPinned

        HandleDocumentKey _    | otherwise -> pure unit

        ApplyEditAt location encval ->
            let
                nextReport :: R.Report subj group item -> R.Report subj group item
                nextReport curReport
                    = case location of -- FIXME: ensure it's not the same as `Navigation2.toModification`
                        Nowhere -> curReport
                        AtSubj subjId -> curReport
                        AtGroup subjId groupPath ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at group"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.GroupName
                                    , newValue : encval
                                    }
                        AtItem subjId groupPath itemIdx ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at item"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemName itemIdx
                                    , newValue : encval
                                    }
                        AtDecorator subjId groupPath itemIdx decoratorKey ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at decorator"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemDecorator itemIdx decoratorKey
                                    , newValue : encval
                                    }
                        AtTag subjId groupPath itemIdx tagIdx ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at tag"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemTag itemIdx tagIdx
                                    , newValue : encval
                                    }
                        AtTabular subjId groupPath itemIdx tabularIdx ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at tabular"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemTabular itemIdx tabularIdx
                                    , newValue : encval
                                    }
            in
                H.modify_ \s ->
                    let theNextReport = nextReport s.sourceReport
                    in s
                        { sourceReport = theNextReport
                        , processedReport = -- TODO: this block repeats, look for `processedReport =`
                            R.toBuilder theNextReport
                            # RB.filterSubjects (R.s_id >>> flip Array.elem s.subjects)
                            # R.fromBuilder
                            # case cfg.recalculate.onEdit of
                                Just recCfg -> Modify.recalculate recCfg
                                Nothing -> identity
                        }

        StartEditing mevt -> stopMEPropagation mevt -- <> H.modify_ _ { editingValue = true }
        CancelEditing -> H.modify_ clearEditing <> focusOnReportArea
        ToggleReadOnlyMode -> do
            H.modify_ clearEditing
            s <- H.get
            H.modify_ $ withFlags \f -> f { readOnlyMode = if Array.length s.process > 0 then true else not f.readOnlyMode }
        ToggleDebugMode -> H.modify_ $ withFlags \f -> f { debugEnabled = not f.debugEnabled }
        ToggleProgressPlates -> H.modify_ $ withFlags \f -> f { progressPlatesShown = not f.progressPlatesShown }
        EnableExport exportTarget -> H.modify_ _ { mbExportTo = Just exportTarget }
        DisableExport -> H.modify_ _ { mbExportTo = Nothing }
        CollapseSubjectNavigation -> H.modify_ $ withFlags _ { subjectNavigationExpanded = false }
        ExpandSubjectNavigation   -> H.modify_ $ withFlags _ { subjectNavigationExpanded = true  }
        CollapseGroupNavigation   -> H.modify_ $ withFlags _ { groupNavigationExpanded   = false }
        ExpandGroupNavigation     -> H.modify_ $ withFlags _ { groupNavigationExpanded   = true  }
        ToggleSubjectNavigationPinned -> H.modify_ $ withFlags \f -> f { subjectNavigationPinned = not f.subjectNavigationPinned }
        ToggleGroupNavigationPinned   -> H.modify_ $ withFlags \f -> f { groupNavigationPinned   = not f.groupNavigationPinned   }
        AddToItemsFilter mevt itemTag -> do
            stopMEPropagation mevt
            H.modify_ (\s -> s { process = Array.snoc s.process $ FilterBy itemTag, collapsed = Map.empty })
            H.modify_ $ withFlags _ { readOnlyMode = true }
            H.modify_ updateProcessedReport
            updateUrl
        SortItemsBy  mevt itemTagKind -> do
            stopMEPropagation mevt
            H.modify_ $ \s -> s { process = Array.snoc s.process $ SortBy itemTagKind, collapsed = Map.empty }
            H.modify_ $ withFlags _ { readOnlyMode = true }
            H.modify_ updateProcessedReport
            updateUrl
        GroupItemsBy mevt itemTagKind -> do
            stopMEPropagation mevt
            H.modify_ $ \s -> s { process = Array.snoc s.process $ GroupBy itemTagKind, collapsed = Map.empty }
            H.modify_ $ withFlags _ { readOnlyMode = true }
            H.modify_ updateProcessedReport
            updateUrl
        CancelProcess mevt process -> do
            stopMEPropagation mevt
            s <- H.get
            let filteredProcess = Array.filter (_ /= process) s.process
            H.modify_ _ { process = filteredProcess, collapsed = Map.empty }
            H.modify_ $ withFlags \f -> f { readOnlyMode = if Array.length filteredProcess > 0 then true else f.readOnlyMode }
            H.modify_ updateProcessedReport
            updateUrl
        ResetPostProcess -> do
            H.modify_ _ { process = [], collapsed = Map.empty }
            H.modify_ updateProcessedReport
            updateUrl
        ToggleGroupCollapse mevt subjId groupPath -> H.modify_ \s ->
            let
                prevCollapsedStateOfThisSubj = fromMaybe Map.empty $ Map.lookup subjId $ s.collapsed
                groupWasCollapsedBefore = fromMaybe false $ Map.lookup groupPath $ prevCollapsedStateOfThisSubj
                groupShouldBeCollapsed = not groupWasCollapsedBefore
                probablyCollapseSubGroupsAsWell subjCollapseMap =
                    if ME.shiftKey mevt && groupShouldBeCollapsed then
                        foldl
                            (\theMap group ->
                                if GP.startsWithNotEq groupPath $ R.g_path group
                                    then Map.insert (R.g_path group) true theMap
                                    else theMap)
                            subjCollapseMap
                            $ RB.allGroupsOf subjId
                            $ R.toBuilder s.processedReport
                    else subjCollapseMap
                newCollapsedStateOfThisSubj =
                    prevCollapsedStateOfThisSubj
                        # Map.insert groupPath groupShouldBeCollapsed
                        # probablyCollapseSubGroupsAsWell
                newCollapsedState = s.collapsed # Map.insert subjId newCollapsedStateOfThisSubj
            in s { collapsed = newCollapsedState }
        HandleUrlChange -> do
            state <- H.get

            nextHashCfg <- H.liftEffect $ do

                window <- Web.window
                loc <- Window.location window
                search <- Location.search loc

                let curCfg = collectUrlConfig state
                let nextCfg = UC.loadFromUrl curCfg $ UC.fromUrlPairs search

                Console.log search
                Console.log $ show $ unwrap nextCfg
                pure nextCfg

            H.modify_ $ loadUrlConfig @item_tag_kind nextHashCfg

            H.modify_ updateProcessedReport

        StoreContentToImport format content ->
            H.modify_ \s -> s { importFrom = s.importFrom # Map.insert format content }

        TryImportFromCurrent -> do
            mbCurrentFormat <- H.get <#> _.mbExportTo
            whenJust mbCurrentFormat $ handleAction <<< TryImportFrom

        TryImportFrom format -> do
            s <- H.get
            let mbContent = Map.lookup format s.importFrom
            whenJust mbContent $ \content -> do
                liftEffect $ Console.log $ show format <> "::" <> content
                when (importSupportedFor format) $ do
                    let
                        eNextReport =
                            case format of
                                Rep -> Report.fromRep @x @subj_id @subj_tag @item_tag content
                                _   -> Left $ UnsupportedFormat format
                    case eNextReport of
                        Right theNextReport -> do
                            liftEffect $ Console.log $ "APPLY CHANGE IN " <> show format
                    -- let theNextReport = nextReport s.sourceReport
                            H.modify_ _
                                { sourceReport = theNextReport
                                , processedReport =
                                    R.toBuilder theNextReport
                                    # RB.filterSubjects (R.s_id >>> flip Array.elem s.subjects)
                                    # R.fromBuilder
                                    # case cfg.recalculate.onEdit of
                                        Just recCfg -> Modify.recalculate recCfg
                                        Nothing -> identity
                                }
                        Left theError ->
                            liftEffect $ Console.log $ printImportError theError
                    liftEffect $ Console.log $ "TRY IMPORT"

        NoOp -> pure unit


type SubjectRenderingOptions =
    { progressPlatesShown :: Boolean
    }


renderSubject
    :: forall @subj_id @subj_tag @item_tag_kind @item_tag subj group item slots m
     . Eq subj_id
    => Ord subj_id
    => R.IsTag subj_tag
    => R.IsTag item_tag
    => R.IsSortable item_tag_kind item_tag
    => R.IsItem item
    => R.IsGroup group
    => R.IsSubjectId subj_id subj
    => R.IsSubject subj_id subj
    => R.HasDecorators item
    => R.HasTabular item
    => R.HasTabular subj
    => R.HasStats group
    => R.HasTags subj_tag subj
    => R.HasTags item_tag item
    => SubjectRenderingOptions
    -> NavigatedTo subj_id
    -> CollapseMap subj_id
    -> subj
    -> Array (group /\ Array item)
    -> HH.ComponentHTML (ReportComponentAction subj_id subj_tag item_tag_kind item_tag subj group item) slots m
renderSubject options navigatedTo collapsedMap subj groupsArr =
    HH.div
        [ HP.style "padding: 10px 0 10px 20px;"
        , HP.id $ "subject-" <> subjUniqueId
        ]
        $ HH.div
            [ HP.style $ "margin: 15px 0 30px 0; max-width: 60%; border-bottom: 1px solid gray; padding-bottom: 5px; font-size: 1.2em;"
                <> if isNavigatedToSubj then subjSelectedStyle else subjUsualStyle
            , HE.onClick $ const ClearNavigation
            ]
            [ HH.text $ R.s_name @subj_id subj
            , HH.span [ HP.style "font-size: 0.8em; margin-left: 5px;" ] $ pure $ renderSubjTags (R.i_tags @subj_tag subj)
            ]
        : (const NoOp <$> renderSubjectTabularValues subj)
        : (renderTree <$> groupsArr)
        where
            subjId = R.s_id subj
            subjUniqueId = R.convertTo @String subjId
            isNavigatedToSubj = navigatedTo # Navigation.atSubj subjId
            marginFor groupPath = (max 0.0 $ (Int.toNumber $ GP.howDeep groupPath) - 1.0) * nestMargin
            subjSelectedStyle = "background-color: cornsilk;" -- ghostwhite
            subjUsualStyle = ""
            groupSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
            groupUsualStyle = "border: 1px dashed transparent;"

            renderSubjTags :: Array subj_tag -> _
            renderSubjTags subj_tags = HH.span [] $ subjTagWrap <$> subj_tags

            renderTree (group /\ groupItems) =
                let
                    -- group = Chain.last groupC
                    groupPath = R.g_path group
                    groupStats = R.i_stats group
                    isNavigatedTo = navigatedTo # Navigation.atGroup subjId groupPath
                    groupCollapsed = fromMaybe Map.empty $ Map.lookup subjId collapsedMap
                    isCollapsed = fromMaybe false $ Map.lookup groupPath groupCollapsed
               in HH.div
                    [ HP.style $ "padding-bottom: 10px; line-height: "
                        <> show lineHeight <> "em; margin-left: "
                        <> (show $ marginFor groupPath) <> "px;"
                        -- <> (if isNavigatedTo then " background-color: #2e9eff;" else "")
                    , HP.id $ groupPathId groupPath
                    , HE.onClick $ \mevt -> NavigateByMouseClickTo mevt $ AtGroup subjId groupPath
                    ]
                    [ HH.div [ HP.style "display: flex; flex-direction: row;" ]
                        [ HH.div [ {- HP.style "display: flex; flex-direction: column;" -} ]
                            [ HH.div_
                                [ HH.span
                                    [ HP.style $ "font-weight: bold;"
                                        <> if isNavigatedTo then groupSelectedStyle else groupUsualStyle
                                    ]
                                    [ HH.span
                                        [ HE.onClick \mevt -> ToggleGroupCollapse mevt subjId groupPath
                                        , HP.style $ "cursor: pointer; user-select: none; margin-right: 5px;" <> if isCollapsed then "" else "opacity: 0.25;" -- 0.6?
                                        ]
                                        [ HH.text $ if isCollapsed then "▶" else "▼" ]
                                    , HH.text $ R.g_title group
                                    ]
                                , qspacerSpan
                                , renderPath groupPath
                                ]
                            -- , HH.text (show group.mbIndexPath)
                            -- , qspacerSpan
                            , HH.div_ $ pure $ renderGroupStats groupStats
                            ]
                        , case groupStats of
                                R.SWithProgress _ (Just itemsProgress) ->
                                    if options.progressPlatesShown then renderProgressPlates itemsProgress
                                    else HH.text ""
                                _ -> HH.text ""
                        ]
                    , if isCollapsed
                        then HH.text ""
                        else HH.div
                            [ HP.style "border-left: 4px solid #eee; padding-left: 5px; margin-top: 10px; margin-bottom: 15px;" ]
                            $ mapWithIndex (renderGroupItem groupPath) groupItems

                  ]

            renderGroupItem groupPath itemIdx item =
                let
                    itemTitle = R.i_title item
                    isNavigatedToItem = navigatedTo # Navigation.atItem subjId groupPath itemIdx
                    isNavigatedToItemName = navigatedTo # Navigation.atItemName subjId groupPath itemIdx
                    isEditingItemName = navigatedTo # Navigation.editingItemName subjId groupPath itemIdx
                    isEditingDecorator decorator = navigatedTo # Navigation.editingAtDecorator subjId groupPath itemIdx decorator
                    mbCurrentDecorator = if isNavigatedToItem then (Navigation._toNavigationRec navigatedTo).mbDecorator else Nothing
                    itemSelectedStyle = "background-color: #f0f8ff; border-radius: 3px;"
                    itemUsualStyle = "background-color: transparent;"
                    -- itemTitleSelectedStyle = "background-color: #fff8ff; border-radius: 3px;"
                    itemTitleSelectedStyle = "background-color: #fbd6fb; border-radius: 3px;"
                    itemTitleUsualStyle = ""
                    makeItemNameEditEvt = ApplyEditAt $ AtItem subjId groupPath itemIdx
                    makeDecoratorClickEvt decoratorKey mevt = NavigateByMouseClickTo mevt $ AtDecorator subjId groupPath itemIdx decoratorKey
                    makeDecoratorEditEvt decoratorKey = ApplyEditAt $ AtDecorator subjId groupPath itemIdx decoratorKey
                    -- itemSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
                    -- itemUsualStyle = "border: 1px dashed transparent;"
                    itemDecorators = R.i_decorators item
                    hasPrefixes = Array.length (Decorator.prefixes itemDecorators) > 0
                    hasSuffixes = Array.length (Decorator.suffixes itemDecorators) > 0
                    voeItemName = maybe
                        CT.view
                        CT.edit
                        isEditingItemName
                        itemTitle
                            <#> \name -> { itemName : name }
                    editingName = CT.isEditing voeItemName
                    { itemName } = CT.loadViewOrEdit voeItemName
                    ititleEvents =
                        { onClick : const NoOp -- FIXME: TODO
                        , onEdit : const NoOp -- FIXME: TODO
                        , onStartEditing  : StartEditing
                        , onCancelEditing : CancelEditing
                        , noop : NoOp
                        , onEditItemName : makeItemNameEditEvt
                        }
                    -- mkEditTitleInput :: forall a w i. CT.ViewOrEdit a -> H w i
                    mkEditTitleInput = EI.mkValueEditInput ititleEvents ititleEvents.onEditItemName
                    qitemnameSpan color =
                        if not editingName
                            then qcolorSpan color itemName
                            else mkEditTitleInput $ _.itemName <$> voeItemName
                    renderDecoratorsConfig =
                        { mbSelectedDecorator : mbCurrentDecorator
                        , isEditingDecorator
                        , isEditingItemName
                        , onClick : makeDecoratorClickEvt
                        , onEdit : makeDecoratorEditEvt
                        , onEditItemName : makeItemNameEditEvt
                        -- , onTagClick : makeTagClickEvt
                        , onStartEditing  : StartEditing
                        , onCancelEditing : CancelEditing
                        , noop : NoOp
                        }
                    tagsRenderConfig =
                        { isEditingTags : Nothing
                        , isSelected : false  -- FIXME: TODO
                        , onTagClick : makeTagClickEvt
                        , onClick : const NoOp -- FIXME: TODO
                        , onEdit : const NoOp -- FIXME: TODO
                        , onStartEditing : StartEditing
                        , onCancelEditing : CancelEditing
                        , noop : NoOp
                        }
                    (pvStates /\ renderedPrefixes) = renderPrefixes renderDecoratorsConfig item
                    (svStates /\ renderedSuffixes) = renderSuffixes renderDecoratorsConfig item
                    inlinePrefixes = Array.catMaybes $ IoB.loadInlineContent <$> renderedPrefixes
                    inlineSuffixes = Array.catMaybes $ IoB.loadInlineContent <$> renderedSuffixes
                    selectedVState = VStates.selectOne $ unwrap $ SemigroupMap pvStates <> SemigroupMap svStates
                    itemNameColor = VStates.itemNameColor selectedVState
                    blockDecorators =
                        Array.concat
                        $ Array.catMaybes
                        $ IoB.loadBlockContent
                        <$>
                        (renderedPrefixes <> renderedSuffixes)
                in HH.div
                    [ HP.style
                        $ if isNavigatedToItem then itemSelectedStyle else itemUsualStyle
                    , HE.onClick $ \mevt -> NavigateByMouseClickTo mevt $ AtItem subjId groupPath itemIdx
                    ]
                    $ HH.span_ (Array.intersperse qspacerSpan inlinePrefixes)
                    : case itemTitle of
                        "" -> HH.text ""
                        _ ->
                            HH.span
                                [ HP.style
                                    $ if isNavigatedToItemName then itemTitleSelectedStyle else itemTitleUsualStyle
                                ]
                                [ if hasPrefixes then qspacerSpan else qemptySpan
                                , qitemmarkerSpan itemNameColor
                                , qspacerSpan
                                , qitemnameSpan itemNameColor
                                , if hasSuffixes then qspacerSpan else qemptySpan
                                ]
                    : HH.span_ (Array.intersperse qspacerSpan inlineSuffixes)
                    : HH.span_
                        (pure
                            $ renderTags (wrap $ R.i_tags @item_tag item)
                            $ tagsRenderConfig
                        )
                    : HH.div_ blockDecorators
                    : pure (const NoOp <$> renderItemTabularValues item)


-- TODO: remove
navigationHint :: forall @subj subj_id w i. R.IsSubject subj_id subj => NavigatedTo subj_id -> HH.HTML w i
navigationHint navigation =
    let
        navigationHintStyle = "position: fixed; border: 1px solid black; background-color: #ffffe0ff; padding: 5px 10px; bottom: -5px; left: 60px; max-width: 70%; font-size: 0.7em; box-shadow: 2px 2px 5px gray; border-radius: 5px; overflow: hidden;"
        hintIfEditing Nothing = HH.text ""
        hintIfEditing (Just (CT.EncodedValue val )) = HH.text $ "E:" <> show val
        showSubjId = R.convertTo @String @subj_id
        hintText = case Navigation.toLocation navigation of
            Nowhere -> "-"
            AtSubj subjId -> "S: " <> showSubjId subjId
            AtGroup subjId groupPath -> "G: " <> showSubjId subjId <> " / " <> show groupPath
            AtItem subjId groupPath itemIdx -> "I: " <> showSubjId subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ]"
            AtDecorator subjId groupPath itemIdx decoratorKey -> "X: " <> showSubjId subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ] . " <> Decorator.debugNavLabel decoratorKey
            AtTag subjId groupPath itemIdx tagIdx -> "T: " <> showSubjId subjId <> " / " <> show groupPath <> " [ " <> show tagIdx <> " ]"
            AtTabular subjId groupPath itemIdx tabularIdx -> "V: " <> showSubjId subjId <> " / " <> show groupPath <> " [ " <> show tabularIdx <> " ]"

    in case Navigation.toLocation navigation of
        Nowhere -> HH.text ""
        _ ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text hintText
                , qspacerSpan
                , hintIfEditing $ Navigation.mbEditing navigation
                ]


postProcess
    :: forall @item_tag_kind item_tag subj group item
     . Eq item_tag
    => Ord item_tag
    => Ord group
    => R.IsSortable item_tag_kind item_tag
    => R.IsGroupable group item_tag
    => R.HasTags item_tag item
    => R.HasDecorators item
    => Modify.StatsModify group
    => CRB.RecalcBehavior
    -> Array (Process item_tag_kind item_tag)
    -> R.Report subj group item
    -> R.Report subj group item
postProcess recalc processes report =
    case processes of
        [] -> case recalc.onEmpty of
                Just config -> Modify.recalculate config report
                Nothing -> report
        _  -> foldl applyProcess report processes
    where
        applyProcess curReport process = case process of
            FilterBy itemTag ->
                R.filterItemsByTag itemTag curReport
                    # case recalc.onFilter of
                        Just config -> Modify.recalculate config
                        Nothing -> identity
            SortBy itemTagKind ->
                R.sortItemsByKind @item_tag itemTagKind curReport
            GroupBy itemTagKind ->
                R.groupItemsByKind @item_tag itemTagKind curReport
                    # case recalc.onRegroup of
                        Just config -> Modify.recalculate config
                        Nothing -> identity
                    -- Modify.recalculate @item_tag $ nextReport s.report


whichProcess :: forall @item_tag_kind item_tag. R.IsSortable item_tag_kind item_tag => item_tag -> MouseEvent -> Maybe (Process item_tag_kind item_tag)
whichProcess itemTag mevt =
    if ME.shiftKey mevt && not (ME.altKey mevt || ME.metaKey mevt) then
        Just $ FilterBy itemTag
    else if (not $ ME.shiftKey mevt) && (ME.altKey mevt || ME.metaKey mevt) then
        Just $ SortBy $ R.kindOf itemTag
    else if (ME.ctrlKey mevt || (ME.shiftKey mevt && (ME.altKey mevt || ME.metaKey mevt))) then
        Just $ GroupBy $ R.kindOf itemTag
    else Nothing


makeTagClickEvt :: forall subj_id subj_tag @item_tag_kind item_tag x. R.IsSortable item_tag_kind item_tag => item_tag -> MouseEvent -> Action subj_id subj_tag item_tag_kind item_tag x
makeTagClickEvt tag mevt =
    case whichProcess tag mevt of
        Just (FilterBy itemTag)     -> AddToItemsFilter mevt itemTag
        Just (SortBy itemTagKind)   -> SortItemsBy mevt itemTagKind
        Just (GroupBy itemTagKind)  -> GroupItemsBy mevt itemTagKind
        Nothing -> NoOp


newtype ComponentURLConfig
    = ComponentURLConfig
    { subjIdFilter :: Array String
    , subjFilter :: Maybe String
    , subjAlphaSort :: Boolean
    , subjTagFilter :: Array String
    , itemTagFilter :: Array String
    , itemTagKindSorting :: Array String
    , itemTagKindGrouping :: Array String
    }


derive instance Newtype ComponentURLConfig _


instance UC.UrlConfig ComponentURLConfig where
    default :: ComponentURLConfig
    default = ComponentURLConfig
        { subjIdFilter : []
        , subjFilter : Nothing
        , subjAlphaSort : false
        , subjTagFilter : []
        , itemTagFilter : []
        , itemTagKindSorting : []
        , itemTagKindGrouping : []
        }
    writeToUrl :: ComponentURLConfig -> UC.ParamMap
    writeToUrl (ComponentURLConfig cfg) =
        UC.emptyParams
            # UC.insertIf (not $ Array.null cfg.subjIdFilter)        "sif"  (String.joinWith "," cfg.subjIdFilter)
            # UC.insertWhenJust                                      "sf"   cfg.subjFilter
            # UC.insertIf (cfg.subjAlphaSort)                        "sort" "alpha"
            # UC.insertIf (not $ Array.null cfg.subjTagFilter)       "stf"  (String.joinWith "," cfg.subjTagFilter)
            # UC.insertIf (not $ Array.null cfg.itemTagFilter)       "itf"  (String.joinWith "," cfg.itemTagFilter)
            # UC.insertIf (not $ Array.null cfg.itemTagKindSorting)  "itks" (String.joinWith "," cfg.itemTagKindSorting)
            # UC.insertIf (not $ Array.null cfg.itemTagKindGrouping) "itkg" (String.joinWith "," cfg.itemTagKindGrouping)
    loadFromUrl :: ComponentURLConfig -> UC.ParamMap -> ComponentURLConfig
    loadFromUrl _ paramMap =
        ComponentURLConfig
            { subjIdFilter        : paramMap # UC.lookup "sif"  # arrayFromMaybe
            , subjFilter          : paramMap # UC.lookup "sf"
            , subjAlphaSort       : paramMap # UC.lookup "sort" # (maybe false $ const true)
            , subjTagFilter       : paramMap # UC.lookup "stf"  # arrayFromMaybe
            , itemTagFilter       : paramMap # UC.lookup "itf"  # arrayFromMaybe
            , itemTagKindSorting  : paramMap # UC.lookup "itks" # arrayFromMaybe
            , itemTagKindGrouping : paramMap # UC.lookup "itkg" # arrayFromMaybe
            }
        where
            arrayFromMaybe = maybe [] (String.split $ String.Pattern ",")


updateUrl
    :: forall subj_id subj_tag item_tag_kind item_tag subj group item output m
     . MonadEffect m
    => R.IsSubjectId subj_id subj
    => R.ConvertTo (Chain String) subj_tag
    => R.ConvertTo (Chain String) item_tag
    => R.ConvertTo (Chain String) item_tag_kind
    -- => R.IsTag subj_tag
    -- => R.IsTag item_tag
    => R.IsSortable item_tag_kind item_tag
    => ReportComponentM subj_id subj_tag item_tag_kind item_tag subj group item output m
updateUrl = H.get >>= \state ->
    liftEffect $ do
      window <- Web.window
      history <- Window.history window
      hstate <- History.state history
      let nextHashMap = UC.writeToUrl $ collectUrlConfig state
      let url = History.URL $ "?" <> UC.toUrlPairs nextHashMap
      let title = History.DocumentTitle ""
      history # History.pushState hstate title url


collectUrlConfig
    :: forall subj_id subj_tag item_tag_kind item_tag subj group item
     . R.IsSubjectId subj_id subj
    => R.ConvertTo (Chain String) subj_tag
    => R.ConvertTo (Chain String) item_tag
    => R.ConvertTo (Chain String) item_tag_kind
    -- => R.IsTag subj_tag
    -- => R.IsTag item_tag
    -- => R.IsSortable item_tag_kind item_tag
    => ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
    -> ComponentURLConfig
collectUrlConfig state =
    ComponentURLConfig
        { subjIdFilter        : R.convertTo @String @subj_id <$> state.subjects
        , subjFilter          : loadFilterContent state.filter
        , subjAlphaSort       : loadAlphaSort state.sortBy
        , subjTagFilter       : Chain.toString <$> R.convertTo <$> state.tagFilter
        , itemTagFilter       : Array.mapMaybe extractFilterBy state.process
        , itemTagKindSorting  : Array.mapMaybe extractSortBy   state.process
        , itemTagKindGrouping : Array.mapMaybe extractGroupBy  state.process
        }
        where
            loadAlphaSort = case _ of
                Alpha -> true
                _ -> false
            loadFilterContent = case _ of
                NoSubjectFilter -> Nothing
                FromUrl content -> Just content
                FromUser content -> Just content
            extractFilterBy = case _ of
                FilterBy tag -> Just $ Chain.toString $ R.convertTo tag
                _ -> Nothing
            extractSortBy = case _ of
                SortBy tagKind -> Just $ Chain.toString $ R.convertTo tagKind
                _ -> Nothing
            extractGroupBy = case _ of
                GroupBy tagKind -> Just $ Chain.toString $ R.convertTo tagKind
                _ -> Nothing


loadUrlConfig
    :: forall subj_id subj_tag @item_tag_kind item_tag subj group item
     . R.IsSubjectId subj_id subj
    => R.ConvertFrom (Chain String) subj_tag
    => R.ConvertFrom (Chain String) item_tag
    => R.ConvertFrom (Chain String) item_tag_kind
    => R.IsSortable item_tag_kind item_tag
    => ComponentURLConfig
    -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
    -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
loadUrlConfig (ComponentURLConfig cfg) = _
    { filter    = maybe NoSubjectFilter FromUrl cfg.subjFilter
    , sortBy    = if cfg.subjAlphaSort then Alpha else ByWeight
    , subjects  = Array.catMaybes $ R.convertFrom @String @subj_id <$> cfg.subjIdFilter
    , tagFilter = Array.catMaybes $ R.convertFrom <$> (Array.catMaybes $ Chain.fromString <$> cfg.subjTagFilter)
    , process   = Array.catMaybes $
            (map FilterBy <$> (Chain.fromString >>> flip bind R.convertFrom) {- R.decodeTag @item_tag -}  <$> cfg.itemTagFilter)
            <>
            (map GroupBy  <$> (Chain.fromString >>> flip bind R.convertFrom) <$> cfg.itemTagKindGrouping)
            <>
            (map SortBy   <$> (Chain.fromString >>> flip bind R.convertFrom) <$> cfg.itemTagKindSorting)
    }
    -- where
    --     mkAction action tag = { action, tag }


withFlags
    :: forall subj_id subj_tag item_tag_kind item_tag subj group item
     . (Flags -> Flags)
    -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
    -> ReportComponentState subj_id subj_tag item_tag_kind item_tag subj group item
withFlags chFlag s =
    let newFlags = chFlag s.flags in
    s { flags = newFlags }


importSupportedFor :: ReportFormat -> Boolean
importSupportedFor = case _ of
    Rep -> true
    -- Org -> true
    _ -> false