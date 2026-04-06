module Report.Web.Component where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Debug as Debug

import Data.Array ((:))
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe (isJust) as Maybe
import Data.Set as Set
import Data.String (length, contains, toLower, joinWith, split, Pattern(..)) as String
import Data.Tuple (uncurry, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, wrap, unwrap)

import Yoga.JSON (writePrettyJSON, class WriteForeign, class ReadForeign)

import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent, shiftKey, altKey, metaKey, ctrlKey) as ME

import Web.HTML (window) as Web
import Web.HTML.Location (hash, setHash, search, setSearch) as Location
import Web.HTML.Window (toEventTarget, innerWidth, innerHeight, location, history) as Window
import Web.HTML.History (URL(..), DocumentTitle(..), state, pushState) as History
import Web.Event.Event as E

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)

import Report as R
import Report.Builder as RB
import Report.Class as R
import Report.Chain (toString) as Chain
-- import Report.Core.Logic (EncodedValue(..))
import Report.Core.Logic (EncodedValue(..), view, edit, isEditing, loadViewOrEdit, ViewOrEdit) as CT
import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep, startsWithNotEq) as GP
import Report.Decorator (get, put, debugNavLabel, prefixes, suffixes) as Decorator
import Report.Decorator (size, keys, collectProgress, hasProgress) as Decorators
import Report.Decorators.Tags (TagAction(..))
import Report.Decorators.Stats (GotTotal(..), gotTotalFromStats, weightOf) as R
import Report.Decorators.Class.ValueModify as VModify
import Report.Decorators.Stats.Collect as Collect
import Report.Modify (Location(..))
import Report.Modify as Modify
import Report.Convert.Text.Decorator (encodeDecorator) as Decorator
import Report.Convert.Generic (class ToExport, includeOnly) as Report
import Report.Convert.Json (toJson) as Report
import Report.Convert.Dhall (toDhall) as Report
import Report.Convert.Org (toOrg) as Report

import Report.Web.GroupPath (groupPathId, renderPath)
import Report.Web.Helpers (qspacerSpan, qcolorSpan, qitemmarkerSpan, lineHeight, nestMargin, qemptySpan, H)
import Report.Web.Helpers.InlineOrBlock as IoB
import Report.Web.Helpers.UrlConfig as UC
import Report.Web.Decorators.Stats (renderGroupStats, gotTotalBadge)
import Report.Web.Decorators.Tags (subjTagBadge, subjTagWrap, itemTagBadge, itemTagKindBadge)
import Report.Web.Decorators.EditInput as EI
import Report.Web.Navigation (NavigatedTo)
import Report.Web.Navigation as Navigation
import Report.Web.Decorators (renderPrefixes, renderSuffixes, renderTags)
import Report.Web.Helpers.VisualState (selectOne, itemNameColor) as VStates
import Report.Web.Tabular (renderSubjectTabularValues, renderItemTabularValues)
import Report.Web.Component.RecalcBehavior as CRB


type Process item_tag = { action :: TagAction, tag :: item_tag }


type CollapseMap subj_id = Map subj_id (Map GroupPath Boolean)


data SubjectFilter
    = NoSubjectFilter
    | FromUser String
    | FromUrl String


type State subj_id subj_tag item_tag report =
    { subjects :: Array subj_id
    , report :: report
    , filter :: SubjectFilter
    , tagFilter :: Array subj_tag
    , optionsPaneExpanded :: Boolean
    , showItemsTags :: Boolean
    , sortBy :: SubjectSort
    , readOnlyMode :: Boolean
    , debugEnabled :: Boolean
    , showSubjectNavNames :: Boolean
    , mbExportTo :: Maybe ExportTarget
    , navigatedTo :: NavigatedTo subj_id
    , process :: Array (Process item_tag)
    , collapsed :: CollapseMap subj_id
    }


type Input subj group item =
    R.Report subj group item


data Action subj_id subj_tag item_tag report
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
    | NavigateTo MouseEvent (Location subj_id)
    | EditAt (Location subj_id) CT.EncodedValue
    | StartEditing MouseEvent
    | CancelEditing
    | ToggleReadOnlyMode
    | ToggleDebugMode
    | TurnSubjectNavNamesOn
    | TurnSubjectNavNamesOff
    | EnableExport ExportTarget
    | DisableExport
    | AddToItemsFilter MouseEvent item_tag
    | CancelProcess MouseEvent (Process item_tag)
    | SortItemsBy MouseEvent item_tag
    | GroupItemsBy MouseEvent item_tag
    | ResetPostProcess
    | ToggleGroupCollapse MouseEvent subj_id GroupPath
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


data ExportTarget
    = Json
    | Dhall
    | Org


derive instance Eq ExportTarget


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
    ( R.IsTag subj_tag
    , R.IsTag item_tag
    , R.IsItem item
    , R.IsGroup group
    , R.IsSubject subj_id subj
    , R.IsSortable item_tag
    , R.IsGroupable group item_tag
    )
    <= Is subj_id subj_tag item_tag subj group item (x :: Type)


instance
    ( R.IsTag subj_tag
    , R.IsTag item_tag
    , R.IsItem item
    , R.IsGroup group
    , R.IsSubject subj_id subj
    , R.IsSortable item_tag
    , R.IsGroupable group item_tag
    ) =>
    Is subj_id subj_tag item_tag subj group item (R.Report subj group item)


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


type ReportComponentState subj_id subj_tag item_tag subj group item =
    State subj_id subj_tag item_tag (R.Report subj group item)

type ReportComponentAction subj_id subj_tag item_tag subj group item =
    Action subj_id subj_tag item_tag (Input subj group item)

type ReportComponentM subj_id subj_tag item_tag subj group item output m =
    H.HalogenM
        (ReportComponentState subj_id subj_tag item_tag subj group item)
        (ReportComponentAction subj_id subj_tag item_tag subj group item)
        ()
        output
        m
        Unit


component
    :: forall @x @subj_id @subj_tag @item_tag @subj @group @item query output m
     . MonadEffect m
    => Ord subj_id
    => Ord item_tag
    => Show subj_id
    => Is subj_id subj_tag item_tag subj group item x
    => Has subj_tag item_tag subj group item x
    => Modify item_tag group item x
    -- => VModify.EncodableKey subj_id
    -- => ReadForeign item_tag
    -- => WriteForeign item_tag
    -- => WriteForeign subj_tag
    => Report.ToExport subj_id subj_tag item_tag subj group item x
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
    initialState :: x -> ReportComponentState subj_id subj_tag item_tag subj group item
    initialState x =
        { subjects : cfg.preSelected
        , report : R.toReport x
        , filter : NoSubjectFilter
        , tagFilter : []
        , optionsPaneExpanded : true
        , showItemsTags : true
        , sortBy : ByWeight
        , readOnlyMode : true
        , debugEnabled : false
        , showSubjectNavNames : false
        , mbExportTo : Nothing
        , navigatedTo : Navigation.init
        , process : []
        , collapsed : Map.empty
        }

    s_id :: subj -> subj_id
    s_id subj = R.s_id subj

    render :: ReportComponentState subj_id subj_tag item_tag subj group item -> HH.ComponentHTML (ReportComponentAction subj_id subj_tag item_tag subj group item) () m
    render state =
        HH.div
            [ HP.style "font-family: \"JetBrains Mono\", sans-serif; display: flex; flex-direction: row;" ]
            [ HH.div
                [ HP.style "height: 100vh; min-width: 75%; overflow-y: scroll;"
                -- , HE.onClick $ const ClearNavigation
                ]
                $
                ( Tuple.uncurry (renderSubject @subj_id @subj_tag @item_tag state.navigatedTo state.collapsed)
                    <$> selectedSubjects
                )
                <> pure menuButtons
                <> pure subjSelNavigation
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
                            [ HP.style "white-space: pre-wrap; background: transparent; border: none; font-size: 0.7em;"
                            , HP.value $ exportTextFor exportTarget, HP.cols 75, HP.rows 30
                            ]
                        ]
                Nothing -> HH.text ""
            , if state.debugEnabled then navigationHint state.navigatedTo else HH.text ""
            ]
        where

            processedReport =
                R.toBuilder state.report
                # RB.filterSubjects (s_id >>> flip Array.elem state.subjects)
                # R.fromBuilder
                # postProcess cfg.recalculate state.process
            selectedSubjects = processedReport # R.unfoldAll
            allSubjects = RB.allSubjects $ R.toBuilder state.report
            selKeys = Map.fromFoldable $ (\subj -> s_id subj /\ subj) <$> allSubjects

            includeRule = Report.includeOnly state.subjects

            exportTextFor = case _ of
                Json  -> state.report # Report.toJson  @x @subj_id @subj_tag @item_tag includeRule
                Dhall -> state.report # Report.toDhall @x @subj_id @subj_tag @item_tag includeRule
                Org   -> state.report # Report.toOrg   @x @subj_id @subj_tag @item_tag includeRule
            exportSelected trg = state.mbExportTo == Just trg

            findSubjName :: subj_id -> Maybe String
            findSubjName subjId = Map.lookup subjId selKeys <#> R.s_name @subj_id

            menuButtons =
                HH.div
                    [ HP.style "position: fixed; right: 25%; top: 0; border-radius: 5px; background: aliceblue; padding: 5px;" ]
                    $ menuButton <$>
                        (if not state.readOnlyMode && Navigation.isEditing state.navigatedTo then
                            [ { label : "✎", onClick : const CancelEditing, enabled : true } ]
                        else
                            [ ]
                        )
                        <>
                        [ { label : if state.readOnlyMode then "🔒" else "🔓", onClick : const ToggleReadOnlyMode, enabled : state.readOnlyMode }
                        , { label : if state.debugEnabled then "🛠" else "🛠", onClick : const ToggleDebugMode,    enabled : state.debugEnabled }
                        , { label : "JSON",  onClick : const $ if exportSelected Json  then DisableExport else EnableExport Json,  enabled : exportSelected Json  }
                        , { label : "DHALL", onClick : const $ if exportSelected Dhall then DisableExport else EnableExport Dhall, enabled : exportSelected Dhall }
                        , { label : "ORG",   onClick : const $ if exportSelected Org   then DisableExport else EnableExport Org,   enabled : exportSelected Org   }
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
                        -- <> if state.showSubjectNavNames then "line-height: 1.6em;" else "line-height : 1.1em;"
                    ]
                    $ subjNavigationItem <$> state.subjects

            subjNavigationItem subjId =
                let
                    uniqueId = R.s_unique @subj_id @subj subjId
                    subjName = findSubjName subjId # fromMaybe "--"
                in HH.span
                    []
                    [ HH.a
                        [ HP.href $ "#subject-" <> uniqueId, HP.style "color: darkgoldenrod; text-decoration: none;"
                        , HE.onMouseEnter $ const TurnSubjectNavNamesOn
                        , HE.onMouseLeave $ const TurnSubjectNavNamesOff
                        ]
                        [ if state.showSubjectNavNames
                            then HH.span
                                [ HP.style "color: black; margin-right: 5px; font-size: 0.7em; position: relative; top: -1px; margin-left: 4px; " ]
                                [ HH.text subjName ]
                            else HH.text ""
                        , HH.text "⦿"
                        ]
                    ]

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

    applyFilter :: Array subj_tag -> SubjectSort -> SubjectFilter-> Array subj -> Array subj
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
                , HP.title $ if state.optionsPaneExpanded then "Expand tags list" else "Collapse tags list"
                ]
                [ HH.text $ if state.optionsPaneExpanded then "○" else "●" ]
            , HH.span
                [ HE.onClick $ const ToggleItemsTagsInOptions
                , HP.style $ "padding: 1px 3px 0 5px; cursor: pointer;" <> if state.showItemsTags then "" {- }"font-weight: bold;" -} else "opacity: 0.3;"
                , HP.title $ if state.showItemsTags then "Hide items tags" else "Show items tags"
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
        if not state.optionsPaneExpanded then
            ( case state.tagFilter of
                [] -> [ HH.text "" ]
                tags ->
                    subjTagWrap <$> tags
            )
            <>
            [ processingButtons state.process ]
        else
            [ processingButtons state.process ]
            <> (if state.showItemsTags then
                    case R.collectItemsTags $ R.leaveOnlyById state.subjects $ state.report of
                        [] -> [ HH.text "" ]
                        tags ->
                            [ HH.span
                                [ HP.style "display: block; padding: 5px 0; max-height: 300px; overflow-y: scroll;" ]
                                $ (\tag -> HH.span_ [ itemTagBadge (makeTagClickEvt tag) tag, HH.wbr [] ]) <$> tags
                            ]
                else []
                )
            <> (subjTagButton <$> subjTagIsOn state.tagFilter <$> (R.allTags :: Array subj_tag))

    processingButtons = case _ of
        [] -> HH.text ""
        procs -> HH.span [ HP.style "display: block; margin: 7px 4px;" ] $ processButton <$> procs

    processStyle = "background-color: rgb(139, 121, 182); color: white; border-radius: 5px; padding: 3px 5px; margin: 0 3px; cursor: pointer;"

    processButton = case _ of
        { action: FilterBy, tag } -> [ HH.text "F", itemTagKindBadge (flip CancelProcess { action: FilterBy, tag }) tag ]
        { action: SortBy, tag }   -> [ HH.text "S", itemTagKindBadge (flip CancelProcess { action: SortBy,   tag }) tag ]
        { action: GroupBy, tag }  -> [ HH.text "G", itemTagKindBadge (flip CancelProcess { action: GroupBy,  tag }) tag ]
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
            subjId = s_id subj
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

    stopPropagation :: forall s a sl o. MouseEvent -> H.HalogenM s a o sl m Unit
    stopPropagation mEvent = H.liftEffect $ Event.stopPropagation $ ME.toEvent mEvent

    clearCurrentActions = _ { navigatedTo = Navigation.clear }
    clearEditing s = s { navigatedTo = Navigation.clearEditing s.navigatedTo }

    handleAction
        :: ReportComponentAction subj_id subj_tag item_tag subj group item
        -> ReportComponentM subj_id subj_tag item_tag subj group item output m
    handleAction = case _ of
        Initialize -> do
            window <- H.liftEffect $ Web.window

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

            pure unit
        SelectSubject subjId -> H.modify_ _ { subjects = [ subjId ] } *> updateUrl
        AddSubjectToSelection subjId -> H.modify_ (\state -> state { subjects = Array.snoc state.subjects subjId }) *> updateUrl
        DeselectSubject subjId -> H.modify_ (\state -> state { subjects = Array.filter (_ /= subjId) state.subjects }) *> updateUrl
        DeselectAllSubjects -> H.modify_ _ { subjects = [ ] } *> updateUrl
        Receive nextReport -> H.modify_ _ { report = nextReport }
        ChangeListFilter filter -> H.modify_ _ { filter = if String.length filter > 0 then FromUser filter else NoSubjectFilter } *> updateUrl
        IncludeTag subjTag -> H.modify_ (\state -> state { tagFilter = Array.snoc state.tagFilter subjTag }) *> updateUrl
        ExcludeTag subjTag -> H.modify_ (\state -> state { tagFilter = Array.filter (_ /= subjTag) state.tagFilter }) *> updateUrl
        ToggleOptionsPane -> H.modify_ \state -> state { optionsPaneExpanded = not state.optionsPaneExpanded }
        ToggleItemsTagsInOptions -> H.modify_ \state -> state { showItemsTags = not state.showItemsTags }
        NextSort -> H.modify_ \state -> state { sortBy = nextSort state.sortBy }
        ClearNavigation -> H.modify_ clearCurrentActions

        NavigateTo mevt location ->
            let
                nextNavigation   = case location of
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
                editNavigation s = case location of
                    AtGroup subjId groupPath -> Navigation.editGroupName subjId groupPath
                                                $ maybe (CT.EncodedValue "") (R.g_title >>> CT.EncodedValue)
                                                $ R.findGroup subjId groupPath s.report
                    AtItem subjId groupPath itemIdx ->
                                                Navigation.editItemName subjId groupPath itemIdx
                                                $ maybe (CT.EncodedValue "") (R.i_title >>> CT.EncodedValue)
                                                $ R.findItem subjId groupPath itemIdx s.report
                    AtDecorator subjId groupPath itemIdx decoratorKey ->
                                                Navigation.editDecorator subjId groupPath itemIdx decoratorKey
                                                $ maybe (CT.EncodedValue "") (Decorator.encodeDecorator >>> Tuple.snd)
                                                $ Decorator.get decoratorKey
                                                =<< R.i_decorators
                                                <$> R.findItem subjId groupPath itemIdx s.report
                    AtTag subjId groupPath itemIdx tagIdx ->
                                                Navigation.editTag subjId groupPath itemIdx tagIdx
                                                $ maybe (CT.EncodedValue "") (R.i_tags @item_tag >>> flip Array.index tagIdx >>> map (R.tagContent >>> Chain.toString >>> CT.EncodedValue) >>> fromMaybe (CT.EncodedValue ""))
                                                $ R.findItem subjId groupPath itemIdx s.report
                    AtTabular subjId groupPath itemIdx tabularIdx ->
                                                Navigation.editTag subjId groupPath itemIdx tabularIdx
                                                $ maybe (CT.EncodedValue "") (const $ CT.EncodedValue "") -- FIXME
                                                $ R.findItem subjId groupPath itemIdx s.report
                    Nowhere -> s.navigatedTo
                processedReport s = processingCount s > 0
                navigateOrEdit s =
                    if Navigation.isEditing s.navigatedTo then s else
                    if s.readOnlyMode || s.navigatedTo /= nextNavigation
                        then s { navigatedTo = nextNavigation }
                        else
                            if not (processedReport s)
                            then s { navigatedTo = editNavigation s }
                            else s
            in stopPropagation mevt <> H.modify_ navigateOrEdit

        EditAt location encval ->
            let
                nextReport :: R.Report subj group item -> R.Report subj group item
                nextReport curReport
                    = case location of
                        Nowhere -> curReport
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
                case cfg.recalculate.onEdit of
                    Just recCfg ->
                        H.modify_
                            \s -> s { report = Modify.recalculate recCfg $ nextReport s.report }
                    Nothing ->
                        H.modify_
                            \s -> s { report = nextReport s.report }

        StartEditing mevt -> stopPropagation mevt -- <> H.modify_ _ { editingValue = true }
        CancelEditing -> H.modify_ clearEditing
        ToggleReadOnlyMode ->
            H.modify_ clearEditing <> H.modify_ \s -> s { readOnlyMode = not s.readOnlyMode }
        ToggleDebugMode -> H.modify_ \s -> s { debugEnabled = not s.debugEnabled }
        EnableExport exportTarget -> H.modify_ _ { mbExportTo = Just exportTarget }
        DisableExport -> H.modify_ _ { mbExportTo = Nothing }
        TurnSubjectNavNamesOff -> H.modify_ \s -> s { showSubjectNavNames = false }
        TurnSubjectNavNamesOn  -> H.modify_ \s -> s { showSubjectNavNames = true }
        AddToItemsFilter mevt itemTag -> stopPropagation mevt <> H.modify_ (\s -> s { process = Array.snoc s.process { action: FilterBy, tag : itemTag }, readOnlyMode = true }) <> updateUrl
        SortItemsBy mevt itemTag ->      stopPropagation mevt <> H.modify_ (\s -> s { process = Array.snoc s.process { action: SortBy,   tag : itemTag }, readOnlyMode = true }) <> updateUrl
        GroupItemsBy mevt itemTag ->     stopPropagation mevt <> H.modify_ (\s -> s { process = Array.snoc s.process { action: GroupBy,  tag : itemTag }, readOnlyMode = true }) <> updateUrl
        CancelProcess mevt process -> stopPropagation mevt <>
            H.modify_ (\s ->
                let filteredProcess = Array.filter (_ /= process) s.process in
                s { process = filteredProcess, readOnlyMode = if Array.length filteredProcess > 0 then true else s.readOnlyMode }
            )
            <> updateUrl
        ResetPostProcess -> H.modify_ _ { process = [] } <> updateUrl
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
                            $ R.toBuilder s.report
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

            H.modify_ $ loadUrlConfig nextHashCfg

        NoOp -> pure unit


renderSubject
    :: forall @subj_id @subj_tag @item_tag subj group item slots m
     . Eq subj_id
    => Ord subj_id
    => R.IsTag item_tag
    => R.IsTag subj_tag
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
    => NavigatedTo subj_id
    -> CollapseMap subj_id
    -> subj
    -> Array (group /\ Array item)
    -> HH.ComponentHTML (ReportComponentAction subj_id subj_tag item_tag subj group item) slots m
renderSubject navigatedTo collapsedMap subj groupsArr =
    HH.div
        [ HP.style "padding: 10px 0 10px 20px;"
        , HP.id $ "subject-" <> subjUniqueId
        ]
        $ HH.div
            [ HP.style "margin: 15px 0 30px 0; max-width: 60%; border-bottom: 1px solid gray; padding-bottom: 5px; font-size: 1.2em;"
            , HE.onClick $ const ClearNavigation
            ]
            [ HH.text $ R.s_name @subj_id subj
            , HH.span [ HP.style "font-size: 0.8em; margin-left: 5px;" ] $ pure $ renderSubjTags (R.i_tags @subj_tag subj)
            ]
        : (const NoOp <$> renderSubjectTabularValues subj)
        : (renderTree <$> groupsArr)
        where
            subjId = R.s_id subj
            subjUniqueId = R.s_unique @subj_id @subj subjId
            marginFor groupPath = (max 0.0 $ (Int.toNumber $ GP.howDeep groupPath) - 1.0) * nestMargin
            groupSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
            groupUsualStyle = "border: 1px dashed transparent;"

            renderSubjTags :: Array subj_tag -> _
            renderSubjTags subj_tags = HH.span [] $ subjTagWrap <$> subj_tags

            renderTree (group /\ groupItems) =
                let
                    groupPath = R.g_path group
                    groupStats = R.i_stats group
                    isNavigatedTo = navigatedTo # Navigation.atGroup subjId groupPath
                    groupCollapsed = fromMaybe Map.empty $ Map.lookup subjId collapsedMap
                    isCollapsed = fromMaybe false $ Map.lookup groupPath groupCollapsed
               in HH.div
                    [ HP.style $ "padding-bottom: 10px; line-height: "
                        <> show lineHeight <> "em; margin-left: "
                        <> (show $ marginFor groupPath) <> "px;"
                        -- <> (if isNavigatedTo then " background-color: #f0f8ff;" else "")
                    , HP.id $ groupPathId groupPath
                    , HE.onClick $ \mevt -> NavigateTo mevt $ AtGroup subjId groupPath
                    ]
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
                    -- , HH.text (show group.mbIndexPath)
                    , qspacerSpan
                    , renderGroupStats groupStats
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
                    isEditingItemName = navigatedTo # Navigation.editingItemName subjId groupPath itemIdx
                    isEditingDecorator decorator = navigatedTo # Navigation.editingAtDecorator subjId groupPath itemIdx decorator
                    mbCurrentDecorator = if isNavigatedToItem then navigatedTo.mbDecorator else Nothing
                    itemSelectedStyle = "background-color: #f0f8ff; border-radius: 3px;"
                    itemUsualStyle = "background-color: transparent;"
                    makeItemNameEditEvt = EditAt $ AtItem subjId groupPath itemIdx
                    makeDecoratorClickEvt decoratorKey mevt = NavigateTo mevt $ AtDecorator subjId groupPath itemIdx decoratorKey
                    makeDecoratorEditEvt decoratorKey = EditAt $ AtDecorator subjId groupPath itemIdx decoratorKey
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
                    , HE.onClick $ \mevt -> NavigateTo mevt $ AtItem subjId groupPath itemIdx
                    ]
                    $ HH.span_ (Array.intersperse qspacerSpan inlinePrefixes)
                    : case itemTitle of
                        "" -> HH.text ""
                        _ ->
                            HH.span_
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
navigationHint :: forall subj_id w i. Show subj_id => NavigatedTo subj_id -> HH.HTML w i
navigationHint navigation =
    let
        navigationHintStyle = "position: fixed; border: 1px solid black; background-color: #ffffe0ff; padding: 5px 10px; bottom: -5px; left: 60px; max-width: 70%; font-size: 0.7em; box-shadow: 2px 2px 5px gray; border-radius: 5px; overflow: hidden;"
        hintIfEditing Nothing = HH.text ""
        hintIfEditing (Just { value : CT.EncodedValue val }) = HH.text $ "E:" <> show val
        hintText = case Navigation.toLocation navigation of
            Nowhere -> "-"
            AtGroup subjId groupPath -> "G: " <> show subjId <> " / " <> show groupPath
            AtItem subjId groupPath itemIdx -> "I: " <> show subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ]"
            AtDecorator subjId groupPath itemIdx decoratorKey -> "X: " <> show subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ] . " <> Decorator.debugNavLabel decoratorKey
            AtTag subjId groupPath itemIdx tagIdx -> "T: " <> show subjId <> " / " <> show groupPath <> " [ " <> show tagIdx <> " ]"
            AtTabular subjId groupPath itemIdx tabularIdx -> "V: " <> show subjId <> " / " <> show groupPath <> " [ " <> show tabularIdx <> " ]"

    in case Navigation.toLocation navigation of
        Nowhere -> HH.text ""
        _ ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text hintText
                , qspacerSpan
                , hintIfEditing navigation.mbEditing
                ]


postProcess
    :: forall item_tag subj group item
     . Eq item_tag
    => Ord item_tag
    => Ord group
    => R.IsSortable item_tag
    => R.IsGroupable group item_tag
    => R.HasTags item_tag item
    => R.HasDecorators item
    => Modify.StatsModify group
    => CRB.RecalcBehavior
    -> Array (Process item_tag)
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
            { action : FilterBy, tag : itemTag } ->
                R.filterItemsByTag itemTag curReport
                    # case recalc.onFilter of
                        Just config -> Modify.recalculate config
                        Nothing -> identity
            { action : SortBy, tag : itemTag } ->
                R.sortItemsByTag itemTag curReport
            { action : GroupBy, tag : itemTag } ->
                R.groupItemsByTag itemTag curReport
                    # case recalc.onRegroup of
                        Just config -> Modify.recalculate config
                        Nothing -> identity
                    -- Modify.recalculate @item_tag $ nextReport s.report


whichProcess :: forall item_tag. item_tag -> MouseEvent -> Maybe (Process item_tag)
whichProcess itemTag mevt =
    if ME.shiftKey mevt && not (ME.altKey mevt || ME.metaKey mevt) then
        Just { action: FilterBy, tag: itemTag }
    else if (not $ ME.shiftKey mevt) && (ME.altKey mevt || ME.metaKey mevt) then
        Just { action: SortBy, tag: itemTag }
    else if (ME.ctrlKey mevt || (ME.shiftKey mevt && (ME.altKey mevt || ME.metaKey mevt))) then
        Just { action: GroupBy, tag: itemTag }
    else Nothing


makeTagClickEvt :: forall subj_id subj_tag item_tag x. item_tag -> MouseEvent -> Action subj_id subj_tag item_tag x
makeTagClickEvt tag mevt =
    case whichProcess tag mevt of
        Just { action: FilterBy, tag: itemTag } -> AddToItemsFilter mevt itemTag
        Just { action: SortBy, tag: itemTag }   -> SortItemsBy mevt itemTag
        Just { action: GroupBy, tag: itemTag }  -> GroupItemsBy mevt itemTag
        Nothing -> NoOp


newtype ComponentURLConfig
    = ComponentURLConfig
    { subjIdFilter :: Array String
    , subjFilter :: Maybe String
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
            # UC.insertIf (not $ Array.null cfg.subjTagFilter)       "stf"  (String.joinWith "," cfg.subjTagFilter)
            # UC.insertIf (not $ Array.null cfg.itemTagFilter)       "itf"  (String.joinWith "," cfg.itemTagFilter)
            # UC.insertIf (not $ Array.null cfg.itemTagKindSorting)  "itks" (String.joinWith "," cfg.itemTagKindSorting)
            # UC.insertIf (not $ Array.null cfg.itemTagKindGrouping) "itkg" (String.joinWith "," cfg.itemTagKindGrouping)
    loadFromUrl :: ComponentURLConfig -> UC.ParamMap -> ComponentURLConfig
    loadFromUrl _ paramMap =
        ComponentURLConfig
            { subjIdFilter        : paramMap # UC.lookup "sif"  # arrayFromMaybe
            , subjFilter          : paramMap # UC.lookup "sf"
            , subjTagFilter       : paramMap # UC.lookup "stf"  # arrayFromMaybe
            , itemTagFilter       : paramMap # UC.lookup "itf"  # arrayFromMaybe
            , itemTagKindSorting :  paramMap # UC.lookup "itks" # arrayFromMaybe
            , itemTagKindGrouping : paramMap # UC.lookup "itkg" # arrayFromMaybe
            }
        where
            arrayFromMaybe = maybe [] (String.split $ String.Pattern ",")


updateUrl
    :: forall subj_id subj_tag item_tag subj group item output m
     . MonadEffect m
    => R.IsSubjectId subj_id subj
    => R.IsTag subj_tag
    => R.IsTag item_tag
    => R.IsSortable item_tag
    => ReportComponentM subj_id subj_tag item_tag subj group item output m
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
    :: forall subj_id subj_tag item_tag subj group item
     . R.IsSubjectId subj_id subj
    => R.IsTag subj_tag
    => R.IsTag item_tag
    => R.IsSortable item_tag
    => ReportComponentState subj_id subj_tag item_tag subj group item
    -> ComponentURLConfig
collectUrlConfig state =
    ComponentURLConfig
        { subjIdFilter        : R.s_unique @subj_id @subj <$> state.subjects
        , subjFilter          : loadFilterContent state.filter
        , subjTagFilter       : R.encodeTag @subj_tag <$> state.tagFilter
        , itemTagFilter       : Array.mapMaybe (actionToTag FilterBy)    state.process
        , itemTagKindSorting  : Array.mapMaybe (actionToTagKind SortBy)  state.process
        , itemTagKindGrouping : Array.mapMaybe (actionToTagKind GroupBy) state.process
        }
        where
            loadFilterContent = case _ of
                NoSubjectFilter -> Nothing
                FromUrl content -> Just content
                FromUser content -> Just content
            actionToTag cmp { action, tag }     = if cmp == action then Just $ R.encodeTag tag else Nothing
            actionToTagKind cmp { action, tag } = if cmp == action then Just $ R.kindId tag    else Nothing


loadUrlConfig
    :: forall subj_id subj_tag item_tag subj group item
     . R.IsSubjectId subj_id subj
    => R.IsTag subj_tag
    => R.IsTag item_tag
    => R.IsSortable item_tag
    => ComponentURLConfig
    -> ReportComponentState subj_id subj_tag item_tag subj group item
    -> ReportComponentState subj_id subj_tag item_tag subj group item
loadUrlConfig (ComponentURLConfig cfg) = _
    { filter    = maybe NoSubjectFilter FromUrl cfg.subjFilter
    , subjects  = Debug.spy "subjIDs" $ Array.catMaybes $ R.s_decode @subj_id @subj <$> cfg.subjIdFilter
    , tagFilter = Array.catMaybes $ R.decodeTag @subj_tag     <$> cfg.subjTagFilter
    , process   = Array.catMaybes $
            (map (mkAction FilterBy) <$> R.decodeTag  @item_tag <$> cfg.itemTagFilter)
            <>
            (map (mkAction GroupBy)  <$> R.fromKindId @item_tag <$> cfg.itemTagKindGrouping)
            <>
            (map (mkAction SortBy)   <$> R.fromKindId @item_tag <$> cfg.itemTagKindSorting)
    }
    where
        mkAction action tag = { action, tag }