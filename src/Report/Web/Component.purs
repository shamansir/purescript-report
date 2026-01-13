module Report.Web.Component where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Array ((:))
import Data.Array (length, snoc, catMaybes, elem, filter, sortWith, reverse, any) as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.String (length, contains, toLower, Pattern(..)) as String
import Data.Tuple (uncurry, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.JSON (writePrettyJSON, class WriteForeign, class ReadForeign)

import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as ME

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Report (Report, toMap, findGroup, findItem, withItem, withGroup, TransferMap, class ToReport) as R
import Report.Class as R
import Report.Core.Logic (EncodedValue(..))
import Report.Convert.Text.Prefix (encodePrefix) as Prefix
import Report.Convert.Text.Suffix (encodeSuffix) as Suffix
import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep) as GP
import Report.Modifiers.Stats (GotTotal(..), gotTotalFromStats, weightOf) as R
import Report.Modify (Location(..))
import Report.Modify as Modify
import Report.Prefix (get, put, debugNavLabel) as Prefix
import Report.Suffix (get, put, debugNavLabel) as Suffix
import Report.Modifiers.Class.ValueModify as VModify
import Report.Convert.Generic (class ToExport, includeOnly) as Report
import Report.Convert.Json (toJson) as Report
import Report.Convert.Dhall (toDhall) as Report
import Report.Convert.Org (toOrg) as Report

import Report.Web.GroupPath (groupPathId, renderPath)
import Report.Web.Helpers (qspacerSpan, lineHeight, nestMargin)
import Report.Web.Modifiers.Stats (renderGroupStats, gotTotalBadge)
import Report.Web.Modifiers.Tags (subjTagBadge, subjTagWrap)
import Report.Web.Navigation (NavigatedTo)
import Report.Web.Navigation as Navigation
import Report.Web.Prefix (renderPrefixes)
import Report.Web.Suffix (renderSuffixes)
import Report.Web.Tabular (renderSubjectTabularValues, renderItemTabularValues)


type State subj_id subj_tag report =
    { subjects :: Array subj_id
    , report :: report
    , filter :: Maybe String
    , tagFilter :: Array subj_tag
    , optionsPaneExpanded :: Boolean
    , sortBy :: SubjectSort
    , readOnlyMode :: Boolean
    , debugEnabled :: Boolean
    , showSubjectNavNames :: Boolean
    , mbExportTo :: Maybe ExportTarget
    , navigatedTo :: NavigatedTo subj_id
    -- , TODO: collapsed :: Map subj_id (Map GroupPath Boolean)
    }


type Input subj group item = R.Report subj group item


data Action subj_id subj_tag report
    = Receive report
    | SelectSubject subj_id
    | AddSubjectToSelection subj_id
    | DeselectSubject subj_id
    | DeselectAllSubjects
    | ChangeListFilter String
    | IncludeTag subj_tag
    | ExcludeTag subj_tag
    | ToggleOptionsPane
    | NextSort
    | ClearNavigation
    | NavigateTo MouseEvent (Location subj_id)
    | EditAt (Location subj_id) EncodedValue
    | StartEditing MouseEvent
    | CancelEditing
    | ToggleReadOnlyMode
    | ToggleDebugMode
    | TurnSubjectNavNamesOn
    | TurnSubjectNavNamesOff
    | EnableExport ExportTarget
    | DisableExport
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



class
    ( R.IsTag subj_tag
    , R.IsTag item_tag
    , R.IsItem item
    , R.IsGroup group
    , R.IsSubject subj_id subj
    )
    <= Is subj_id subj_tag item_tag subj group item (x :: Type)


instance
    ( R.IsTag subj_tag
    , R.IsTag item_tag
    , R.IsItem item
    , R.IsGroup group
    , R.IsSubject subj_id subj
    ) =>
    Is subj_id subj_tag item_tag subj group item (R.Report subj group item)


class
    ( R.HasPrefixes item
    , R.HasSuffixes item_tag item
    , R.HasTabular item
    , R.HasTags subj_tag subj
    , R.HasStats subj
    , R.HasTabular subj
    -- , R.HasPrefixes subj
    -- , R.HasSuffixes subj_tag subj
    , R.HasStats group
    )
    <= Has subj_tag item_tag subj group item (x :: Type)


instance
    ( R.HasPrefixes item
    , R.HasSuffixes item_tag item
    , R.HasTabular item
    , R.HasTags subj_tag subj
    , R.HasStats subj
    , R.HasTabular subj
    -- , R.HasPrefixes subj
    -- , R.HasSuffixes subj_tag subj
    , R.HasStats group
    ) =>
    Has subj_tag item_tag subj group item (R.Report subj group item)


class
    ( Modify.GroupModify group
    , Modify.StatsModify group
    , Modify.SuffixesModify item_tag item
    , Modify.PrefixesModify item
    , Modify.ItemModify item
    )
    <= Modify item_tag group item (x :: Type)


instance
    ( Modify.GroupModify group
    , Modify.StatsModify group
    , Modify.SuffixesModify item_tag item
    , Modify.PrefixesModify item
    , Modify.ItemModify item
    ) =>
    Modify item_tag group item (R.Report subj group item)


component
    :: forall @x @subj_id @subj_tag @item_tag subj group item query output m
     . MonadEffect m
    => Ord subj
    => Ord subj_id
    => Ord group
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
    => Array subj_id
    -> H.Component query (Input subj group item) output m
component preSelected =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
        }
    where
    initialState :: Input subj group item -> State subj_id subj_tag (R.Report subj group item)
    initialState report =
        { subjects : preSelected
        , report
        , filter : Nothing
        , tagFilter : []
        , optionsPaneExpanded : true
        , sortBy : ByWeight
        , readOnlyMode : true
        , debugEnabled : false
        , showSubjectNavNames : false
        , mbExportTo : Nothing
        , navigatedTo : Navigation.init
        }

    s_id :: subj -> subj_id
    s_id subj = R.s_id subj

    selectionKeyToSubject :: Array subj -> Map subj_id subj
    selectionKeyToSubject = map (\subj -> s_id subj /\ subj) >>> Map.fromFoldable


    selectedSubjects :: R.TransferMap subj group item -> Map subj_id subj -> Array subj_id -> Array (subj /\ Map group (Array item))
    selectedSubjects report selKeys subjects = Array.catMaybes $ (\selId -> Map.lookup selId selKeys >>= (\subj -> Map.lookup subj report <#> (/\) subj)) <$> subjects

    render :: State subj_id subj_tag (R.Report subj group item) -> HH.ComponentHTML (Action subj_id subj_tag (Input subj group item)) () m
    render state =
        HH.div
            [ HP.style "font-family: \"JetBrains Mono\", sans-serif; display: flex; flex-direction: row;" ]
            [ HH.div
                [ HP.style "height: 100vh; min-width: 75%; overflow-y: scroll;"
                -- , HE.onClick $ const ClearNavigation
                ]
                $
                ( Tuple.uncurry (renderSubject @subj_id @subj_tag @item_tag state.navigatedTo)
                    <$> selectedSubjects report selKeys state.subjects
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

            report = R.toMap state.report
            allSubjects = Set.toUnfoldable $ Map.keys report
            selKeys = selectionKeyToSubject allSubjects

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
        HH.div
            [ HP.style "padding: 9px 9px 0 0; width: 400px;" ]
            $ filterInput state
            : optionsPane state
            : (HH.div
                    [ HP.style $ "overflow-y: scroll; height: 100%; position: absolute;" ]
                    $ subjTocRow state.subjects <$> (applyFilter state.tagFilter state.sortBy state.filter) allSubjects)
            : []

    makeTagFilter [] _ = true
    makeTagFilter tags subj = Array.any (flip Array.elem tags) (R.i_tags @subj_tag subj :: Array subj_tag)

    applyFilter tagFilter sortBy mbFilter subjects
        = subjects
            # Array.filter (makeTagFilter tagFilter)
            # Array.filter (case mbFilter of
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
                ]
            , HH.span
                [ HE.onClick $ const ToggleOptionsPane
                , HP.style "padding: 1px 3px 0 5px; cursor: pointer;"
                , HP.title $ if state.optionsPaneExpanded then "Expand tags list" else "Collapse tags list"
                ]
                [ HH.text $ if state.optionsPaneExpanded then "○" else "●" ]
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
            case state.tagFilter of
                [] -> [ HH.text "" ]
                tags ->
                    subjTagWrap <$> tags
        else
            subjTagButton <$> subjTagIsOn state.tagFilter <$> (R.allTags :: Array subj_tag)

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
        :: Action subj_id subj_tag (Input subj group item)
        -> H.HalogenM
            (State subj_id subj_tag (R.Report subj group item))
            (Action subj_id subj_tag (Input subj group item))
            ()
            output
            m
            Unit
    handleAction = case _ of
        SelectSubject subjId -> H.modify_ _ { subjects = [ subjId ] }
        AddSubjectToSelection subjId -> H.modify_ \state -> state { subjects = Array.snoc state.subjects subjId }
        DeselectSubject subjId -> H.modify_ \state -> state { subjects = Array.filter (_ /= subjId) state.subjects }
        DeselectAllSubjects -> H.modify_ _ { subjects = [ ] }
        Receive nextReport -> H.modify_ _ { report = nextReport }
        ChangeListFilter filter -> H.modify_ _ { filter = if String.length filter > 0 then Just filter else Nothing }
        IncludeTag subjTag -> H.modify_ \state -> state { tagFilter = Array.snoc state.tagFilter subjTag }
        ExcludeTag subjTag -> H.modify_ \state -> state { tagFilter = Array.filter (_ /= subjTag) state.tagFilter }
        ToggleOptionsPane -> H.modify_ \state -> state { optionsPaneExpanded = not state.optionsPaneExpanded }
        NextSort -> H.modify_ \state -> state { sortBy = nextSort state.sortBy }
        ClearNavigation -> H.modify_ clearCurrentActions

        NavigateTo mevt location ->
            let
                nextNavigation   = case location of
                    AtGroup subjId groupPath ->
                                                Navigation.toGroup subjId groupPath
                    AtItem subjId groupPath itemIdx ->
                                                Navigation.toItem subjId groupPath itemIdx
                    AtModifier subjId groupPath itemIdx (Modify.PrefixMod prefixKey) ->
                                                Navigation.toPrefix subjId groupPath itemIdx prefixKey
                    AtModifier subjId groupPath itemIdx (Modify.SuffixMod suffixKey) ->
                                                Navigation.toSuffix subjId groupPath itemIdx suffixKey
                    Nowhere ->
                                                Navigation.clear
                editNavigation s = case location of
                    AtGroup subjId groupPath -> Navigation.editGroupName subjId groupPath
                                                $ maybe (EncodedValue "") (R.g_title >>> EncodedValue)
                                                $ R.findGroup subjId groupPath s.report
                    AtItem subjId groupPath itemIdx ->
                                                Navigation.editItemName subjId groupPath itemIdx
                                                $ maybe (EncodedValue "") (R.i_name >>> EncodedValue)
                                                $ R.findItem subjId groupPath itemIdx s.report
                    AtModifier subjId groupPath itemIdx (Modify.PrefixMod prefixKey) ->
                                                Navigation.editPrefix subjId groupPath itemIdx prefixKey
                                                $ maybe (EncodedValue "") (Prefix.encodePrefix >>> Tuple.snd)
                                                $ Prefix.get prefixKey
                                                =<< R.i_prefixes
                                                <$> R.findItem subjId groupPath itemIdx s.report
                    AtModifier subjId groupPath itemIdx (Modify.SuffixMod suffixKey) ->
                                                Navigation.editSuffix subjId groupPath itemIdx suffixKey
                                                $ maybe (EncodedValue "") (Suffix.encodeSuffix >>> Tuple.snd)
                                                $ Suffix.get suffixKey
                                                =<< R.i_suffixes @item_tag
                                                <$> R.findItem subjId groupPath itemIdx s.report
                    Nowhere -> s.navigatedTo
                navigateOrEdit s =
                    if Navigation.isEditing s.navigatedTo then s else
                    if s.readOnlyMode || s.navigatedTo /= nextNavigation
                        then s { navigatedTo = nextNavigation }
                        else s { navigatedTo = editNavigation s }
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
                                # fromMaybe curReport
                        AtItem subjId groupPath itemIdx ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at item"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemName itemIdx
                                    , newValue : encval
                                    }
                                # fromMaybe curReport
                        AtModifier subjId groupPath itemIdx modKey ->
                            curReport
                                # Modify.modifyAt @item_tag
                                    -- ( Debug.spy "edit at prefix"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemModifier itemIdx modKey
                                    , newValue : encval
                                    }
                                # fromMaybe curReport
            in
                H.modify_
                    \s -> s { report = Modify.recalculate @item_tag $ nextReport s.report }

        StartEditing mevt -> stopPropagation mevt -- <> H.modify_ _ { editingValue = true }
        CancelEditing -> H.modify_ clearEditing
        ToggleReadOnlyMode ->
            H.modify_ clearEditing <> H.modify_ \s -> s { readOnlyMode = not s.readOnlyMode }
        ToggleDebugMode -> H.modify_ \s -> s { debugEnabled = not s.debugEnabled }
        EnableExport exportTarget -> H.modify_ _ { mbExportTo = Just exportTarget }
        DisableExport -> H.modify_ _ { mbExportTo = Nothing }
        TurnSubjectNavNamesOff -> H.modify_ \s -> s { showSubjectNavNames = false }
        TurnSubjectNavNamesOn -> H.modify_ \s -> s { showSubjectNavNames = true }
        NoOp -> pure unit


renderSubject
    :: forall @subj_id @subj_tag @item_tag subj group item slots m
     . Eq subj_id
    => R.IsTag item_tag
    => R.IsTag subj_tag
    => R.IsItem item
    => R.IsGroup group
    => R.IsSubjectId subj_id subj
    => R.IsSubject subj_id subj
    => R.HasPrefixes item
    => R.HasSuffixes item_tag item
    => R.HasTabular item
    => R.HasTabular subj
    => R.HasStats group
    => R.HasTags subj_tag subj
    => NavigatedTo subj_id
    -> subj
    -> Map group (Array item)
    -> HH.ComponentHTML (Action subj_id subj_tag (R.Report subj group item)) slots m
renderSubject navigatedTo subj itemsMap  =
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
        : (renderSubjectTabularValues subj)
        : (renderTree <$> Map.toUnfoldable itemsMap)
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
                        [ HH.text $ R.g_title group ]
                    , qspacerSpan
                    , renderPath groupPath
                    -- , HH.text (show group.mbIndexPath)
                    , qspacerSpan
                    , renderGroupStats groupStats
                    , HH.div
                        [ HP.style "border-left: 4px solid #eee; padding-left: 5px; margin-top: 10px; margin-bottom: 15px;" ]
                        $ mapWithIndex (renderGroupItem groupPath) groupItems
                    ]

            renderGroupItem groupPath itemIdx item =
                let
                    isNavigatedToItem = navigatedTo # Navigation.atItem subjId groupPath itemIdx
                    isEditingItemName = navigatedTo # Navigation.editingItemName subjId groupPath itemIdx
                    isEditingPrefix prefix = navigatedTo # Navigation.editingAtPrefix subjId groupPath itemIdx prefix
                    isEditingSuffix suffix = navigatedTo # Navigation.editingAtSuffix subjId groupPath itemIdx suffix
                    mbCurrentPrefix = if isNavigatedToItem then navigatedTo.mbModifier >>= Modify.loadPrefixKey else Nothing
                    mbCurrentSuffix = if isNavigatedToItem then navigatedTo.mbModifier >>= Modify.loadSuffixKey else Nothing
                    itemSelectedStyle = "background-color: #f0f8ff; border-radius: 3px;"
                    itemUsualStyle = "background-color: transparent;"
                    makeItemNameEditEvt = EditAt $ AtItem subjId groupPath itemIdx
                    makePrefixClickEvt prefixKey mevt = NavigateTo mevt $ AtModifier subjId groupPath itemIdx $ Modify.PrefixMod prefixKey
                    makePrefixEditEvt prefixKey = EditAt $ AtModifier subjId groupPath itemIdx $ Modify.PrefixMod prefixKey
                    makeSuffixClickEvt suffixKey mevt = NavigateTo mevt $ AtModifier subjId groupPath itemIdx $ Modify.SuffixMod suffixKey
                    makeSuffixEditEvt suffixKey = EditAt $ AtModifier subjId groupPath itemIdx $ Modify.SuffixMod suffixKey
                    -- itemSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
                    -- itemUsualStyle = "border: 1px dashed transparent;"
                in HH.div
                    [ HP.style
                        $ if isNavigatedToItem then itemSelectedStyle else itemUsualStyle
                    , HE.onClick $ \mevt -> NavigateTo mevt $ AtItem subjId groupPath itemIdx
                    ]
                    $ HH.span_
                        (renderPrefixes
                            { mbSelectedPrefix : mbCurrentPrefix
                            , isEditingPrefix
                            , onClick : makePrefixClickEvt
                            , onEdit : makePrefixEditEvt
                            , onStartEditing  : StartEditing
                            , onCancelEditing : CancelEditing
                            , noop : NoOp
                            }
                            item
                        )
                    : case R.i_mbTitle item of
                        Just title -> HH.text title
                        Nothing -> HH.text ""
                    : HH.span_ (renderSuffixes
                            @item_tag
                            { mbSelectedSuffix : mbCurrentSuffix
                            , isEditingSuffix
                            , isEditingItemName
                            , onClick : makeSuffixClickEvt
                            , onEdit : makeSuffixEditEvt
                            , onEditItemName : makeItemNameEditEvt
                            , onStartEditing  : StartEditing
                            , onCancelEditing : CancelEditing
                            , noop : NoOp
                            }
                            item
                        )
                    : pure (renderItemTabularValues item)


-- TODO: remove
navigationHint :: forall subj_id w i. Show subj_id => NavigatedTo subj_id -> HH.HTML w i
navigationHint navigation =
    let
        navigationHintStyle = "position: fixed; border: 1px solid black; background-color: #ffffe0ff; padding: 5px 10px; bottom: -5px; left: 60px; max-width: 70%; font-size: 0.7em; box-shadow: 2px 2px 5px gray; border-radius: 5px; overflow: hidden;"
        hintIfEditing Nothing = HH.text ""
        hintIfEditing (Just { value : EncodedValue val }) = HH.text $ "E:" <> show val
    in case Navigation.toLocation navigation of
        Nowhere -> HH.text ""
        AtGroup subjId groupPath ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text $ "G: " <> show subjId <> " / " <> show groupPath
                , qspacerSpan
                , hintIfEditing navigation.mbEditing
                ]
        AtItem subjId groupPath itemIdx ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text $ "I: " <> show subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ]"
                , qspacerSpan
                , hintIfEditing navigation.mbEditing
                ]
        AtModifier subjId groupPath itemIdx (Modify.PrefixMod prefixKey) ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text $ "X: " <> show subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ] . " <> Prefix.debugNavLabel prefixKey
                , qspacerSpan
                , hintIfEditing navigation.mbEditing
                ]
        AtModifier subjId groupPath itemIdx (Modify.SuffixMod suffixKey) ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text $ "X: " <> show subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ] . " <> Suffix.debugNavLabel suffixKey
                , qspacerSpan
                , hintIfEditing navigation.mbEditing
                ]