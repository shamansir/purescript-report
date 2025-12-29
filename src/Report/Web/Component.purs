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
import Data.Tuple.Nested ((/\))

import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as ME

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Report (Report, toMap, findGroup, findItem, withItem, withGroup) as R
import Report.Class as R
import Report.Core.Logic (EncodedValue(..))
import Report.Encoding.Suffix (encodeSuffix) as Suffix
import Report.GroupPath (howDeep) as GP
import Report.Modifiers.Stats (GotTotal(..), gotTotalFromStats, weightOf) as R
import Report.Modify (Location(..))
import Report.Modify (class GroupModify, What(..), WhatKey(..), modifyAt) as Modify
import Report.Suffix (get, put, debugNavLabel) as Suffix

import Report.Web.GroupPath (groupPathId, renderPath)
import Report.Web.Helpers (qspacerSpan, lineHeight, nestMargin)
import Report.Web.Modifiers.Stats (renderGroupStats, gotTotalBadge)
import Report.Web.Modifiers.Tags (subjTagBadge, subjTagWrap)
import Report.Web.Navigation (NavigatedTo)
import Report.Web.Navigation as Navigation
import Report.Web.Suffix (renderSuffixes)


showNavigationDebugHint = true :: Boolean


type State subj_id subj_tag report =
    { subjects :: Array subj_id
    , report :: report
    , filter :: Maybe String
    , tagFilter :: Array subj_tag
    , optionsPaneExpanded :: Boolean
    , sortBy :: SubjectSort
    , readOnlyMode :: Boolean
    , navigatedTo :: NavigatedTo subj_id
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


component
    :: forall @subj_id @subj_tag @item_tag subj group item query output m
     . MonadEffect m
    => Ord subj
    => Ord subj_id
    => Show subj_id
    => Ord group
    => Modify.GroupModify group
    => R.IsTag subj_tag
    => R.IsTag item_tag
    => R.IsItem item_tag item
    => R.IsGroup group
    => R.IsSubject subj_id subj_tag subj
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
        , readOnlyMode : false
        , navigatedTo : Navigation.init
        }

    s_id :: subj -> subj_id
    s_id subj = R.s_id subj

    selectionKeyToSubject :: Array subj -> Map subj_id subj
    selectionKeyToSubject = map (\subj -> s_id subj /\ subj) >>> Map.fromFoldable

    render :: State subj_id subj_tag (R.Report subj group item) -> HH.ComponentHTML (Action subj_id subj_tag (Input subj group item)) () m
    render state =
        let
            report = R.toMap state.report
            allSubjects = Set.toUnfoldable $ Map.keys report
            selKeys = selectionKeyToSubject allSubjects
        in HH.div
            [ HP.style "font-family: \"JetBrains Mono\", sans-serif; display: flex; flex-direction: row;" ]
            [ HH.div
                [ HP.style "height: 100vh; min-width: 75%; overflow-y: scroll;"
                -- , HE.onClick $ const ClearNavigation
                ]
                 $  Tuple.uncurry (renderSubject @subj_id @subj_tag @item_tag state.navigatedTo)
                <$> Array.catMaybes ((\selId -> Map.lookup selId selKeys >>= (\subj -> Map.lookup subj report <#> (/\) subj)) <$> state.subjects)
            , HH.div
                [ HP.style "margin: 0 auto; max-width: 900px; padding: 20px 20px 50px 20px;" ]
                [ subjectsToc state allSubjects ]
            , if showNavigationDebugHint then navigationHint state.navigatedTo else HH.text ""
            ]

    subjectsToc state allSubjects =
        HH.div
            [ HP.style "padding: 9px 9px 0 0; width: 400px;" ]
            $ filterInput state
            : optionsPane state
            : (HH.div
                    [ HP.style $ "overflow-y: scroll;" ]
                    $ subjTocRow state.subjects <$> (applyFilter state.tagFilter state.sortBy state.filter) allSubjects)
            : []

    makeTagFilter [] _ = true
    makeTagFilter tags subj = Array.any (flip Array.elem tags) (R.s_tags @subj_id @subj_tag subj :: Array subj_tag)

    applyFilter tagFilter sortBy mbFilter subjects
        = subjects
            # Array.filter (makeTagFilter tagFilter)
            # Array.filter (case mbFilter of
                Just filterStr -> \subj -> String.contains (String.Pattern $ String.toLower filterStr) $ String.toLower $ R.s_name @subj_id @subj_tag subj
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
        ByWeight -> R.s_stats @subj_id @subj_tag >>> R.weightOf >>> SN
        Alpha -> R.s_name @subj_id @subj_tag >>> SS

    subjTocRow selectedSubjects subj =
        let
            subjId = s_id subj
            isSelected = Array.elem subjId selectedSubjects
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
                    [ HH.text $ R.s_name @subj_id @subj_tag subj
                    , case R.gotTotalFromStats $ R.s_stats @subj_id @subj_tag subj of
                        R.Defined gotTotal -> gotTotalBadge gotTotal
                        _ -> HH.text ""
                    ] <>
                    (subjTagBadge <$> R.s_tags @subj_id @subj_tag subj)
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
                    AtSuffix subjId groupPath itemIdx suffixKey ->
                                                Navigation.toSuffix subjId groupPath itemIdx suffixKey
                    Nowhere ->
                                                Navigation.clear
                editNavigation s = case location of
                    AtGroup subjId groupPath -> Navigation.editGroupName subjId groupPath
                                                $ maybe (EncodedValue "") (R.g_title >>> EncodedValue)
                                                $ R.findGroup subjId groupPath s.report
                    AtItem subjId groupPath itemIdx ->
                                                Navigation.editItemName subjId groupPath itemIdx
                                                $ maybe (EncodedValue "") (R.i_name @item_tag >>> EncodedValue)
                                                $ R.findItem subjId groupPath itemIdx s.report
                    AtSuffix subjId groupPath itemIdx suffixKey ->
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
                                # Modify.modifyAt
                                    -- ( Debug.spy "edit at group"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.GroupName
                                    , newValue : encval
                                    }
                                # fromMaybe curReport
                        AtItem subjId groupPath itemIdx ->
                            curReport
                                # Modify.modifyAt
                                    -- ( Debug.spy "edit at item"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemName itemIdx
                                    , newValue : encval
                                    }
                                # fromMaybe curReport
                        AtSuffix subjId groupPath itemIdx suffixKey ->
                            curReport
                                # Modify.modifyAt
                                    -- ( Debug.spy "edit at suffix"
                                    { subjId
                                    , path : groupPath
                                    , what : Modify.ItemSuffix itemIdx suffixKey
                                    , newValue : encval
                                    }
                                # fromMaybe curReport
            in
                H.modify_
                    \s -> s { report = nextReport s.report }

        StartEditing mevt -> stopPropagation mevt -- <> H.modify_ _ { editingValue = true }
        CancelEditing -> H.modify_ clearEditing
        NoOp -> pure unit


renderSubject
    :: forall @subj_id @subj_tag @item_tag subj group item slots m
     . Eq subj_id
    => R.IsTag item_tag
    => R.IsItem item_tag item
    => R.IsGroup group
    => R.IsSubject subj_id subj_tag subj
    => NavigatedTo subj_id
    -> subj
    -> Map group (Array item)
    -> HH.ComponentHTML (Action subj_id subj_tag (R.Report subj group item)) slots m
renderSubject navigatedTo subj itemsMap  =
    HH.div
        [ HP.style "padding: 10px 0 10px 20px;" ]
        $ HH.div
            [ HP.style "margin: 15px 0 30px 0; max-width: 60%; border-bottom: 1px solid gray; padding-bottom: 5px; font-size: 1.2em;"
            , HE.onClick $ const ClearNavigation
            ]
            [ HH.text $ R.s_name @subj_id @subj_tag subj ]
        : (renderTree <$> Map.toUnfoldable itemsMap)
        where
            subjId = R.s_id subj
            marginFor groupPath = (max 0.0 $ (Int.toNumber $ GP.howDeep groupPath) - 1.0) * nestMargin
            groupSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
            groupUsualStyle = "border: 1px dashed transparent;"
            renderTree (group /\ groupItems) =
                let
                    groupPath = R.g_path group
                    groupStats = R.g_stats group
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
                    isNavigatedTo = navigatedTo # Navigation.atItem subjId groupPath itemIdx
                    isEditingItemName = navigatedTo # Navigation.editingItemName subjId groupPath itemIdx
                    isEditingSuffix suffix = navigatedTo # Navigation.editingAtSuffix subjId groupPath itemIdx suffix
                    mbCurrentSuffix = if isNavigatedTo then navigatedTo.mbSuffix else Nothing
                    itemSelectedStyle = "background-color: #f0f8ff; border-radius: 3px;"
                    itemUsualStyle = "background-color: transparent;"
                    makeSuffixClickEvt key mevt = NavigateTo mevt $ AtSuffix subjId groupPath itemIdx key
                    makeSuffixEditEvt key = EditAt $ AtSuffix subjId groupPath itemIdx key
                    makeItemNameEditEvt = EditAt $ AtItem subjId groupPath itemIdx
                    -- itemSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
                    -- itemUsualStyle = "border: 1px dashed transparent;"
                in HH.div
                    [ HP.style
                        $ if isNavigatedTo then itemSelectedStyle else itemUsualStyle
                    , HE.onClick $ \mevt -> NavigateTo mevt $ AtItem subjId groupPath itemIdx
                    ]
                    $ case R.i_mbTitle @item_tag item of
                        Just title -> HH.text title
                        Nothing -> HH.text ""
                    -- : renderSuffixes @item_tag makeSuffixClickEvt isEditingSuffix mbCurrentSuffix isEditingItemName item
                    : renderSuffixes @item_tag
                            { onClick : makeSuffixClickEvt
                            , onEdit : makeSuffixEditEvt
                            , onEditItemName : makeItemNameEditEvt
                            , mbSelectedSuffix : mbCurrentSuffix
                            , isEditingSuffix
                            , isEditingItemName
                            , onStartEditing  : StartEditing
                            , onCancelEditing : CancelEditing
                            , noop : NoOp
                            , item
                            }
                            -- makeSuffixClickEvt isEditingSuffix mbCurrentSuffix isEditingItemName item


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
        AtSuffix subjId groupPath itemIdx suffixKey ->
            HH.div
                [ HP.style navigationHintStyle ]
                [ HH.text $ "X: " <> show subjId <> " / " <> show groupPath <> " [ " <> show itemIdx <> " ] . " <> Suffix.debugNavLabel suffixKey
                , qspacerSpan
                , hintIfEditing navigation.mbEditing
                ]