module Report.Web.Component where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Array ((:))
import Data.Array (length, snoc, catMaybes, elem, filter, sortWith, reverse, any) as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (length, contains, toLower, Pattern(..)) as String
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\))
import Data.FunctorWithIndex (mapWithIndex)

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as ME
import Web.Event.Event (stopPropagation) as Event

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- import Input.GameLog.Achievement as Ach
-- import Input.GameLog.InfiniteBacklog.API.Data.Games (GameCollection') as IBL
-- import Input.GameLog.Backloggery.Game (Rec) as BLGame
-- import Input.GameLog.Types as GLT

import Report.Core (EncodedValue(..))
import Report.GroupPath (GroupPath, howDeep) as GP
import Report.Modifiers.Stats (GotTotal(..), gotTotalFromStats, weightOf) as S
import Report.Class as S
import Report (Report, toMap) as S
import Report.Suffix (Key) as Suffix

import Report.Web.Helpers (qspacerSpan, lineHeight, nestMargin)
import Report.Web.Modifiers.Stats (renderGroupStats, gotTotalBadge)
import Report.Web.Modifiers.Tag (subjTagBadge, subjTagWrap)
import Report.Web.GroupPath (groupPathId, renderPath)
import Report.Web.Suffix (renderSuffixes)
import Report.Web.Navigation (NavigatedTo)
import Report.Web.Navigation as Navigation


type State subj_id subj_tag report =
    { subjects :: Array subj_id
    , report :: report
    , filter :: Maybe String
    , tagFilter :: Array subj_tag
    , optionsPaneExpanded :: Boolean
    , sortBy :: SubjectSort
    , readOnlyMode :: Boolean
    , navigatedTo :: NavigatedTo subj_id
    , editing :: Maybe EncodedValue
    }


type Input subj group item = S.Report subj group item


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
    | ClearNavigation
    | NavigateToGroup MouseEvent subj_id GP.GroupPath
    | NavigateToItem MouseEvent subj_id GP.GroupPath Int
    | NavigateToSuffix MouseEvent subj_id GP.GroupPath Int Suffix.Key
    | NextSort


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
    => Ord subj_id
    => Ord subj
    => Ord group
    => S.IsTag subj_tag
    => S.IsTag item_tag
    => S.IsItem item_tag item
    => S.IsGroup group
    => S.IsSubject subj_id subj_tag subj
    => Array subj_id
    -> H.Component query (Input subj group item) output m
component preSelected =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
        }
    where
    initialState :: Input subj group item -> State subj_id subj_tag (S.Report subj group item)
    initialState report =
        { subjects : preSelected
        , report
        , filter : Nothing
        , tagFilter : []
        , optionsPaneExpanded : true
        , sortBy : ByWeight
        , readOnlyMode : true
        , navigatedTo : Navigation.init
        , editing : Nothing
        }

    s_id :: subj -> subj_id
    s_id subj = S.s_id subj

    selectionKeyToSubject :: Array subj -> Map subj_id subj
    selectionKeyToSubject = map (\subj -> s_id subj /\ subj) >>> Map.fromFoldable

    render :: State subj_id subj_tag (S.Report subj group item) -> HH.ComponentHTML (Action subj_id subj_tag (Input subj group item)) () m
    render state =
        let
            report = S.toMap state.report
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
    makeTagFilter tags subj = Array.any (flip Array.elem tags) (S.s_tags @subj_id @subj_tag subj :: Array subj_tag)

    applyFilter tagFilter sortBy mbFilter subjects
        = subjects
            # Array.filter (makeTagFilter tagFilter)
            # Array.filter (case mbFilter of
                Just filterStr -> \subj -> String.contains (String.Pattern $ String.toLower filterStr) $ String.toLower $ S.s_name @subj_id @subj_tag subj
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
            subjTagButton <$> subjTagIsOn state.tagFilter <$> (S.allTags :: Array subj_tag)

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
        ByWeight -> S.s_stats @subj_id @subj_tag >>> S.weightOf >>> SN
        Alpha -> S.s_name @subj_id @subj_tag >>> SS

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
                    [ HH.text $ S.s_name @subj_id @subj_tag subj
                    , case S.gotTotalFromStats $ S.s_stats @subj_id @subj_tag subj of
                        S.Defined gotTotal -> gotTotalBadge gotTotal
                        _ -> HH.text ""
                    ] <>
                    (subjTagBadge <$> S.s_tags @subj_id @subj_tag subj)
                )

            ]

    nextSort = case _ of
        ByWeight -> Alpha
        Alpha -> ByWeight

    stopPropagation :: forall s a sl o. MouseEvent -> H.HalogenM s a o sl m Unit
    stopPropagation mEvent = H.liftEffect $ Event.stopPropagation $ ME.toEvent mEvent

    handleAction
        :: Action subj_id subj_tag (Input subj group item)
        -> H.HalogenM
            (State subj_id subj_tag (S.Report subj group item))
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
        ClearNavigation -> H.modify_ _ { navigatedTo = Navigation.clear }
        NavigateToGroup mevt subjId  groupPath ->
            stopPropagation mevt <> H.modify_ _ { navigatedTo = Navigation.toGroup subjId groupPath }
        NavigateToItem mevt subjId groupPath itemIdx ->
            stopPropagation mevt <> H.modify_ _ { navigatedTo = Navigation.toItem subjId groupPath itemIdx }
        NavigateToSuffix mevt subjId groupPath itemIdx suffixKey ->
            stopPropagation mevt <> H.modify_ _ { navigatedTo = Navigation.toSuffix subjId groupPath itemIdx suffixKey }


renderSubject
    :: forall @subj_id @subj_tag @item_tag subj group item slots m
     . Eq subj_id
    => S.IsTag item_tag
    => S.IsItem item_tag item
    => S.IsGroup group
    => S.IsSubject subj_id subj_tag subj
    => NavigatedTo subj_id
    -> subj
    -> Map group (Array item)
    -> HH.ComponentHTML (Action subj_id subj_tag (S.Report subj group item)) slots m
renderSubject navigatedTo subj itemsMap  =
    HH.div
        [ HP.style "padding: 10px 0 10px 20px;" ]
        $ HH.div
            [ HP.style "margin: 15px 0 30px 0; max-width: 60%; border-bottom: 1px solid gray; padding-bottom: 5px; font-size: 1.2em;"
            , HE.onClick $ const ClearNavigation
            ]
            [ HH.text $ S.s_name @subj_id @subj_tag subj ]
        : (renderTree <$> Map.toUnfoldable itemsMap)
        where
            subjId = S.s_id subj
            marginFor groupPath = (max 0.0 $ (Int.toNumber $ GP.howDeep groupPath) - 1.0) * nestMargin
            groupSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
            groupUsualStyle = "border: 1px dashed transparent;"
            renderTree (group /\ groupItems) =
                let
                    groupPath = S.g_path group
                    groupStats = S.g_stats group
                    isNavigatedTo = navigatedTo # Navigation.atGroup subjId groupPath
                in HH.div
                    [ HP.style $ "padding-bottom: 10px; line-height: "
                        <> show lineHeight <> "em; margin-left: "
                        <> (show $ marginFor groupPath) <> "px;"
                        -- <> (if isNavigatedTo then " background-color: #f0f8ff;" else "")
                    , HP.id $ groupPathId groupPath
                    , HE.onClick $ \mevt -> NavigateToGroup mevt subjId groupPath
                    ]
                    [ HH.span
                        [ HP.style $ "font-weight: bold;"
                            <> if isNavigatedTo then groupSelectedStyle else groupUsualStyle
                        ]
                        [ HH.text $ S.g_title group ]
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
                    mbCurrentSuffix = if isNavigatedTo then navigatedTo.mbSuffix else Nothing
                    itemSelectedStyle = "background-color: #f0f8ff; border-radius: 3px;"
                    itemUsualStyle = "background-color: transparent;"
                    makeSuffixClickEvt key mevt = NavigateToSuffix mevt subjId groupPath itemIdx key
                    -- itemSelectedStyle = "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
                    -- itemUsualStyle = "border: 1px dashed transparent;"
                in HH.div
                    [ HP.style
                        $ if isNavigatedTo then itemSelectedStyle else itemUsualStyle
                    , HE.onClick $ \mevt -> NavigateToItem mevt subjId groupPath itemIdx
                    ]
                    $ case S.i_mbTitle @item_tag item of
                        Just title -> HH.text title
                        Nothing -> HH.text ""
                    : renderSuffixes @item_tag makeSuffixClickEvt mbCurrentSuffix item