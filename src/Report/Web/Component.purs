module Report.Web.Component where

import Prelude

import Data.Array ((:))
import Data.Array (length, snoc, catMaybes, elem, filter, sortWith, reverse, any) as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (length, contains, toLower, Pattern(..)) as String
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- import Input.GameLog.Achievement as Ach
-- import Input.GameLog.InfiniteBacklog.API.Data.Games (GameCollection') as IBL
-- import Input.GameLog.Backloggery.Game (Rec) as BLGame
-- import Input.GameLog.Types as GLT

import Report.Core as CT
import Report.GroupPath (howDeep) as S
import Report.Modifiers.Stats (GotTotal(..), gotTotalFromStats, weightOf) as S
import Report.Suffix as Suffixes
import Report.Class as S
import Report (Report, unwrap) as S

import Report.Web.Helpers (qcolorSpan, qspacerSpan, timeColor)
import Report.Web.Modifiers.Progress (renderProgress)
import Report.Web.Modifiers.Stats (renderGroupStats)
import Report.Web.GroupPath (groupPathId, renderGroupRef, renderPath)


type State subj_id subj_tag report =
    { selected :: Array subj_id
    , report :: report
    , filter :: Maybe String
    , tagFilter :: Array subj_tag
    , optionsPaneExpanded :: Boolean
    , sortBy :: SubjectSort
    }


type Input subj group item = S.Report subj group item


data Action subj_id subj_tag input
    = SelectItem subj_id
    | AddToSelection subj_id
    | DeselectItem subj_id
    | DeselectAll
    | Receive input
    | ChangeListFilter String
    | IncludeTag subj_tag
    | ExcludeTag subj_tag
    | ToggleOptionsPane
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
     . Ord subj_id
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
        { selected : preSelected
        , report
        , filter : Nothing
        , tagFilter : []
        , optionsPaneExpanded : true
        , sortBy : ByWeight
        }

    selectionKeyToSubject :: Array subj -> Map subj_id subj
    selectionKeyToSubject = map (\subj -> S.s_id @subj_id @subj_tag subj /\ subj) >>> Map.fromFoldable

    render :: State subj_id subj_tag (S.Report subj group item) -> HH.ComponentHTML (Action subj_id subj_tag (Input subj group item)) () m
    render state =
        let
            report = S.unwrap state.report
            allSubjects = Set.toUnfoldable $ Map.keys report
            selKeys = selectionKeyToSubject allSubjects
        in HH.div
            [ HP.style "font-family: \"JetBrains Mono\", sans-serif; display: flex; flex-direction: row;" ]
            [ HH.div
                [ HP.style "height: 100vh; min-width: 75%; overflow-y: scroll;" ]
                 $  Tuple.uncurry (renderSubject @subj_id @subj_tag @item_tag)
                <$> Array.catMaybes ((\selId -> Map.lookup selId selKeys >>= (\subj -> Map.lookup subj report <#> (/\) subj)) <$> state.selected)
            , HH.div
                [ HP.style "margin: 0 auto; max-width: 900px; padding: 20px 20px 50px 20px;" ]
                [ subjectsToc state allSubjects ]
            ]

    subjectsToc state allSubjects =
        let
            tagsFitHorz = 10
            rowHeightVert = 17
            selTagsRows = Int.ceil (Int.toNumber (Array.length state.tagFilter) / Int.toNumber tagsFitHorz)
            collapsedOPHeight = selTagsRows * rowHeightVert
            allTagsRows = Int.ceil (Int.toNumber (Array.length (S.allTags :: Array subj_tag)) / Int.toNumber tagsFitHorz)
            expandedOPHeight = allTagsRows * rowHeightVert
            listShift  = show (if state.optionsPaneExpanded then 40 + expandedOPHeight else 40 + collapsedOPHeight) <> "px"
            listHeight = show (if state.optionsPaneExpanded then 92 else 95) <> "vh"
        in HH.div
            [ HP.style "padding: 9px 9px 0 0; width: 400px;" ]
            $ filterInput state
            : optionsPane state
            : (HH.div
                    [ HP.style $ "overflow-y: scroll;" ]
                    $ subjTocRow state.selected <$> (applyFilter state.tagFilter state.sortBy state.filter) allSubjects)
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

    subjTagWrap tag =
        HH.span [ HP.style $ "display: inline-block; cursor: pointer; padding: 3px 0;" ] [ subjTagBadge tag ]

    subjTagButton (tag /\ tagEnabled) =
        HH.span
            [ HE.onClick $ const $ if tagEnabled then ExcludeTag tag else IncludeTag tag
            , HP.style $ "display: inline-block; cursor: pointer; padding: 3px 0; opacity: " <> show (if tagEnabled then 1.0 else 0.5) <> ";"
            ]
            [ subjTagBadge tag ]

    -- topInTheList = 500.0

    -- getGameId = case _ of
    --     FromDhall dhallKey _ -> Left dhallKey
    --     Other (Ach.Game game) -> Right game.gameId

    -- sortByAchieved = case _ of
    --     FromDhall _ _ -> topInTheList
    --     Other (Ach.Game game) -> game.stats # S.weightOf

    sortSubjects :: SubjectSort -> subj -> SortKey
    sortSubjects = case _ of
        ByWeight -> S.s_stats @subj_id @subj_tag >>> S.weightOf >>> SN
        Alpha -> S.s_name @subj_id @subj_tag >>> SS

    subjTocRow selected subj =
        let
            subjId = S.s_id @subj_id @subj_tag subj
            isSelected = Array.elem subjId selected
        in HH.div
            [ HP.style "margin: 5px 0;" ]
            [ HH.span
                [ HE.onClick $ const $ if isSelected then DeselectItem subjId else AddToSelection subjId
                , HP.style "cursor: pointer;"
                ]
                [ HH.text $ if isSelected then "(-)" else "(+)"
                ]
            , HH.span
                [ HE.onClick $ const $ SelectItem subjId
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

    gotTotalBadge { got, total } =
        HH.span
            [ HP.style "font-size: 0.8em; opacity: 0.8; margin: 5px 0;" ]
            [ HH.text $ " (" <> show got <> "/" <> show total <> ")" ]


    subjTagBadge :: subj_tag -> _
    subjTagBadge tag =
        let tagStyle = S.tagColors tag
        in HH.span
            [ HP.style $ "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; padding: 1px 3px; border-radius: 4px; border: 1px solid " <> tagStyle.border <> "; opacity: 0.8; background-color: " <> tagStyle.background <> "; color: " <> tagStyle.text <> ";" ]
            [ HH.text $ S.tagContent tag ]

    nextSort = case _ of
        ByWeight -> Alpha
        Alpha -> ByWeight

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
        SelectItem subjId -> H.modify_ _ { selected = [ subjId ] }
        AddToSelection subjId -> H.modify_ \state -> state { selected = Array.snoc state.selected subjId }
        DeselectItem subjId -> H.modify_ \state -> state { selected = Array.filter (_ /= subjId) state.selected }
        DeselectAll -> H.modify_ _ { selected = [ ] }
        Receive nextReport -> H.modify_ _ { report = nextReport }
        ChangeListFilter filter -> H.modify_ _ { filter = if String.length filter > 0 then Just filter else Nothing }
        IncludeTag subjTag -> H.modify_ \state -> state { tagFilter = Array.snoc state.tagFilter subjTag }
        ExcludeTag subjTag -> H.modify_ \state -> state { tagFilter = Array.filter (_ /= subjTag) state.tagFilter }
        ToggleOptionsPane -> H.modify_ \state -> state { optionsPaneExpanded = not state.optionsPaneExpanded }
        NextSort -> H.modify_ \state -> state { sortBy = nextSort state.sortBy }


renderSubject
    :: forall @subj_id @subj_tag @item_tag subj group item slots action m
     . S.IsTag item_tag
    => S.IsItem item_tag item
    => S.IsGroup group
    => S.IsSubject subj_id subj_tag subj
    => subj -> Map group (Array item) -> HH.ComponentHTML slots action m
renderSubject subj itemsMap  =
    HH.div
        [ HP.style "padding: 10px 0 10px 20px;" ]
        $ HH.div
            [ HP.style "margin: 15px 0 30px 0; max-width: 60%; border-bottom: 1px solid gray; padding-bottom: 5px; font-size: 1.2em; " ]
            [ HH.text $ S.s_name @subj_id @subj_tag subj ]
        : (renderTree <$> Map.toUnfoldable itemsMap)
        where
            lineHeight = 1.9
            nestMargin = 30.0
            marginFor groupPath = (max 0.0 $ (Int.toNumber $ S.howDeep groupPath) - 1.0) * nestMargin
            renderTree (group /\ groupItems) =
                HH.div
                    [ HP.style $ "padding-bottom: 10px; line-height: " <> show lineHeight <> "em; margin-left: " <> (show $ marginFor $ S.g_path group) <> "px;"
                    , HP.id $ groupPathId $ S.g_path group
                    ]
                    [ HH.span [ HP.style "font-weight: bold;" ] [ HH.text $ S.g_title group ]
                    , qspacerSpan
                    , renderPath $ S.g_path group
                    -- , HH.text (show group.mbIndexPath)
                    , qspacerSpan
                    , renderGroupStats $ S.g_stats group
                    , HH.div
                        [ HP.style "border-left: 4px solid #eee; padding-left: 5px; margin-top: 10px; margin-bottom: 15px;" ]
                        $ renderGroupItem <$> groupItems
                    ]

            renderGroupItem item =
                let
                    i_suffixes = S.i_suffixes @item_tag item
                in HH.div
                    []
                    [ case S.i_mbTitle @item_tag item of
                        Just title -> HH.text title
                        Nothing -> case i_suffixes # Suffixes.getProgress of
                            Just progress -> renderProgress (S.i_name @item_tag item) progress
                            Nothing -> HH.text ""
                    , case i_suffixes # Suffixes.getEarnedAt of
                        Just (CT.SDate { day, month, year }) -> HH.span []
                            [ qspacerSpan
                            , HH.span [ HP.style "font-size: 0.8em; color: silver;" ]
                                [ qcolorSpan timeColor $ CT.toLeadingZero day
                                , qspacerSpan
                                , qcolorSpan timeColor $ show month
                                , qspacerSpan
                                , qcolorSpan timeColor $ show year
                                ]
                            ]
                        Nothing -> HH.text ""
                    , case i_suffixes # Suffixes.getDescription of
                        Just description -> HH.span []
                            [ qspacerSpan
                            , HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                            ]
                        Nothing -> HH.text ""
                    , case i_suffixes # Suffixes.getReference of
                        Just groupRef ->
                            renderGroupRef groupRef
                        Nothing -> HH.text ""
                    , case S.i_tags @item_tag item of
                        [] -> HH.text ""
                        tags -> HH.span [] $ itemTagBadge <$> tags
                    ]

            itemTagBadge :: item_tag -> _
            itemTagBadge tag =
              let tagStyle = S.tagColors tag
              in HH.span
                [ HP.style $ "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; padding: 1px 3px; border-radius: 4px; border: 1px solid " <> tagStyle.border <> "; opacity: 0.8; background-color: " <> tagStyle.background <> "; color: " <> tagStyle.text <> ";" ]
                [ HH.text $ S.tagContent tag ]



{-
gameName :: Game -> String
gameName = case _ of
    FromDhall dhallKey _ -> dhallKeyToGameName dhallKey
    Other (Ach.Game game) -> game.name


dhallKeyToGameName :: DhallKey -> String
dhallKeyToGameName = case _ of
    AstralChain -> "Astral Chain"
    NonogramsKatana -> "Nonograms Katana"
    Torchlight2 -> "Torchlight II"
    ForzaHorizon5 -> "Forza Horizon 5"
    StarlinkBattleOfAtlas -> "Starlink: Battle of Atlas"
    MarioOdissey -> "Super Mario: Odissey"
-}
