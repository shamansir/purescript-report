module Report.Web.Component where

import Prelude

import Data.Array ((:))
import Data.Array (length, snoc, catMaybes, elem, intersperse, filter, sortWith, reverse, any) as Array
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.String (length, contains, toLower, joinWith, Pattern(..)) as String
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
import Report.GroupPath (howDeep, pathToArray) as S
import Report.Task (TaskP(..)) as S
import Report.Stats (GotTotal(..), gotTotalFromStats, weightOf) as S
import Report.Progress (Progress(..)) as Stats
import Report.Class as S
import Report (Report, unwrap) as S



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
            [ HP.style "font-family: \"JetBrains Mono\", sans-serif; height: 100vh; overflow-y: scroll;" ]
            $ subjectsList state allSubjects
            : (Tuple.uncurry (renderSubject @subj_id @subj_tag @item_tag)
                <$> Array.catMaybes ((\selId -> Map.lookup selId selKeys >>= (\subj -> Map.lookup subj report <#> (/\) subj)) <$> state.selected))

    subjectsList state allSubjects =
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
            [ HP.style "position: absolute; right: 0; padding: 9px 9px 0 0; height: 100vh; width: 400px;" ]
            $ filterInput state
            : optionsPane state
            : (HH.div
                    [ HP.style $ "position: fixed; top: " <> listShift <> "; height: " <> listHeight <> "; overflow: scroll;" ]
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
            [ HP.style "position : fixed;" ]
            [ HH.input
                [ HE.onValueInput ChangeListFilter
                , HP.style "border: 1px solid lightgray; border-radius: 5px; padding: 5px 6px; margin-bottom: 15px; min-width: 250px;"
                ]
            , HH.span
                [ HE.onClick $ const ToggleOptionsPane
                , HP.style "padding: 1px 3px 0 5px; cursor: pointer;"
                ]
                [ HH.text $ if state.optionsPaneExpanded then "○" else "●" ]
            , HH.span
                [ HE.onClick $ const NextSort
                , HP.style "padding: 1px 3px; cursor: pointer;"
                ]
                [ HH.text $ case state.sortBy of
                    ByWeight -> "W"
                    Alpha -> "A"
                ]
            ]

    optionsPane state =
        HH.div
            [ HP.style "position: fixed; top: 33px;"
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
            levelsMargin = 30.0
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

            qcolor color = HP.style $ "color: " <> color <> ";"

            qcolorSpan color text =  HH.span [ qcolor color ] [ HH.text text ]
            qspacerSpan = HH.span [] [ HH.text " " ]
            qthinspacerSpan = HH.span [] [ HH.text " " ]
            qitemmarkerSpan color = HH.span [ qcolor color ] [ HH.text "🞄" {- "●" -}  ]
            qlevelmarkerSpan color = HH.span [ qcolor color ] [ HH.text "▪" ]
            qtimesplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "::" ]
            qpathsplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "::" ]
            qpersplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "/" ]
            qrangesplitSpan = HH.span [ qcolor splitcolor ] [ HH.text "-" ]

            completeColor = "#333"
            genericColor = "silvergray"
            incompleteColor = "#999"
            progressBarCompleteColor = "forestgreen"
            progressBarIncompleteColor = "silver"
            groupProgressBarCompleteColor = "darkslategray" -- cadetblue, cornflowerblue, darkcyan, darkslategray
            groupProgressBarIncompleteColor = "gainsboro" -- gainsboro
            textColor = "dodgerblue"
            numColor = "green"
            measureColor = "darkgray"
            timeColor = "darkolivegreen"
            splitcolor = "lightgray"
            errorColor = "red"

            renderProgress gitemName = case _ of
                Stats.None ->
                    HH.span []
                        [ qitemmarkerSpan incompleteColor
                        , qspacerSpan
                        , qcolorSpan incompleteColor gitemName
                        , qspacerSpan
                        , qcolorSpan incompleteColor "<None>"
                        ]
                Stats.Unknown ->
                    HH.span []
                        [ qitemmarkerSpan incompleteColor
                        , qspacerSpan
                        , qcolorSpan incompleteColor gitemName
                        , qspacerSpan
                        , qcolorSpan incompleteColor "???"
                        ]
                Stats.Error errorStr ->
                    HH.span []
                        [ qitemmarkerSpan genericColor
                        , qspacerSpan
                        , qcolorSpan genericColor gitemName
                        , qspacerSpan
                        , qcolorSpan errorColor errorStr
                        ]
                Stats.PText text ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan textColor text
                        ]
                Stats.PNumber num ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatNum num
                        ]
                Stats.PInt num ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatInt num
                        ]
                Stats.OnDate (CT.SDate { day, month, year }) ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero day
                        , qspacerSpan
                        , qcolorSpan timeColor $ show month
                        , qspacerSpan
                        , qcolorSpan timeColor $ show year
                        ]
                Stats.OnTime { hrs, min, sec } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero hrs
                        , qtimesplitSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero min
                        , qtimesplitSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero sec
                        ]
                Stats.ToComplete { done } ->
                    HH.span []
                        [ qitemmarkerSpan $ if done then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if done then completeColor else incompleteColor) gitemName
                        , qspacerSpan
                        , qcompleteCheckbox done
                        ]
                Stats.PercentI pctInt ->
                    let
                        isDone = pctInt >= 100
                    in
                    HH.span []
                        [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) gitemName
                        , qspacerSpan
                        , percentage 100.0 $ Int.toNumber pctInt
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt pctInt <> "%"
                        ]
                Stats.PercentN pctN ->
                    let
                        isDone = pctN >= 1.0
                    in
                    HH.span []
                        [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) gitemName
                        , qspacerSpan
                        , percentage 1.0 pctN
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum (pctN * 100.0) <> "%"
                        ]
                Stats.PercentSign { pct, sign } ->
                    let
                        isDone = pct >= 1.0
                        signStr = if sign >= 0 then "+" else "-"
                    in
                    HH.span []
                        [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) gitemName
                        , qspacerSpan
                        , percentage 1.0 pct
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ signStr <> formatNum (pct * 100.0) <> "%"
                        ]
                Stats.ToGetI { got, total } ->
                    let
                        isDone = (total > 0) && (got >= total)
                    in
                    HH.span []
                        [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) gitemName
                        , qspacerSpan
                        , percentage (Int.toNumber total) (Int.toNumber got)
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt got <> "/" <> formatInt total
                        ]
                Stats.ToGetN { got, total } ->
                    let
                        isDone = (total > 0.0) && (got >= total)
                    in
                    HH.span []
                        [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) gitemName
                        , qspacerSpan
                        , percentage total got
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum got <> "/" <> formatNum total
                        ]
                Stats.MeasuredI { amount, measure } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatInt amount
                        , qthinspacerSpan
                        , qcolorSpan measureColor $ measure
                        ]
                Stats.MeasuredN { amount, measure } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatNum amount
                        , qthinspacerSpan
                        , qcolorSpan measureColor $ measure
                        ]
                Stats.MeasuredSign { amount, measure, sign } ->
                    let
                        signStr = if sign >= 0 then "+" else "-"
                    in
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ signStr <> formatNum amount
                        , qthinspacerSpan
                        , qcolorSpan measureColor $ measure
                        ]
                Stats.PerI { amount, per } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatInt amount
                        , qpersplitSpan
                        , qcolorSpan measureColor per
                        ]
                Stats.PerN { amount, per } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatNum amount
                        , qpersplitSpan
                        , qcolorSpan measureColor per
                        ]
                Stats.RangeI { from, to } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatInt from
                        , qrangesplitSpan
                        , qcolorSpan numColor $ formatInt to
                        ]
                Stats.RangeN { from, to } ->
                    HH.span []
                        [ qitemmarkerSpan completeColor
                        , qspacerSpan
                        , qcolorSpan completeColor gitemName
                        , qspacerSpan
                        , qcolorSpan numColor $ formatNum from
                        , qrangesplitSpan
                        , qcolorSpan numColor $ formatNum to
                        ]
                Stats.Task task ->
                    HH.span []
                        [ qitemmarkerSpan $ taskTextColor task
                        , qspacerSpan
                        , qcolorSpan (taskTextColor task) gitemName
                        , qspacerSpan
                        , qtaskCheckbox task
                        ]
                Stats.LevelsI { reached, levels } ->
                    let
                        maximum = foldl max 0 $ _.maximum <$> levels
                        isDone = reached >= maximum
                    in HH.div
                        []
                        $ HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage (Int.toNumber maximum) (Int.toNumber reached)
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                            ]
                        : (renderLevelI reached <$> levels)
                Stats.LevelsN { reached, levels } ->
                    let
                        maximum = foldl max 0.0 $ _.maximum <$> levels
                        isDone = reached >= maximum
                    in HH.div
                        []
                        $ HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage maximum reached
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum reached <> "/" <> formatNum maximum
                            ]
                        : (renderLevelN reached <$> levels)
                Stats.LevelsS { reached, levels } ->
                    let
                        maximum = Array.length levels
                        isDone = reached >= maximum
                    in HH.div
                        []
                        $ HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage (Int.toNumber maximum) (Int.toNumber reached)
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                            ]
                        : (mapWithIndex (renderLevelS reached) levels)
                Stats.LevelsE { reached, total } ->
                    let
                        isDone = reached >= total
                    in HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage (Int.toNumber total) (Int.toNumber reached)
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt total <> "L"
                            ]
                Stats.LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } ->
                    let
                        isDone = levelReached >= totalLevels
                    in HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage (Int.toNumber totalLevels) (Int.toNumber levelReached)
                            , qcolorSpan (if isDone then completeColor else incompleteColor)
                                    $ formatInt levelReached <> "/" <> formatInt totalLevels <> "L"
                                    <> " : "
                                    <> formatInt reachedAtCurrent <> "/" <> formatInt maximumAtCurrent
                            ]
                Stats.LevelsP { levels } ->
                    let
                        maximum = Array.length levels
                        reached = Array.length $ Array.filter (_ == S.TDone) $ _.proc <$> levels
                        isDone = reached >= maximum
                    in HH.div
                        []
                        $ HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage (Int.toNumber maximum) (Int.toNumber reached)
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                            ]
                        : (mapWithIndex renderLevelP levels)
                Stats.LevelsO { reached, levels } ->
                    -- TODO: we only know if we are done is when all maximum values are Just

                    {- let
                        maximum = foldl max 0 $ _.maximum <$> levels
                        isDone = reached >= maximum
                     in -}
                     HH.div
                        []
                        $ {- HH.div []
                            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ gitemName
                            , qspacerSpan
                            , percentage (Int.toNumber maximum) (Int.toNumber reached)
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                            ]
                          : -} (renderLevelO reached <$> levels)

            pctWidth = 250.0
            percentage = percentage' progressBarCompleteColor progressBarIncompleteColor
            percentage' completeColor incompleteColor total amount =
                let
                    factor = pctWidth / total
                    toGoWidth = min pctWidth $ amount * factor
                    completedWidth = min pctWidth $ (total - amount) * factor
                in HH.span
                    [ HP.style $ "display: inline-block; position: relative; top: 2px; margin-right: 3px;" ]
                    [ HH.span
                        [ HP.style $ "display: inline-block; background-color: " <> completeColor <> "; height: 1em; width: " <> (show toGoWidth) <> "px;"
                        ]
                        [ ]
                    , HH.span
                        [ HP.style $ "display: inline-block; background-color: " <> incompleteColor <> "; height: 1em; width: " <> (show completedWidth) <> "px;"
                        ]
                        [ ]
                    ]
            renderLevelI reached { maximum, name } =
                let
                    isDone = reached >= maximum
                in HH.div
                    [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
                    [ HH.span []
                        [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) name
                        , qspacerSpan
                        , percentage (Int.toNumber maximum) (min (Int.toNumber maximum) (Int.toNumber reached))
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                        ]
                    ]
            renderLevelN reached { maximum, name } =
                let
                    isDone = reached >= maximum
                in HH.div
                    [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
                    [ HH.span []
                        [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) name
                        , qspacerSpan
                        , percentage maximum $ min reached maximum
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum reached <> "/" <> formatNum maximum
                        ]
                    ]
            renderLevelS reached levenN { gives } =
                let
                    isDone = reached > levenN
                in HH.div
                    [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
                    [ HH.span []
                        [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) gives
                        , qspacerSpan
                        , qcompleteCheckbox isDone
                        ]
                    ]
            renderLevelO reached { mbMaximum, name } =
                let
                    isDone = case mbMaximum of
                        Just maximum -> reached >= maximum
                        Nothing -> false
                in HH.div
                    [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
                    [ HH.span []
                        [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
                        , qspacerSpan
                        , qcolorSpan (if isDone then completeColor else incompleteColor) name
                        , qspacerSpan
                        , case mbMaximum of
                              Just maximum -> percentage (Int.toNumber maximum) (min (Int.toNumber maximum) (Int.toNumber reached))
                              Nothing -> HH.text "???"
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> maybe "??" formatInt mbMaximum
                        ]
                    ]
            renderLevelP levelN { proc, name } =
                let
                    isDone = proc == S.TDone
                in HH.div
                    [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
                    [ HH.span []
                        [ qlevelmarkerSpan $ taskTextColor proc
                        , qspacerSpan
                        , qcolorSpan (taskTextColor proc) name
                        , qspacerSpan
                        , qtaskCheckbox proc
                        ]
                    ]

            qcompleteCheckbox =
                qcheckbox (\v -> if v then progressBarCompleteColor else progressBarIncompleteColor) $ const ""
                -- [ HH.text $ if done then "" {-"●" -} else "" ]

            qtaskCheckbox =
                qcheckbox taskColor $ const ""
                -- [ HH.text $ if done then "" {-"●" -} else "" ]

            qcheckbox :: forall a. (a -> String) -> (a -> String) -> a -> _
            qcheckbox toColor toText a =
                HH.span
                    [ HP.style $ "display: inline-block;"
                                <> "border: 1px solid darkgray;"
                                <> "border-radius: 3px;"
                                <> "width: 1em;"
                                <> "height: 1em;"
                                <> "color: white;"
                                <> "position: relative; top: 2px; left: -2px;"
                                <> "background-color: " <> toColor a <> ";"
                    ]
                    [ HH.text $ toText a ]

            taskColor = case _ of
                S.TDone -> progressBarCompleteColor
                _ -> progressBarIncompleteColor -- TODO: implement other colors

            taskTextColor = case _ of
                S.TDone -> completeColor
                _ -> incompleteColor -- TODO: implement other colors

            formatInt = CT.formatWithCommas
            formatNum = CT.formatNumWithCommas { fixedTo : Just 2 }

            renderPath groupPath =
                HH.span
                    [ HP.style "font-size: 0.8em;" ]
                    $ Array.intersperse qpathsplitSpan $ renderPathSegment <$> S.pathToArray groupPath

            renderPathSegment segment =
                HH.span
                    [ HP.style "border: 1px solid #ddd; border-radius: 3px; padding: 2px 3px; color: coral;" ]
                    [ HH.text segment ]

            renderGroupStats = S.gotTotalFromStats >>> case _ of
                S.Defined { got, total } ->
                    let
                        isDone = got >= total
                    in
                        HH.div
                            [ HP.style "opacity: 0.55; max-width: 60%;" ]
                            [ {- qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                            , qspacerSpan
                            , -} percentage' groupProgressBarCompleteColor groupProgressBarIncompleteColor (Int.toNumber total) (Int.toNumber got)
                            , qcolorSpan (if isDone then completeColor else incompleteColor) $ "(" <> formatInt got <> "/" <> formatInt total <> ")"
                            ]
                    -- HH.span
                    --     [ HP.style "font-size: 0.8em; opacity: 0.8; margin: 5px 0;" ]
                    --     [ HH.text $ " (" <> show got <> "/" <> show total <> ")" ]
                S.GTStatsValue -> HH.text ""
                S.Undefined -> HH.text ""

            groupPathId = String.joinWith "--" <<< S.pathToArray

            renderGroupItem item =
                HH.div
                    []
                    [ case S.i_mbTitle @item_tag item of
                        Just title -> HH.text title
                        Nothing -> renderProgress (S.i_name @item_tag item) (S.i_progress @item_tag item)
                    , case (S.i_mbEarnedAt @item_tag item) of
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
                    , case (S.i_mbDescription @item_tag item) of
                        Just description -> HH.span []
                            [ qspacerSpan
                            , HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                            ]
                        Nothing -> HH.text ""
                    , case (S.i_mbReference @item_tag item) of
                        Just groupRef ->
                            HH.span []
                                [ qspacerSpan
                                , HH.a [ HP.href $ "#" <> groupPathId groupRef, HP.style "color: cadetblue; text-decoration: none;" ] [ HH.text "#" ]
                                ]
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
