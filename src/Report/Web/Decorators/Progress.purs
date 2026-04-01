module Report.Web.Decorators.Progress where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Int as Int
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array ((:))
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.KeyboardEvent as KE

import Report.Core as CT
import Report.Core.Logic as CT
import Report.Decorators.Task (TaskP(..)) as S
import Report.Decorators.Progress (Progress(..), Relation(..)) as Prog
import Report.Web.Decorators.EditInput as EI
import Report.Convert.Keyed

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Report.Web.Helpers
import Report.Web.Helpers.InlineOrBlock
import Report.Web.Decorators.Types (ProgressRenderConfig, VState(..))
import Report.Web.Decorators.Task (qtaskCheckbox, taskTextColor, taskVState)


renderProgress :: forall w i. ProgressRenderConfig i -> CT.ViewOrEdit { itemName :: String } -> CT.ViewOrEdit Prog.Progress -> VState /\ InlineOrBlock w i
renderProgress events voeItemName voeProgress = case progress of
    Prog.None ->
        Tuple Incomplete $
        Inline $ HH.span_
            [ qitemmarkerSpan incompleteColor
            , qspacerSpan
            , qitemnameSpan incompleteColor
            , qspacerSpan
            , qprogress $ qcolorSpan incompleteColor "<None>"
            ]
    Prog.Unknown ->
        Tuple Incomplete $
        Inline $ HH.span_
            [ qitemmarkerSpan incompleteColor
            , qspacerSpan
            , qitemnameSpan incompleteColor
            , qspacerSpan
            , qprogress $ qcolorSpan incompleteColor "???"
            ]
    Prog.Error errorStr ->
        Tuple Error $
        Inline $ HH.span_
            [ qitemmarkerSpan genericColor
            , qspacerSpan
            , qitemnameSpan genericColor
            , qspacerSpan
            , qprogress $ qcolorSpan errorColor errorStr
            ]
    Prog.PText text ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ qcolorSpan textColor text
            ]
    Prog.PNumber num ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ qcolorSpan numColor $ formatNum num
            ]
    Prog.PInt num ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ qcolorSpan numColor $ formatInt num
            ]
    Prog.OnDate (CT.SDate { day, month, year }) ->
        Tuple Neutral $ -- TODO: check if date is current?
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan timeColor $ CT.toLeadingZero day
                , qspacerSpan
                , qcolorSpan timeColor $ show month
                , qspacerSpan
                , qcolorSpan timeColor $ show year
                ]
            ]
    Prog.OnTime { hrs, min, sec } ->
        Tuple Neutral $ -- TODO: check if date is current?
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan timeColor $ CT.toLeadingZero hrs
                , qtimesplitSpan
                , qcolorSpan timeColor $ CT.toLeadingZero min
                , qtimesplitSpan
                , qcolorSpan timeColor $ CT.toLeadingZero sec
                ]
            ]
    Prog.ToComplete { done } ->
        Tuple (vsFrom done) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ if done then completeColor else incompleteColor
            , qspacerSpan
            , qitemnameSpan $ if done then completeColor else incompleteColor
            , qspacerSpan
            , qprogress $ qcompleteCheckbox done
            ]
    Prog.PercentI pctInt ->
        let
            isDone = pctInt >= 100
        in
        Tuple (vsFrom isDone) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qitemnameSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ percentage 100.0 $ Int.toNumber pctInt
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt pctInt <> "%"
                ]
            ]
    Prog.PercentN pctN ->
        let
            isDone = pctN >= 1.0
        in
        Tuple (vsFrom isDone) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qitemnameSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ percentage 1.0 pctN
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum (pctN * 100.0) <> "%"
                ]
            ]
    Prog.PercentSign { pct, sign } ->
        let
            isDone = pct >= 1.0
            signStr = if sign >= 0 then "+" else "-"
        in
        Tuple (vsFrom isDone) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qitemnameSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ percentage 1.0 pct
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ signStr <> formatNum (pct * 100.0) <> "%"
                ]
            ]
    Prog.ToGetI { got, total } ->
        let
            isDone = (total > 0) && (got >= total)
        in
        Tuple (vsFrom isDone) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qitemnameSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ percentage (Int.toNumber total) (Int.toNumber got)
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt got <> "/" <> formatInt total
                ]
            ]
    Prog.ToGetN { got, total } ->
        let
            isDone = (total > 0.0) && (got >= total)
        in
        Tuple (vsFrom isDone) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qitemnameSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ percentage total got
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum got <> "/" <> formatNum total
                ]
            ]
    Prog.MeasuredI { amount, measure } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ formatInt amount
                , qthinspacerSpan
                , qcolorSpan measureColor $ measure
                ]
            ]
    Prog.MeasuredN { amount, measure } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ formatNum amount
                , qthinspacerSpan
                , qcolorSpan measureColor $ measure
                ]
            ]
    Prog.MeasuredSign { amount, measure, sign } ->
        let
            signStr = if sign >= 0 then "+" else "-"
        in
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ signStr <> formatNum amount
                , qthinspacerSpan
                , qcolorSpan measureColor $ measure
                ]
            ]
    Prog.PerI { amount, per } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ formatInt amount
                , qpersplitSpan
                , qcolorSpan measureColor per
                ]
            ]
    Prog.PerN { amount, per } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ formatNum amount
                , qpersplitSpan
                , qcolorSpan measureColor per
                ]
            ]
    Prog.RangeI { from, to } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ formatInt from
                , qrangesplitSpan
                , qcolorSpan numColor $ formatInt to
                ]
            ]
    Prog.RangeN { from, to } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan numColor $ formatNum from
                , qrangesplitSpan
                , qcolorSpan numColor $ formatNum to
                ]
            ]
    Prog.Task task ->
        Tuple (taskVState task) $
        Inline $ HH.span_
            [ qitemmarkerSpan $ taskTextColor task
            , qspacerSpan
            , qitemnameSpan $ taskTextColor task
            , qspacerSpan
            , qprogress $ qtaskCheckbox task
            ]
    Prog.LevelsI { reached, levels } ->
        let
            maximum = foldl max 0 $ _.maximum <$> levels
            isDone = reached >= maximum
        in Tuple (vsFrom isDone) $
        Block
            ( HH.span_
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber maximum) (Int.toNumber reached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                    ]
                ]
            )
            $ renderLevelI reached <$> levels
    Prog.LevelsN { reached, levels } ->
        let
            maximum = foldl max 0.0 $ _.maximum <$> levels
            isDone = reached >= maximum
        in Tuple (vsFrom isDone) $
        Block
            ( HH.span_
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage maximum reached
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum reached <> "/" <> formatNum maximum
                    ]
                ]
            )
            $ renderLevelN reached <$> levels
    Prog.LevelsS { reached, levels } ->
        let
            maximum = Array.length levels
            isDone = reached >= maximum
        in Tuple (vsFrom isDone) $
        Block
            ( HH.span_
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber maximum) (Int.toNumber reached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                    ]
                ]
            )
            $ mapWithIndex (renderLevelS reached) levels
    Prog.LevelsE { reached, total } ->
        let
            isDone = reached >= total
        in Tuple (vsFrom isDone) $
        Inline $
            HH.span_
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber total) (Int.toNumber reached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt total <> "L"
                    ]
                ]
    Prog.LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } ->
        let
            isDone = levelReached >= totalLevels
        in Tuple (vsFrom isDone) $
        Inline $
            HH.span_
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber totalLevels) (Int.toNumber levelReached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor)
                            $ formatInt levelReached <> "/" <> formatInt totalLevels <> "L"
                            <> " : "
                            <> formatInt reachedAtCurrent <> "/" <> formatInt maximumAtCurrent
                    ]
                ]
    Prog.LevelsP { levels } ->
        let
            maximum = Array.length levels
            reached = Array.length $ Array.filter (_ == S.TDone) $ _.proc <$> levels
            isDone = reached >= maximum
        in Tuple (vsFrom isDone) $
        Block
                ( HH.span []
                    [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                    , qspacerSpan
                    , qitemnameSpan $ if isDone then completeColor else incompleteColor
                    , qspacerSpan
                    , qprogress $ HH.span_
                        [ percentage (Int.toNumber maximum) (Int.toNumber reached)
                        , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                        ]
                    ]
                )
            $ mapWithIndex renderLevelP levels
    Prog.LevelsO { reached, levels } ->
        -- TODO: we only know if we are done is when all maximum values are Just

        {- let
            maximum = foldl max 0 $ _.maximum <$> levels
            isDone = reached >= maximum
            in -}
        Tuple Neutral $ -- FIXME
            OnlyBlock $
            {- HH.div []
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan (if isDone then completeColor else incompleteColor) $ gitemName
                , qspacerSpan
                , percentage (Int.toNumber maximum) (Int.toNumber reached)
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                ]
                : -} renderLevelO reached <$> levels

    Prog.RelTime rel { hrs, min, sec } ->
        Tuple Neutral $
        Inline $ HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ HH.span_
                [ qcolorSpan measureColor $ relMarker rel
                , qthinspacerSpan
                , qcolorSpan timeColor $ CT.toLeadingZero hrs
                , qtimesplitSpan
                , qcolorSpan timeColor $ CT.toLeadingZero min
                , qtimesplitSpan
                , qcolorSpan timeColor $ CT.toLeadingZero sec
                ]
            ]
    where
        progress = CT.loadViewOrEdit voeProgress
        { itemName } = CT.loadViewOrEdit voeItemName
        qitemnameSpan color =
            if not editingName
                then qcolorSpan color itemName
                else mkValueEditInput events.onEditItemName $ _.itemName <$> voeItemName
        qprogress htmlWhenViewing =
            if not editingProgress
                then htmlWhenViewing
                else mkValueEditInput events.onEdit voeProgress
        mkValueEditInput :: forall a. (CT.EncodedValue -> i) -> CT.ViewOrEdit a -> H w i
        mkValueEditInput = EI.mkValueEditInput events
        editingName = CT.isEditing voeItemName
        editingProgress = CT.isEditing voeProgress
        relMarker = case _ of
            Prog.RMoreThan -> ">"
            Prog.REqual -> "="
            Prog.RLessThan -> "<"
        vsFrom true = Complete
        vsFrom false = Incomplete



percentage :: forall w i. Number -> Number -> H w i
percentage = percentage' progressBarCompleteColor progressBarIncompleteColor
percentage' :: forall w i. String -> String -> Number -> Number -> H w i
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

renderLevelI :: forall w i r. Int -> { maximum :: Int, name :: String | r } -> H w i
renderLevelI reached { maximum, name } =
    let
        isDone = reached >= maximum
    in HH.div
        [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
        [ HH.span_
            [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qcolorSpan (if isDone then completeColor else incompleteColor) name
            , qspacerSpan
            , percentage (Int.toNumber maximum) (min (Int.toNumber maximum) (Int.toNumber reached))
            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
            ]
        ]

renderLevelN :: forall w i r. Number -> { maximum :: Number, name :: String | r } -> H w i
renderLevelN reached { maximum, name } =
    let
        isDone = reached >= maximum
    in HH.div
        [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
        [ HH.span_
            [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qcolorSpan (if isDone then completeColor else incompleteColor) name
            , qspacerSpan
            , percentage maximum $ min reached maximum
            , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum reached <> "/" <> formatNum maximum
            ]
        ]

renderLevelS :: forall w i r x. Ord x => x -> x -> { gives :: String | r } -> H w i
renderLevelS reached levenN { gives } =
    let
        isDone = reached > levenN
    in HH.div
        [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
        [ HH.span_
            [ qlevelmarkerSpan $ if isDone then completeColor else incompleteColor
            , qspacerSpan
            , qcolorSpan (if isDone then completeColor else incompleteColor) gives
            , qspacerSpan
            , qcompleteCheckbox isDone
            ]
        ]

renderLevelO :: forall w i r. Int -> { mbMaximum :: Maybe Int, name :: String | r } -> H w i
renderLevelO reached { mbMaximum, name } =
    let
        isDone = case mbMaximum of
            Just maximum -> reached >= maximum
            Nothing -> false
    in HH.div
        [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
        [ HH.span_
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

renderLevelP :: forall w i r x. x -> { proc :: S.TaskP, name :: String | r } -> H w i
renderLevelP levelN { proc, name } =
    let
        isDone = proc == S.TDone
    in HH.div
        [ HP.style $ "margin-left: " <> show levelsMargin <> "px;" ]
        [ HH.span_
            [ qlevelmarkerSpan $ taskTextColor proc
            , qspacerSpan
            , qcolorSpan (taskTextColor proc) name
            , qspacerSpan
            , qtaskCheckbox proc
            ]
        ]