module Report.Web.Modifiers.Progress where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Int as Int
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array ((:))
import Data.Array as Array

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.KeyboardEvent as KE

import Report.Core as CT
import Report.Core.Logic as CT
import Report.Suffix (Key) as Suffix
import Report.Modifiers (class IsModifier)
import Report.Modifiers.Task (TaskP(..)) as S
import Report.Modifiers.Progress (Progress(..)) as Prog

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Report.Web.Helpers
import Report.Web.Modifiers.Task (qtaskCheckbox, taskTextColor)


type Events i =
    { onEdit :: CT.EncodedValue -> i
    , onEditItemName :: CT.EncodedValue -> i
    , onStartEditing :: MouseEvent -> i
    , onCancelEditing :: i
    , noop :: i
    }


-- TODO: CT.ViewOrEdit { itemName :: String }
-- TODO: CT.ViewOrEdit Prog.Progress
renderProgress :: forall w i. Events i -> CT.ViewOrEdit { itemName :: String } -> CT.ViewOrEdit Prog.Progress -> H w i
renderProgress events voeItemName voeProgress = case progress of
    Prog.None ->
        HH.span_
            [ qitemmarkerSpan incompleteColor
            , qspacerSpan
            , qitemnameSpan incompleteColor
            , qspacerSpan
            , qprogress $ qcolorSpan incompleteColor "<None>"
            ]
    Prog.Unknown ->
        HH.span_
            [ qitemmarkerSpan incompleteColor
            , qspacerSpan
            , qitemnameSpan incompleteColor
            , qspacerSpan
            , qprogress $ qcolorSpan incompleteColor "???"
            ]
    Prog.Error errorStr ->
        HH.span_
            [ qitemmarkerSpan genericColor
            , qspacerSpan
            , qitemnameSpan genericColor
            , qspacerSpan
            , qprogress $ qcolorSpan errorColor errorStr
            ]
    Prog.PText text ->
        HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ qcolorSpan textColor text
            ]
    Prog.PNumber num ->
        HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ qcolorSpan numColor $ formatNum num
            ]
    Prog.PInt num ->
        HH.span_
            [ qitemmarkerSpan completeColor
            , qspacerSpan
            , qitemnameSpan completeColor
            , qspacerSpan
            , qprogress $ qcolorSpan numColor $ formatInt num
            ]
    Prog.OnDate (CT.SDate { day, month, year }) ->
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        HH.span_
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
        in HH.div
            []
            $ HH.div []
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber maximum) (Int.toNumber reached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                    ]
                ]
            : (renderLevelI reached <$> levels)
    Prog.LevelsN { reached, levels } ->
        let
            maximum = foldl max 0.0 $ _.maximum <$> levels
            isDone = reached >= maximum
        in HH.div
            []
            $ HH.div []
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage maximum reached
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatNum reached <> "/" <> formatNum maximum
                    ]
                ]
            : (renderLevelN reached <$> levels)
    Prog.LevelsS { reached, levels } ->
        let
            maximum = Array.length levels
            isDone = reached >= maximum
        in HH.div
            []
            $ HH.div []
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber maximum) (Int.toNumber reached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                    ]
                ]
            : (mapWithIndex (renderLevelS reached) levels)
    Prog.LevelsE { reached, total } ->
        let
            isDone = reached >= total
        in HH.div []
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
        in HH.div []
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
        in HH.div
            []
            $ HH.div []
                [ qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qitemnameSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , qprogress $ HH.span_
                    [ percentage (Int.toNumber maximum) (Int.toNumber reached)
                    , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                    ]
                ]
            : (mapWithIndex renderLevelP levels)
    Prog.LevelsO { reached, levels } ->
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
                , qitemnameSpan (if isDone then completeColor else incompleteColor) $ gitemName
                , qspacerSpan
                , percentage (Int.toNumber maximum) (Int.toNumber reached)
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ formatInt reached <> "/" <> formatInt maximum
                ]
                : -} (renderLevelO reached <$> levels)
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
        editingName = CT.isEditing voeItemName
        editingProgress = CT.isEditing voeProgress
        mkValueEditInput :: forall a. (CT.EncodedValue -> i) -> CT.ViewOrEdit a -> H w i
        mkValueEditInput onEdit =
                maybe (HH.text "<EDIT>")
                    (\(CT.EncodedValue encVal) ->
                         HH.input
                            [ HP.type_ HP.InputText
                            , HP.value encVal
                            , HE.onClick events.onStartEditing
                            , HE.onValueChange (CT.EncodedValue >>> onEdit)
                            , HE.onKeyUp (KE.code >>> -- Debug.spy "key up" >>>
                                \code -> if code == "Escape"
                                    then events.onCancelEditing
                                    else events.noop)
                            , HE.onBlur $ const events.onCancelEditing
                            , HE.onAbort $ const events.onCancelEditing
                            ]
                    )
                    <<< CT.loadEncoded


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