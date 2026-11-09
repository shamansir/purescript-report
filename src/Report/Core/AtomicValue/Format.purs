module Report.AtomicValue.Format where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), maybe)

import Report.Core (Fmt(..), formatInt, formatNum, dateToRec, SDate(..), toLeadingZero, monthThreeLetter) as RC
import Report.Core (Relation(..)) as RP
import Report.Decorators.Task (taskPToString) as RT
import Report.AtomicValue as RAV


data SepKind
    = SKTimeHrsMin
    | SKTimeMinSec
    | SKPath
    | SKNumDecimal
    | SKNumThousands
    | SKTimeRange
    | SKDateRange
    | SKValRange
    | SKPer
    | SKGotTotal
    | SKMinMax
    | SKDateSep
    | SKDateTime
    | SKRelVal
    | SKTextVis


data TCSymbol
    = TCSPercentSign
    | TCSNumNegative
    | TCSRel Ordering
    -- | CSQuestion


data TCellStyle
    = TCSUnknown
    | TCSText
    | TCSDigitsInt
    | TCSDigitsNum

    | TCSMeasure
    | TCSPer

    | TCSDay
    | TCSMonth
    | TCSYear

    | TCSHour
    | TCSMinutes
    | TCSSeconds

    | TCSProgress

    | TCSError

    -- | TCSGot
    -- | TCSTotal
    -- | TCSDoing
    -- | TCSMinimum
    -- | TCSMaximum


-- data SpaceKind
--     = SKThin
--     | SKNomral


data CompleteStatus -- FIXME: Same as `Reached`, `NProgress` and many others
    = Completed
    | InProgress
    | NotStarted


data TCell
    = Span TCellStyle String
    | Separator SepKind
    | Symbol TCSymbol
    | WithStatus CompleteStatus (Array TCell)
    | Empty


formatTextual :: RAV.AtomicValue -> Array TCell -- TODO: can be used in `AV.toReadableString`
formatTextual = case _ of
    RAV.None ->
        pure Empty
    RAV.Unknown ->
        pure $ c_unk "?"
    RAV.PText text ->
        pure $ c_text text
    RAV.PInt int ->
        fmt_int int
    RAV.PNumber num ->
        fmt_num3 num
    RAV.PBoolean bool ->
        pure $ c_text $ if bool then "true" else "false"
    RAV.PYear year ->
        pure $ c_text $ show year -- not formatted as a number
    RAV.PID id ->
        pure $ c_text $ "#" <> show id -- not formatted as a number
    RAV.ToComplete { done } ->
        pure $ c_prog $ if done then "DONE" else "TODO" -- FIXME
    RAV.ToGetI { got, total } ->
        -- TODO: completed or not
        fmt_int got <> pure sep_gt <> fmt_int total
    RAV.ToGetN { got, total } ->
        -- fmt_num3 got <> pure sep_gt <> fmt_num3 total
        let status = if got >= total then Completed else if got <= 0.0 then NotStarted else InProgress
        in pure $ with_st status $ fmt_num3 got <> pure sep_gt <> fmt_num3 total
    RAV.ToGetIX { got, doing, total } ->
        -- TODO: completed or not
        fmt_int got <> pure sep_gt <> fmt_int doing <> pure sep_gt <> fmt_int total
    RAV.ToGetNX { got, doing, total } ->
        -- TODO: completed or not
        fmt_num3 got <> pure sep_gt <> fmt_num3 doing <> pure sep_gt <> fmt_num3 total
    RAV.ToGetMI { min, got, max } ->
        -- TODO: completed or not
        fmt_int min <> pure sep_mm <> fmt_int got <> pure sep_mm <> fmt_int max
    RAV.ToGetMN { min, got, max } ->
        -- TODO: completed or not
        fmt_num3 min <> pure sep_mm <> fmt_num3 got <> pure sep_mm <> fmt_num3 max
    RAV.ToGetMIX { min, got, doing, max } ->
        -- TODO: completed or not
        fmt_int min <> pure sep_mm <> fmt_int got <> pure sep_mm <> fmt_int doing <> pure sep_mm <> fmt_int max
    RAV.ToGetMNX { min, got, doing, max } ->
        -- TODO: completed or not
        fmt_num3 min <> pure sep_mm <> fmt_num3 got <> pure sep_mm <> fmt_num3 doing <> pure sep_mm <> fmt_num3 max
    RAV.PercentI int ->
        -- TODO: completed or not
        fmt_int int <> pure sym_pct
    RAV.PercentN num ->
        -- TODO: completed or not
        fmt_num3 num <> pure sym_pct
    RAV.PercentSign { pct, sign } ->
        -- TODO: completed or not
        mksign sign : fmt_num3 pct <> pure sym_pct
    RAV.MeasuredI { amount, measure } ->
        fmt_int amount <> pure (c_mes measure)
    RAV.MeasuredN { amount, measure } ->
        fmt_num3 amount <> pure (c_mes measure)
    RAV.MeasuredSign { sign, amount, measure } ->
        mksign sign : fmt_num3 amount <> pure (c_mes measure)
    RAV.PerI { amount, per } ->
        fmt_int amount <> pure sep_per <> pure (c_per per)
    RAV.PerN { amount, per } ->
        fmt_num3 amount <> pure sep_per <> pure (c_per per)
    RAV.OnDate date ->
        fmt_date date
    RAV.OnTime time ->
        fmt_time time
    RAV.OnDateTime date time ->
        fmt_date date <> pure sep_sdt <> fmt_time time
    RAV.TimeRange { from, to } ->
        fmt_time from <> pure sep_str <> fmt_time to
    RAV.DateRange { from, to } ->
        fmt_date from <> pure sep_sdr <> fmt_date to
    RAV.DateTimeRange { from, to } ->
        fmt_date from.date <> pure sep_sdt <> fmt_time from.time
        <> pure sep_sdr <>
        fmt_date to.date   <> pure sep_sdt <> fmt_time to.time
    RAV.RangeI { from, to } ->
        fmt_int from <> pure sep_svr <> fmt_int to
    RAV.RangeN { from, to } ->
        fmt_num3 from <> pure sep_svr <> fmt_num3 to
    RAV.RelTime rel time ->
        mkrel rel : pure sep_srv <> fmt_time time
    RAV.RelInt rel int ->
        mkrel rel : pure sep_srv <> fmt_int int
    RAV.RelNum rel num ->
        mkrel rel : pure sep_srv <> fmt_num3 num
    RAV.PTask task ->
        pure $ c_prog $ RT.taskPToString task
    RAV.PPath pathC ->
        pure Empty -- FIXME: implement
    RAV.Tags tags ->
        pure Empty -- FIXME: implement
    RAV.PRawTags rawTags ->
        pure Empty -- FIXME: implement
    RAV.PRating rating ->
        pure Empty -- FIXME: implement
    RAV.PPriority priority ->
        pure Empty -- FIXME: implement
    RAV.PColor color ->
        pure Empty -- FIXME: implement
    RAV.Error error ->
        pure $ c_err error
    where
        c_unk  = Span TCSUnknown
        c_text = Span TCSText
        c_mes  = Span TCSMeasure
        c_per  = Span TCSPer
        c_day  = Span TCSDay
        c_mon  = Span TCSMonth
        c_year = Span TCSYear
        c_hrs  = Span TCSHour
        c_min  = Span TCSMinutes
        c_sec  = Span TCSSeconds
        c_prog = Span TCSProgress
        c_err  = Span TCSError

        sep_gt  = Separator SKGotTotal
        sep_mm  = Separator SKMinMax
        sep_per = Separator SKPer
        sep_ds  = Separator SKDateSep
        sep_thm = Separator SKTimeHrsMin
        sep_tms = Separator SKTimeMinSec
        sep_sdt = Separator SKDateTime
        sep_sdr = Separator SKDateRange
        sep_str = Separator SKTimeRange
        sep_svr = Separator SKValRange
        sep_srv = Separator SKRelVal

        sym_pct = Symbol TCSPercentSign
        -- sym_q = Symbol CSQuestion

        with_st = WithStatus

        mksign s = if s < 0 then Symbol TCSNumNegative else Empty
        mkrel = Symbol <<< TCSRel <<< case _ of
            RP.RMoreThan -> GT
            RP.RLessThan -> LT
            RP.REqual -> EQ

        rcfmt_to_cell digits = case _ of
            RC.FmtDigits s -> Span digits s
            RC.FmtDelim -> Separator SKNumThousands
            RC.FmtDecimalSep -> Separator SKNumDecimal
            RC.FmtSign "-" -> Symbol TCSNumNegative
            RC.FmtSign _ -> Empty
            RC.FmtEmpty -> Empty

        fmt_int int  = rcfmt_to_cell TCSDigitsInt <$> RC.formatInt int
        fmt_num3 num = rcfmt_to_cell TCSDigitsNum <$> RC.formatNum { fixedTo : Just 3 } num

        fmt_date (RC.SDate { day, month, year })
            =  pure (c_day $ RC.toLeadingZero day)
            <> pure sep_ds
            <> pure (c_mon $ RC.monthThreeLetter month)
            <> pure sep_ds
            <> pure (c_year $ show year)

        fmt_time { hrs, min, sec }
            =  pure (c_hrs $ RC.toLeadingZero hrs)
            <> pure sep_thm
            <> pure (c_min $ RC.toLeadingZero min)
            <> pure sep_tms
            <> pure (c_sec $ RC.toLeadingZero sec)