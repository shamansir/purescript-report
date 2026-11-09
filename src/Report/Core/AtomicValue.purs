module Report.AtomicValue where

import Prelude

import Foreign (F, Foreign)

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Array (length) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Newtype (unwrap)
import Data.String (joinWith) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber) as Int

import Yoga.JSON (class WriteForeign, writeImpl, class ReadForeign, readImpl)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

import Utils.Data.WColor (WColor)
import Utils.Data.WColor (toHex) as WColor

import Report.Core (SDate(..), SDateRec, STimeRec, dateFromRec, dateToRec, formatWithCommas, formatNumWithCommas, monthThreeLetter, toLeadingZero, Relation(..), encodeRel, decodeRel) as RCT
import Report.Core.ProgressRefl (NProgress(..), NormalizedProgress) as RP
import Report.Core.Logic (foreignToMaybe) as RCT
-- import Report.Decorators.Progress (Progress(..), Relation(..), encodeRel, decodeRel, NProgress(..)) as RP
-- import Report.Decorators.Tabular.TabularValue (TabularAtomicValue) as RAV
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Task (taskPToString, toNProgress) as RT
import Report.Decorators.Rating (Rating)
import Report.Decorators.Rating (toStars, toString) as Rating
import Report.Decorators.Priority (Priority)
import Report.Decorators.Priority (priorityChar, toString) as Priority
import Report.Chain (Chain)
import Report.Chain (toString, toStringSep) as Chain

-- import Report.Decorators.Task (isDone) as TaskP


data AtomicValue
    = None
    | Unknown
    | PText String
    | PNumber Number
    | PInt Int
    | ToComplete { done :: Boolean }
    | ToGetI { got :: Int, total :: Int }
    | ToGetN { got :: Number, total :: Number }
    | ToGetIX { got :: Int, doing :: Int, total :: Int }
    | ToGetNX { got :: Number, doing :: Number, total :: Number }
    | ToGetMI { min :: Int, got :: Int, max :: Int } -- not the same as got / doing / total, where min == 0, total == max
    | ToGetMN { min :: Number, got :: Number, max :: Number } -- not the same as got / doing / total, where min == 0.0, total == max
    | ToGetMIX { min :: Int, got :: Int, doing :: Int, max :: Int }
    | ToGetMNX { min :: Number, got :: Number, doing :: Number, max :: Number }
    | PercentI Int
    | PercentN Number
    | PercentSign { sign :: Int, pct :: Number }
    | MeasuredI { amount :: Int, measure :: String }
    | MeasuredN { amount :: Number, measure :: String }
    | MeasuredSign { sign :: Int, amount :: Number, measure :: String }
    | PerI { amount :: Int, per :: String }
    | PerN { amount :: Number, per :: String }
    | OnDate RCT.SDate
    | OnTime RCT.STimeRec
    | OnDateTime RCT.SDate RCT.STimeRec
    | TimeRange { from :: RCT.STimeRec, to :: RCT.STimeRec }
    | DateRange { from :: RCT.SDate,    to :: RCT.SDate }
    | DateTimeRange
        { from :: { date :: RCT.SDate, time :: RCT.STimeRec }
        , to   :: { date :: RCT.SDate, time :: RCT.STimeRec }
        }
    | RangeI { from :: Int, to :: Int }
    | RangeN { from :: Number, to :: Number }
    | RelTime RCT.Relation RCT.STimeRec
    | RelInt RCT.Relation Int
    | RelNum RCT.Relation Number
    | PTask TaskP
    | PPath (Chain String)
    | Tags (Array (Chain String))
    | PColor WColor
    | PRating Rating
    | PPriority Priority
    | Error String


derive instance Generic AtomicValue _
derive instance Eq AtomicValue
instance WriteForeign AtomicValue where
    writeImpl = writeImpl <<< _writeAtomicValue
instance ReadForeign AtomicValue where
    readImpl f = (readImpl f :: F AtomicValueJson) >>= _decodeAtomicValue
instance EncodeJson AtomicValue where encodeJson = genericEncodeJson
instance DecodeJson AtomicValue where decodeJson = genericDecodeJson


data AtomicValueTag
    = AVTNone
    | AVTUnknown
    | AVTPText
    | AVTPNumber
    | AVTPInt
    | AVTToComplete
    | AVTToGetI
    | AVTToGetN
    | AVTToGetIX
    | AVTToGetNX
    | AVTToGetMI
    | AVTToGetMN
    | AVTToGetMIX
    | AVTToGetMNX
    | AVTPercentI
    | AVTPercentN
    | AVTPercentSign
    | AVTMeasuredI
    | AVTMeasuredN
    | AVTMeasuredSign
    | AVTPerI
    | AVTPerN
    | AVTOnDate
    | AVTOnTime
    | AVTOnDateTime
    | AVTTimeRange
    | AVTDateRange
    | AVTDateTimeRange
    | AVTRangeI
    | AVTRangeN
    | AVTRelTime
    | AVTRelInt
    | AVTRelNum
    | AVTPTask
    | AVTPPath
    | AVTTags
    | AVTColor
    -- | AVTTaskRef
    | AVTPRating
    | AVTPPriority
    | AVTError



newtype AVStrValueTag = AVStrValueTag String
type AtomicValueJsonRec = { t :: AVStrValueTag, v :: Foreign }
newtype AtomicValueJson = AtomicValueJson AtomicValueJsonRec
derive newtype instance Eq  AVStrValueTag
-- derive newtype instance Ord AVStrValueTag
derive newtype instance Show AVStrValueTag
derive newtype instance ReadForeign  AVStrValueTag
derive newtype instance WriteForeign AVStrValueTag
derive newtype instance ReadForeign  AtomicValueJson
derive newtype instance WriteForeign AtomicValueJson


type DateRec = RCT.SDateRec
type TimeRec = RCT.STimeRec


{-
fromProgress :: RP.Progress -> AtomicValue /\ Array { name :: String, reached :: ReachedOrNot, v :: Maybe AtomicValue }
fromProgress = case _ of
    RP.None -> None /\ []
    RP.Unknown -> Unknown /\ []
    RP.PText text -> PText text /\ []
    RP.PNumber pnumber -> PNumber pnumber /\ []
    RP.PInt pint -> PInt pint /\ []
    RP.ToComplete tocomplete -> ToComplete tocomplete /\ []
    RP.ToGetI togeti -> ToGetI togeti /\ []
    RP.ToGetN togetn -> ToGetN togetn /\ []
    -- RP.ToGetIX togetix -> ToGetIX togetix /\ []
    -- RP.ToGetNX togetnx -> ToGetNX togetnx /\ []
    RP.PercentI percenti -> PercentI percenti /\ []
    RP.PercentN percentn -> PercentN percentn /\ []
    RP.PercentSign percentsign -> PercentSign percentsign /\ []
    RP.MeasuredI measuredi -> MeasuredI measuredi /\ []
    RP.MeasuredN measuredn -> MeasuredN measuredn /\ []
    RP.MeasuredSign measuredsign -> MeasuredSign measuredsign /\ []
    RP.PerI peri -> PerI peri /\ []
    RP.PerN pern -> PerN pern /\ []
    RP.OnDate ondate -> OnDate ondate /\ []
    RP.OnTime ontime -> OnTime ontime /\ []
    -- RP.OnDateTime ondatetime -> OnDateTime ondatetime /\ []
    -- RP.TimeRange timerange -> TimeRange timerange /\ []
    -- RP.DateRange daterange -> DateRange daterange /\ []
    -- RP.DateTimeRange datetimerange -> DateTimeRange datetimerange /\ []
    RP.RangeI rangei -> RangeI rangei /\ []
    RP.RangeN rangen -> RangeN rangen /\ []
    RP.RelTime rel time -> RelTime rel time /\ []
    RP.Task taskP -> PTask taskP /\ []
    RP.LevelsI levelsI -> levelsIVal levelsI /\ mapWithIndex (mapLevelI levelsI.reached) levelsI.levels
    RP.LevelsN levelsN -> levelsNVal levelsN /\ mapWithIndex (mapLevelN levelsN.reached) levelsN.levels
    RP.LevelsO levelsO -> levelsOVal levelsO /\ mapWithIndex (mapLevelO levelsO.reached) levelsO.levels
    RP.LevelsS levelsS -> levelsSVal levelsS /\ mapWithIndex (mapLevelS levelsS.reached) levelsS.levels
    RP.LevelsE levelsE -> levelsEVal levelsE /\ []
    RP.LevelsC levelsC -> levelsCVal levelsC /\ pure (levelCHelper levelsC)
    RP.LevelsP levelsP -> levelsPVal levelsP /\ mapWithIndex mapLevelP levelsP.levels
    RP.Error error -> Error error /\ []
    where

        levelsIVal { levels, reached } = ToGetI { got : foldl (\g lmax -> if reached >= lmax then g + 1 else g) 0 $ _.maximum <$> levels, total : Array.length levels }
        levelsNVal { levels, reached } = ToGetI { got : foldl (\g lmax -> if reached >= lmax then g + 1 else g) 0 $ _.maximum <$> levels, total : Array.length levels }
        levelsEVal {  total, reached } = ToGetI { got : reached, total }
        levelsSVal { levels, reached } = ToGetI { got : reached, total : Array.length levels }
        levelsCVal { levelReached, totalLevels }
                                       = ToGetI { got : levelReached, total : totalLevels }
        levelsPVal { levels }          = ToGetI { got : foldl (\g lproc -> if isTaskPDone lproc then g + 1 else g) 0 $ _.proc <$> levels, total : Array.length levels }
        levelsOVal { levels, reached } = let mbMaxs = _.mbMaximum <$> levels in
                                         ToGetIX { got : foldl (levelOGotFoldF reached) 0 mbMaxs, doing : foldl (levelODoingFoldF reached) 0 mbMaxs, total : Array.length levels }

        mapLevelI reached idx levelI = { name : levelI.name,  reached : if reached >= levelI.maximum then levelI.date # maybe Reached ReachedAt else NotReached, v : Just $ PInt levelI.maximum }
        mapLevelN reached idx levelN = { name : levelN.name,  reached : if reached >= levelN.maximum then levelN.date # maybe Reached ReachedAt else NotReached, v : Just $ PNumber levelN.maximum }
        mapLevelO reached idx levelO = { name : levelO.name,  reached : levelOHelper reached levelO.mbMaximum levelO.date, v : PInt <$> levelO.mbMaximum }
        mapLevelS reached idx levelS = { name : levelS.gives, reached : if idx < reached then levelS.date # maybe Reached ReachedAt else NotReached, v : Nothing }
        mapLevelP idx levelP         = { name : levelP.name,  reached : levelPHelper levelP.proc levelP.date, v : Just $ PTask levelP.proc }

        levelOGotFoldF   reached g lmbmax = if maybe false (reached >= _) lmbmax then g + 1 else g
        levelODoingFoldF reached d lmbmax = maybe (d + 1) (\max -> d) lmbmax -- FIXME: we count only Nothing as "doing"

        levelOHelper reached mbMax mbDate =
            case mbMax, mbDate of
                Just max, Just date -> if reached >= max then ReachedAt date else NotReached
                Nothing,  Just date -> ReachedAt date
                Just max, Nothing   -> if reached >= max then Reached else NotReached
                Nothing,  Nothing   -> RUnknown

        levelCHelper { reachedAtCurrent, maximumAtCurrent, date } =
            { name : "Current"
            , reached : if reachedAtCurrent >= maximumAtCurrent then date # maybe Reached ReachedAt else NotReached
            , v : Just $ ToGetI { got : reachedAtCurrent, total : maximumAtCurrent }
            }

        levelPHelper taskP mbDate =
            if isTaskPDone taskP then mbDate # maybe Reached ReachedAt
            else if isTaskPDoing taskP then InProgress
            else NotReached

        isTaskPDone = case _ of
            TDone -> true
            _ -> false

        isTaskPDoing = case _ of
            TDoing -> true
            _ -> false
-}


rawToAtomicValueJson :: { t :: String, v :: Foreign } -> AtomicValueJson
rawToAtomicValueJson { t, v } = AtomicValueJson { t : AVStrValueTag t, v }


_writeAtomicValue :: AtomicValue -> AtomicValueJsonRec
_writeAtomicValue aval = case aval of
    None ->        { t : avtag, v : writeImpl "" }
    Unknown ->     { t : avtag, v : writeImpl "" }
    PInt i ->      { t : avtag, v : writeImpl i }
    PNumber n ->   { t : avtag, v : writeImpl n }
    PText text ->  { t : avtag, v : writeImpl text }
    ToComplete { done } -> { t : avtag, v : writeImpl done }
    PercentI i ->  { t : avtag, v : writeImpl i }
    PercentN n ->  { t : avtag, v : writeImpl n }
    PercentSign { sign, pct } ->
        { t : avtag, v : writeImpl { sign, pct } }
    ToGetI { got, total } ->
        { t : avtag, v : writeImpl { done : got, total } }
    ToGetN { got, total } ->
        { t : avtag, v : writeImpl { done : got, total } }
    OnTime timeRec ->
        { t : avtag, v : writeImpl timeRec }
    OnDate sdate ->
        { t : avtag, v : writeImpl $ RCT.dateToRec sdate }
    PerI { amount, per } ->
        { t : avtag, v : writeImpl { i : amount, per } }
    PerN { amount, per } ->
        { t : avtag, v : writeImpl { d : amount, per } }
    MeasuredI { amount, measure } ->
        { t : avtag, v : writeImpl { i : amount, measure } }
    MeasuredN { amount, measure } ->
        { t : avtag, v : writeImpl { d : amount, measure } }
    MeasuredSign { sign, amount, measure } ->
        { t : avtag, v : writeImpl { d : amount, measure, sign } }
    RangeI { from, to } ->
        { t : avtag, v : writeImpl { from, to } }
    RangeN { from, to } ->
        { t : avtag, v : writeImpl { from, to } }
    PTask taskP ->
        { t : avtag, v : writeImpl taskP }
    PPath path ->
        { t : avtag, v : writeImpl path }
    RelTime rel timeRec ->
        { t : avtag, v : writeImpl { rel : RCT.encodeRel rel, focus : timeRec } }
    RelInt rel int ->
        { t : avtag, v : writeImpl { rel : RCT.encodeRel rel, focus : int } }
    RelNum rel num ->
        { t : avtag, v : writeImpl { rel : RCT.encodeRel rel, focus : num } }
    PColor color ->
        { t : avtag, v : writeImpl color }
    Error err ->
        { t : avtag, v : writeImpl err }

    ToGetIX { got, doing, total } ->
        { t : avtag, v : writeImpl { done : got, doing, total } }
    ToGetNX { got, doing, total } ->
        { t : avtag, v : writeImpl { done : got, doing, total } }
    ToGetMI { min, got, max } ->
        { t : avtag, v : writeImpl { min, done : got, max } }
    ToGetMN { min, got, max } ->
        { t : avtag, v : writeImpl { min, done : got, max } }
    ToGetMIX { min, got, doing, max } ->
        { t : avtag, v : writeImpl { min, done : got, doing, max } }
    ToGetMNX { min, got, doing, max } ->
        { t : avtag, v : writeImpl { min, done : got, doing, max } }
    OnDateTime sdate timeRec ->
        { t : avtag, v : writeImpl { date : RCT.dateToRec sdate, time : timeRec } }
    TimeRange { from, to } ->
        { t : avtag, v : writeImpl { from : from, to : to } }
    DateRange { from, to } ->
        { t : avtag, v : writeImpl { from : RCT.dateToRec from, to : RCT.dateToRec to } }
    DateTimeRange { from, to } ->
        { t : avtag
        , v : writeImpl
            { from : { date : RCT.dateToRec from.date, time : from.time }
            , to :   { date : RCT.dateToRec to.date,   time : to.time }
            }
        }

    Tags tags -> { t : avtag, v : writeImpl tags }
    -- TaskRef ref -> { t : avtag, v : writeImpl ref }
    PRating rating -> { t : avtag, v : writeImpl rating }
    PPriority priority -> { t : avtag, v : writeImpl priority }

    where
        avtag = encodeAtomicValueTag $ valueTagOf aval


_readAtomicValue :: AtomicValueTag -> Foreign -> F AtomicValue
_readAtomicValue atag frgn =
    case atag of
        AVTNone -> pure None
        AVTUnknown -> pure Unknown
        AVTPInt -> PInt <$> (readImpl frgn :: F Int)
        AVTPNumber -> PNumber <$> (readImpl frgn :: F Number)
        AVTPText -> PText <$> (readImpl frgn :: F String)
        AVTToComplete -> (\b -> ToComplete { done : b }) <$> (readImpl frgn :: F Boolean)
        AVTPercentI -> PercentI <$> (readImpl frgn :: F Int)
        AVTPercentN  -> PercentN <$> (readImpl frgn :: F Number)
        AVTPercentSign -> (\{ sign, pct } -> PercentSign { sign, pct })    <$> (readImpl frgn :: F { sign :: Int, pct :: Number })
        AVTToGetI -> (\{ done, total } -> ToGetI { got : done, total }) <$> (readImpl frgn :: F { done :: Int, total :: Int })
        AVTToGetN -> (\{ done, total } -> ToGetN { got : done, total }) <$> (readImpl frgn :: F { done :: Number, total :: Number })
        AVTOnTime -> OnTime <$> (readImpl frgn :: F TimeRec)
        AVTOnDate -> OnDate <$> RCT.dateFromRec <$> (readImpl frgn :: F DateRec)
        AVTPerI -> (\{ i, per } -> PerI { amount : i, per }) <$> (readImpl frgn :: F { i :: Int, per :: String })
        AVTPerN -> (\{ d, per } -> PerN { amount : d, per }) <$> (readImpl frgn :: F { d :: Number, per :: String })
        AVTMeasuredI -> (\{ i, measure } -> MeasuredI { amount : i, measure }) <$> (readImpl frgn :: F { i :: Int, measure :: String })
        AVTMeasuredN -> (\{ d, measure } -> MeasuredN { amount : d, measure }) <$> (readImpl frgn :: F { d :: Number, measure :: String })
        AVTMeasuredSign -> (\{ d, measure, sign } -> MeasuredSign { amount : d, measure, sign }) <$> (readImpl frgn :: F { d :: Number, measure :: String, sign :: Int })
        AVTRangeI -> (\{ from, to } -> RangeI { from, to }) <$> (readImpl frgn :: F { from :: Int, to :: Int })
        AVTRangeN -> (\{ from, to } -> RangeN { from, to }) <$> (readImpl frgn :: F { from :: Number, to :: Number })
        AVTPTask -> PTask <$> (readImpl frgn :: F TaskP)
        AVTPPath -> PPath <$> (readImpl frgn :: F (Chain String))
        AVTRelTime ->
            (readImpl frgn :: F { rel :: String, focus :: TimeRec }) <#> \{ rel, focus } -> RelTime (RCT.decodeRel rel) focus
        AVTRelInt ->
            (readImpl frgn :: F { rel :: String, focus :: Int }) <#> \{ rel, focus } -> RelInt (RCT.decodeRel rel) focus
        AVTRelNum ->
            (readImpl frgn :: F { rel :: String, focus :: Number }) <#> \{ rel, focus } -> RelNum (RCT.decodeRel rel) focus
        AVTColor ->
            (readImpl frgn :: F WColor) <#> PColor
        AVTError -> Error <$> (readImpl frgn :: F String)

        AVTToGetIX -> (\{ done, doing, total } -> ToGetIX { got : done, doing, total }) <$> (readImpl frgn :: F { done :: Int, doing :: Int, total :: Int })
        AVTToGetNX -> (\{ done, doing, total } -> ToGetNX { got : done, doing, total }) <$> (readImpl frgn :: F { done :: Number, doing :: Number,  total :: Number })
        AVTToGetMI -> (\{ done, min, max } -> ToGetMI { got : done, min, max }) <$> (readImpl frgn :: F { min :: Int, done :: Int, max :: Int })
        AVTToGetMN -> (\{ done, min, max } -> ToGetMN { got : done, min, max }) <$> (readImpl frgn :: F { min :: Number, done :: Number, max :: Number })
        AVTToGetMIX -> (\{ done, doing, min, max } -> ToGetMIX { got : done, min, max, doing }) <$> (readImpl frgn :: F { min :: Int, max :: Int, done :: Int, doing :: Int })
        AVTToGetMNX -> (\{ done, doing, min, max } -> ToGetMNX { got : done, min, max, doing }) <$> (readImpl frgn :: F { min :: Number, max :: Number, done :: Number, doing :: Number })
        AVTOnDateTime -> (\{ date, time } -> OnDateTime (RCT.dateFromRec date) time) <$> (readImpl frgn :: F { date :: DateRec, time :: TimeRec })
        AVTDateRange -> (\{ from, to } -> DateRange { from : RCT.dateFromRec from, to : RCT.dateFromRec from }) <$> (readImpl frgn :: F { from :: DateRec, to :: DateRec })
        AVTTimeRange -> (\{ from, to } -> TimeRange { from, to }) <$> (readImpl frgn :: F { from :: TimeRec, to :: TimeRec })
        AVTDateTimeRange ->
            (\{ from, to } ->
                DateTimeRange
                    { from : { date : RCT.dateFromRec from.date, time : from.time }
                    , to : { date : RCT.dateFromRec to.date, time : to.time }
                    }
            )
            <$> (readImpl frgn :: F { from :: { date :: DateRec, time :: TimeRec }, to :: { date :: DateRec, time :: TimeRec } })

        AVTTags -> Tags <$> (readImpl frgn :: F (Array (Chain String)))
        -- AVTTaskRef -> TaskRef <$> (readImpl frgn :: F TaskId)
        AVTPRating -> PRating <$> (readImpl frgn :: F Rating)
        AVTPPriority -> PPriority <$> (readImpl frgn :: F Priority)


_readAtomicValueS :: AVStrValueTag -> Foreign -> F AtomicValue
_readAtomicValueS astrtag frgn = decodeAtomicValueTag astrtag # case _ of
    Just avtag -> _readAtomicValue avtag frgn
    Nothing -> pure None


fromJson :: AtomicValueJson -> Maybe AtomicValue
fromJson = _decodeAtomicValue >>> RCT.foreignToMaybe


valueTagOf :: AtomicValue -> AtomicValueTag
valueTagOf = case _ of
    None ->          AVTNone
    Unknown ->       AVTUnknown
    PInt _ ->        AVTPInt
    PNumber _ ->     AVTPNumber
    PText _ ->       AVTPText
    ToComplete _ ->  AVTToComplete
    PercentI _ ->    AVTPercentI
    PercentN _ ->    AVTPercentN
    PercentSign _ -> AVTPercentSign
    ToGetI _ ->      AVTToGetI
    ToGetN _ ->      AVTToGetN
    OnTime _ ->      AVTOnTime
    OnDate _ ->      AVTOnDate
    PerI _ ->        AVTPerI
    PerN _ ->        AVTPerN
    MeasuredI _ ->   AVTMeasuredI
    MeasuredN _ ->   AVTMeasuredN
    MeasuredSign _ -> AVTMeasuredSign
    RangeI _ ->      AVTRangeI
    RangeN _ ->      AVTRangeN
    PTask _ ->       AVTPTask
    RelTime _ _ ->   AVTRelTime
    RelInt _ _ ->    AVTRelInt
    RelNum _ _ ->    AVTRelNum
    PColor _ ->      AVTColor
    Error _ ->       AVTError

    PPath _ ->       AVTPPath
    ToGetIX _ ->     AVTToGetIX
    ToGetNX _ ->     AVTToGetNX
    ToGetMI _ ->     AVTToGetMI
    ToGetMN _ ->     AVTToGetMN
    ToGetMIX _ ->    AVTToGetMIX
    ToGetMNX _ ->    AVTToGetMNX
    OnDateTime _ _ -> AVTOnDateTime
    TimeRange _ ->   AVTTimeRange
    DateRange _ ->   AVTDateRange
    DateTimeRange _ -> AVTDateTimeRange
    Tags _ ->        AVTTags
    -- TaskRef _ ->     AVTTaskRef
    PRating _ ->     AVTPRating
    PPriority _ ->   AVTPPriority


encodeAtomicValueTag :: AtomicValueTag -> AVStrValueTag
encodeAtomicValueTag = AVStrValueTag <<< case _ of
    AVTNone ->        "E"
    AVTUnknown ->     "UNK"
    AVTPInt ->        "I"
    AVTPNumber ->     "N"
    AVTPText ->       "T"
    AVTToComplete ->  "D"
    AVTPercentI ->    "PCTI"
    AVTPercentN ->    "PCT"
    AVTPercentSign -> "PCTX"
    AVTToGetI ->      "PI"
    AVTToGetN ->      "PN"
    AVTOnTime ->      "TIME"
    AVTOnDate ->      "DATE"
    AVTPerI ->        "PERI"
    AVTPerN ->        "PERN"
    AVTMeasuredI ->   "MESI"
    AVTMeasuredN ->   "MESN"
    AVTMeasuredSign -> "MESX"
    AVTRangeI ->      "RNGI"
    AVTRangeN ->      "RNGN"
    AVTPTask ->       "PROC"
    AVTPPath ->       "PATH"
    AVTRelTime ->     "RELT"
    AVTRelInt ->      "RELI"
    AVTRelNum ->      "RELD"
    AVTColor ->       "COL"
    AVTError ->       "X"

    AVTToGetIX ->     "PIX"
    AVTToGetNX ->     "PNX"
    AVTToGetMIX ->    "MIX"
    AVTToGetMNX ->    "MNX"
    AVTToGetMI ->     "MI"
    AVTToGetMN ->     "MN"
    AVTOnDateTime ->  "PDT"
    AVTTimeRange ->   "PTRNG"
    AVTDateRange ->   "PDRRNG"
    AVTDateTimeRange -> "PDTRNG"

    AVTTags ->        "TAGS"
    -- AVTTaskRef ->     "TREF"
    AVTPRating ->     "PRAT"
    AVTPPriority ->   "PPRI"


decodeAtomicValueTag :: AVStrValueTag -> Maybe AtomicValueTag
decodeAtomicValueTag (AVStrValueTag avstag) = case avstag of
    "E" -> Just AVTNone
    "UNK" -> Just AVTUnknown
    "I" -> Just AVTPInt
    "N" -> Just AVTPNumber
    "T" -> Just AVTPText
    "D" -> Just AVTToComplete
    "PCTI" -> Just AVTPercentI
    "PCT" -> Just AVTPercentN
    "PCTX" -> Just AVTPercentSign
    "PI" -> Just AVTToGetI
    "PD" -> Just AVTToGetN
    "PN" -> Just AVTToGetN
    "TIME" -> Just AVTOnTime
    "DATE" -> Just AVTOnDate
    "PERI" -> Just AVTPerI
    "PERN" -> Just AVTPerN
    "MESI" -> Just AVTMeasuredI
    "MESD" -> Just AVTMeasuredN
    "MESN" -> Just AVTMeasuredN
    "MESX" -> Just AVTMeasuredSign
    "RNGI" -> Just AVTRangeI
    "RNGN" -> Just AVTRangeN
    "RNGD" -> Just AVTRangeN
    "PROC" -> Just AVTPTask
    "RELT" -> Just AVTRelTime
    "RELI" -> Just AVTRelInt
    "RELD" -> Just AVTRelNum
    "COL"  -> Just AVTColor
    "X" -> Just AVTError

    "PIX" -> Just AVTToGetIX
    "PNX" -> Just AVTToGetNX
    "MIX" -> Just AVTToGetMIX
    "MNX" -> Just AVTToGetMNX
    "MI" -> Just AVTToGetMI
    "MN" -> Just AVTToGetMN
    "PDT" -> Just AVTOnDateTime
    "PTRNG" -> Just AVTTimeRange
    "PDRRNG" -> Just AVTDateRange
    "PDTRNG" -> Just AVTDateTimeRange

    "TAGS" -> Just AVTTags
    -- "TREF" -> Just AVTTaskRef
    "PRAT" -> Just AVTPRating
    "PPRI" -> Just AVTPPriority

    _ -> Nothing


_decodeAtomicValue :: AtomicValueJson -> F AtomicValue
_decodeAtomicValue (AtomicValueJson { t, v }) = _readAtomicValueS t v
{-
either
    (NEL.toUnfoldable
        >>> map renderForeignError
        >>> String.joinWith "; "
        >>> Error)
    identity
$ ME.runExcept $ _readProgress (PValueTag t) v
-}

toReadableString :: AtomicValue -> String
toReadableString = case _ of
    None -> "-"
    Unknown -> "?"
    PText t -> t
    PInt i -> fwc i
    PNumber n -> fnwc n
    ToComplete { done } -> if done then "[V]" else "[X]"
    ToGetI { got, total } -> fwc got <> "/" <> fwc total
    ToGetN { got, total } -> fnwc got <> "/" <> fnwc total
    ToGetIX { got, doing, total } -> fwc got <> "/" <> fwc doing <> "/" <> fwc total
    ToGetNX { got, doing, total } -> fnwc got <> "/" <> fnwc doing <> "/" <> fnwc total
    ToGetMI { min, got, max } -> fwc min <> "--" <> fwc got <> "--" <> fwc max
    ToGetMN { min, got, max } -> fnwc min <> "--" <> fnwc got <> "--" <> fnwc max
    ToGetMIX { min, got, doing, max } -> fwc min <> "--" <> fwc got <> "(" <> fwc doing <> ")" <> "--" <> fwc max
    ToGetMNX { min, got, doing, max } -> fnwc min <> "--" <> fnwc got <> "(" <> fnwc doing <> ")" <>  "--" <> fnwc max
    PercentI i -> fwc i <> "%"
    PercentN n -> fnwc2 (n * 100.0) <> "%"
    PercentSign { sign, pct } -> ssign sign <> fnwc pct <> "%"
    MeasuredI { amount, measure } -> fwc amount <> measure
    MeasuredN { amount, measure } -> fnwc amount <> measure
    MeasuredSign { sign, amount, measure } -> ssign sign <> fnwc amount <> measure
    PerI { amount, per } -> fwc amount <> "/" <> per
    PerN { amount, per } -> fnwc amount <> "/" <> per
    RangeI { from, to } -> fwc from <> "—" <> fwc to
    RangeN { from, to } -> fnwc from <> "—" <> fnwc to
    OnDate date -> sdate date
    OnTime time -> stime time
    OnDateTime date time -> sdate date <> " " <> stime time
    TimeRange { from, to } -> stime from <> "—" <> stime to
    DateRange { from, to } -> sdate from <> "—" <> sdate to
    DateTimeRange { from, to } -> sdate from.date <> " " <> stime from.time <> "—" <> sdate to.date <> " " <> stime to.time
    PTask taskP -> RT.taskPToString taskP
    PPath path -> Chain.toStringSep "::" path
    RelTime rel time -> srel rel <> " " <> stime time
    RelInt rel int -> srel rel <> " " <> fwc int
    RelNum rel num -> srel rel <> " " <> fnwc num
    Tags tags -> String.joinWith "," $ Chain.toStringSep "::" <$> tags
    -- TaskRef ref -> "{" <> show ref <> "}"
    PRating rating -> Rating.toStars rating -- Rating.toString rating
    PPriority priority -> "[#" <> Priority.priorityChar priority <> "]"
    PColor wcolor -> WColor.toHex wcolor
    Error err -> "ERR: " <> err
    where
        ssign n = if n >= 0 then "" else "-"
        sdate (RCT.SDate { day, month, year }) = lz day <> " " <> mtl month <> " " <> show year
        stime { hrs, min, sec } = lz hrs <> ":" <> lz min <> "." <> lz sec -- if sec == 0 then lz hrs <> ":" <> else
        lz    = RCT.toLeadingZero
        mtl   = RCT.monthThreeLetter
        fwc   = RCT.formatWithCommas
        fnwc  = RCT.formatNumWithCommas { fixedTo : Just 3 }
        fnwc2 = RCT.formatNumWithCommas { fixedTo : Just 2 }
        srel  = RCT.encodeRel


normalizeProgress :: AtomicValue -> Maybe RP.NormalizedProgress
normalizeProgress = case _ of
    ToGetI { got, total } ->
        if (total == 0) then zeroes else
        Just
            { got   : Int.toNumber got   / Int.toNumber total
            , doing : 0.0
            }
    ToGetN { got, total } ->
        if (total == 0.0) then zeroes else
        Just
            { got : got / total
            , doing : 0.0
            }
    ToGetIX { got, doing, total } ->
        if (total == 0) then zeroes else
        Just
            { got   : Int.toNumber got   / Int.toNumber total
            , doing : Int.toNumber doing / Int.toNumber total
            }
    ToGetNX { got, doing, total } ->
        if (total == 0.0) then zeroes else
        Just
            { got   : got   / total
            , doing : doing / total
            }
    ToGetMI { min, got, max } ->
        if (max - min == 0) then zeroes else
        Just
            { got   : Int.toNumber (got - min) / Int.toNumber (max - min)
            , doing : 0.0
            }
    ToGetMN { min, got, max } ->
        if (max - min == 0.0) then zeroes else
        Just
            { got   : (got - min) / (max - min)
            , doing : 0.0
            }
    ToGetMIX { min, got, doing, max } ->
        if (max - min == 0) then zeroes else
        Just
            { got   : Int.toNumber (got - min)   / Int.toNumber (max - min)
            , doing : Int.toNumber (doing - min) / Int.toNumber (max - min)
            }
    ToGetMNX { min, got, doing, max } ->
        if (max - min == 0.0) then zeroes else
        Just
            { got   : (got - min)   / (max - min)
            , doing : (doing - min) / (max - min)
            }
    PercentI int ->
        Just
            { got   : Int.toNumber int / 100.0
            , doing : 0.0
            }
    PercentN num ->
        Just
            { got   : num
            , doing : 0.0
            }
    PercentSign { pct, sign } ->
        Just
            { got   : (pct / 100.0)
            , doing : 0.0
            }
    ToComplete { done } ->
        Just $
            if done then
                { got : 1.0
                , doing : 0.0
                }
            else
                { got : 0.0
                , doing : 0.0
                }
    PTask pTask ->
        Just $
            if pTask == TDone then
                { got : 1.0
                , doing : 0.0
                }
            else if pTask == TDoing then
                { got : 0.5
                , doing : 0.0
                }
            else
                { got : 0.0
                , doing : 0.0
                }
    _ ->
        Nothing
    where
        zeroes = Just { got : 0.0, doing : 0.0 } -- mempty



toNProgress :: AtomicValue -> RP.NProgress -- use `normalizedProgress`?
toNProgress =
    case _ of
        None -> RP.Skip
        Unknown -> RP.Skip
        PText _ -> RP.StatsValue
        PNumber _ -> RP.StatsValue
        PInt _ -> RP.StatsValue
        MeasuredI _ -> RP.StatsValue
        MeasuredN _ -> RP.StatsValue
        MeasuredSign _ -> RP.StatsValue
        PerI _ -> RP.StatsValue
        PerN _ -> RP.StatsValue
        OnDate _ -> RP.StatsValue -- achieved on that date?
        OnTime _ -> RP.StatsValue
        OnDateTime _ _ -> RP.StatsValue  -- achieved on that date?
        RangeI _ -> RP.StatsValue
        RangeN _ -> RP.StatsValue
        TimeRange _ -> RP.StatsValue
        DateRange _ -> RP.StatsValue
        DateTimeRange _ -> RP.StatsValue
        RelTime _ _ -> RP.StatsValue
        RelInt _ _ -> RP.StatsValue
        RelNum _ _ -> RP.StatsValue
        PPath _ -> RP.StatsValue
        Tags _ -> RP.StatsValue
        PColor _ -> RP.StatsValue
        -- TaskRef _ -> RP.StatsValue
        PRating _ -> RP.StatsValue
        PPriority _ -> RP.StatsValue
        Error _ -> RP.StatsValue
        ToComplete { done } -> if done then RP.Achieved else RP.NotAchieved
        ToGetI { got, total }  -> if got >= total && total /=   0 then RP.Achieved else if got == 0   || total == 0   then RP.NotAchieved else RP.OnTheWay $ Int.toNumber got / Int.toNumber total
        ToGetN { got, total }  -> if got >= total && total /= 0.0 then RP.Achieved else if got == 0.0 || total == 0.0 then RP.NotAchieved else RP.OnTheWay $ got / total
        ToGetIX { got, total } -> if got >= total && total /=   0 then RP.Achieved else if got == 0   || total == 0   then RP.NotAchieved else RP.OnTheWay $ Int.toNumber got / Int.toNumber total
        ToGetNX { got, total } -> if got >= total && total /= 0.0 then RP.Achieved else if got == 0.0 || total == 0.0 then RP.NotAchieved else RP.OnTheWay $ got / total
        ToGetMI { min, got, max }  -> if got >= max && max /=   0 then RP.Achieved else if got == min   || max == 0   then RP.NotAchieved else RP.OnTheWay $ Int.toNumber (got - min) / Int.toNumber (max - min)
        ToGetMN { min, got, max }  -> if got >= max && max /= 0.0 then RP.Achieved else if got == min   || max == 0.0 then RP.NotAchieved else RP.OnTheWay $ (got - min) / (max - min)
        ToGetMIX { min, got, max } -> if got >= max && max /=   0 then RP.Achieved else if got == min   || max == 0   then RP.NotAchieved else RP.OnTheWay $ Int.toNumber (got - min) / Int.toNumber (max - min)
        ToGetMNX { min, got, max } -> if got >= max && max /= 0.0 then RP.Achieved else if got == min   || max == 0.0 then RP.NotAchieved else RP.OnTheWay $ (got - min) / (max - min)
        PercentI i -> if i >= 100 then RP.Achieved else if i == 0   then RP.NotAchieved else RP.OnTheWay $ Int.toNumber i / 100.0
        PercentN n -> if n >= 1.0 then RP.Achieved else if n == 0.0 then RP.NotAchieved else RP.OnTheWay n
        PercentSign { pct, sign } ->
            if (sign > 0) && (pct >= 1.0) then RP.Achieved
            else if (sign < 0) || (pct <= 0.0) then RP.NotAchieved
            else RP.OnTheWay $ if sign >= 0 then pct else 0.0
        PTask task -> RT.toNProgress task


collectAtomic :: Array RP.NProgress -> AtomicValue
collectAtomic items = ToGetIX $ foldl foldF { got : 0, doing : 0, total : 0 } items
    where
        foldF prev =
            case _ of
                RP.Achieved ->
                    prev { got = prev.got + 1, total = prev.total + 1 }
                RP.OnTheWay _ ->
                    prev { doing = prev.doing + 1, total = prev.total + 1 }
                RP.NotAchieved ->
                    prev { total = prev.total + 1 }
                RP.StatsValue ->
                    prev
                RP.Skip ->
                    prev
