module Data.Stats.Types where

import Prelude

import Input.CoreTypes as CT

import Foreign (F, Foreign)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Int as Int
import Data.String (joinWith, take, length) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Foldable (foldl, foldr)
import Data.Array (length, index) as Array

-- import Input.GameLog.Types as GLT

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)


type DateRec = { mon :: Int, day :: Int, year :: Int }
type TimeRec = { hrs :: Int, min :: Int, sec :: Int }


type LevelN = { maximum :: Number, name :: String, date :: Maybe DateRec }
type LevelI = { maximum :: Int, name :: String, date :: Maybe DateRec }
type LevelS = { gives :: String, date :: Maybe DateRec }
type LevelP = { name :: String, proc :: TaskP, date :: Maybe DateRec }
type LevelO = { mbMaximum :: Maybe Int, name :: String, date :: Maybe DateRec }


type LevelsN = { reached :: Number, levels :: Array LevelN }
type LevelsI = { reached :: Int, levels :: Array LevelI }
type LevelsS = { reached :: Int, levels :: Array LevelS }
type LevelsO = { reached :: Int, levels :: Array LevelO }
type LevelsE = { reached :: Int, total :: Int }
type LevelsC =
    { levelReached :: Int
    , totalLevels :: Int
    , reachedAtCurrent :: Int
    , maximumAtCurrent :: Int
    , date :: Maybe DateRec
    }
type LevelsP = { levels :: Array LevelP }


{- Progress -}


data Progress
    = None
    | Unknown
    | PText String
    | PNumber Number
    | PInt Int
    | ToComplete { done :: Boolean }
    | ToGetI { got :: Int, total :: Int }
    | ToGetN { got :: Number, total :: Number }
    | PercentI Int
    | PercentN Number
    | PercentSign { sign :: Int, pct :: Number }
    | MeasuredI { amount :: Int, measure :: String }
    | MeasuredN { amount :: Number, measure :: String }
    | MeasuredSign { sign :: Int, amount :: Number, measure :: String }
    | PerI { amount :: Int, per :: String }
    | PerN { amount :: Number, per :: String }
    | OnDate CT.SDate
    | OnTime TimeRec
    | RangeI { from :: Int, to :: Int }
    | RangeN { from :: Number, to :: Number }
    | Task TaskP
    | LevelsN LevelsN
    | LevelsI LevelsI
    | LevelsO LevelsO
    | LevelsS LevelsS
    | LevelsE LevelsE
    | LevelsC LevelsC
    | LevelsP LevelsP
    | Error String


instance Show Progress where show = progressToString
derive instance Eq Progress


data TaskP
    = TTodo
    | TDone
    | TDoing
    | TWait
    | TCanceled
    | TNow
    | TLater


instance Show TaskP where
    show = case _ of
        TTodo -> "TODO"
        TDone -> "DONE"
        TDoing -> "DOING"
        TWait -> "WAIT"
        TCanceled -> "CANCELED"
        TNow -> "NOW"
        TLater -> "LATER"
derive instance Eq TaskP


{- Stats -}


data Stats
    = SGotTotal { got :: Int, total :: Int }
    | SWithProgress { total :: Int, got :: Int, onTheWay :: Int }
    -- | SCompletionStatus GLT.Completion
    | SFromProgress Progress
    | SNotRelevant -- means everything is just values
    | SYetUnknown


derive instance Eq Stats
instance Ord Stats where
    compare statsA statsB = compare (weightOf statsA) (weightOf statsB)


defaultStats :: Stats
defaultStats = SYetUnknown


{- GroupPath -}


newtype PathSegment = PathSegment String
derive newtype instance ReadForeign PathSegment
derive newtype instance WriteForeign PathSegment

newtype GroupPath = GroupPath (Array PathSegment)


derive instance Newtype PathSegment _
derive instance Newtype GroupPath _
instance Show GroupPath where show = unwrap >>> map unwrap >>> String.joinWith "::"
derive newtype instance ReadForeign GroupPath
derive newtype instance WriteForeign GroupPath


instance Eq GroupPath where
    eq pathA pathB = (unwrap <$> unwrap pathA) == (unwrap <$> unwrap pathB)
instance Ord GroupPath where
    compare pathA pathB = compare (unwrap <$> unwrap pathA) (unwrap <$> unwrap pathB)


howDeep :: GroupPath -> Int
howDeep = unwrap >>> Array.length


pathToArray :: GroupPath -> Array String
pathToArray = unwrap >>> map unwrap


pathFromArray :: Array String -> GroupPath
pathFromArray = map wrap >>> wrap


startsWithNotEq :: GroupPath -> GroupPath -> Boolean
startsWithNotEq possibleStart sample =
    (howDeep possibleStart > 0) &&
    (howDeep sample > 0) &&
    (howDeep possibleStart < howDeep sample) &&
    ( let
        sampleArr = pathToArray sample
        possibleStartArr = pathToArray possibleStart
    in
        foldlWithIndex
            (\idx prev val -> prev && (Array.index sampleArr idx == Just val))
            true
            possibleStartArr
    )


{- NProgress -}


data NProgress
    = Achieved
    | OnTheWay Number -- FIXME: OnTheWay 1.0 == Achieved, OnTheWay 0.0 == NotAchieved
    | NotAchieved
    | StatsValue
    | Skip


-- data IProgress
--     = IAchieved
--     | IOnTheWay { got :: Int, total :: Int } -- FIXME: OnTheWay 1.0 == Achieved, OnTheWay 0.0 == NotAchieved
--     | INotAchieved
--     | IStatsValue
--     | ISkip


derive instance Eq NProgress


{- GotTotal -}


data GotTotal
    = Defined { got :: Int, total :: Int }
    | GTStatsValue
    | Undefined


derive instance Eq GotTotal
derive instance Ord GotTotal


{- Classes -}


progressToString :: Progress -> String
progressToString = case _ of
    None -> "<None>"
    Unknown -> "<Unknown>"
    PText text -> done_ <> " " <> text
    PNumber n -> done_ <> " " <> show n
    PInt n -> done_ <> " " <> show n
    ToComplete { done } -> if done then done_ else todo_
    ToGetI { got, total } ->
        let strRepr = show got <> "/" <> show total
        in if got == total then done_ <> " " <> strRepr else todo_ <> " " <> strRepr
    ToGetN { got, total } ->
        let strRepr = show got <> "/" <> show total
        in if got == total then done_ <> " " <> strRepr else todo_ <> " " <> strRepr
    PercentI i -> if i == 100 then done_ <> " " <> show i <> "%" else todo_ <> " " <> show i <> "%"
    PercentN n -> if n >= 1.00 then done_ <> " " <> show (n * 100.0) <> "%" else todo_ <> " " <> show n <> "%"
    PercentSign { sign, pct } ->
        let signChar = if sign >= 0 then "+" else "-"
        in
            if sign >= 0 && pct >= 1.00
                then done_ <> " " <> signChar <> show (pct * 100.0) <> "%"
                else todo_ <> " " <> signChar <> show pct <> "%"
    MeasuredI { amount, measure } -> done_ <> " " <> show amount <> measure
    MeasuredN { amount, measure } -> done_ <> " " <> show amount <> measure
    MeasuredSign { sign, amount, measure } ->
        let signChar = if sign >= 0 then "+" else "-"
        in
            done_ <> " " <> signChar <> show amount <> measure
    PerI { amount, per } -> done_ <> " " <> show amount <> "/" <> per
    PerN { amount, per } -> done_ <> " " <> show amount <> "/" <> per
    OnDate sdate -> done_ <> " @ " <> show sdate
    OnTime { hrs, min, sec } -> done_ <> " : " <> show hrs <> ":" <> show min <> ":" <> show sec
    RangeI { from, to } -> done_ <> " " <> show from <> "-" <> show to
    RangeN { from, to } -> done_ <> " " <> show from <> "-" <> show to
    Task taskP -> show taskP
    LevelsN { reached, levels } ->
        levelPrefix <> (String.joinWith levelSep $ showLevelN reached <$> levels)
    LevelsI { reached, levels } ->
        levelPrefix <> (String.joinWith levelSep $ showLevelI reached <$> levels)
    LevelsO { reached, levels } ->
        levelPrefix <> (String.joinWith levelSep $ showLevelO reached <$> levels)
    LevelsS { reached, levels } ->
        levelPrefix <> (String.joinWith levelSep $ mapWithIndex (showLevelS reached) levels)
    LevelsE { reached, total } ->
        -- 6/10L:200/200
        show reached <> "/" <> show total <> "L"
    LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } ->
        -- 6/10L:200/200
        show levelReached <> "/" <> show totalLevels <> "L" <> ":" <> show reachedAtCurrent <> "/" <> show maximumAtCurrent
    LevelsP { levels } ->
        levelPrefix <> (String.joinWith levelSep $ show <$> levels)
    Error err -> "ERROR: " <> show err
    where
        done_ = "[V]"
        todo_ = "[ ]"
        levelPrefix = ">> "
        levelSep = "\n.. "
        showLevelN :: Number -> LevelN -> String
        showLevelN reached { maximum, name } =
            if (reached >= maximum)
                then done_ <> " " <> show reached <> " / " <> show maximum <> " " <> name
                else todo_ <> " " <> show reached <> " / " <> show maximum <> " " <> name
        showLevelI :: Int -> LevelI -> String
        showLevelI reached { maximum, name } =
            if (reached >= maximum)
                then done_ <> " " <> show reached <> " / " <> show maximum <> " " <> name
                else todo_ <> " " <> show reached <> " / " <> show maximum <> " " <> name
        showLevelO :: Int -> LevelO -> String
        showLevelO reached { mbMaximum, name } =
            case mbMaximum of
                Just maximum ->
                    if (reached >= maximum)
                        then done_ <> " " <> show reached <> " / " <> show maximum <> " " <> name
                        else todo_ <> " " <> show reached <> " / " <> show maximum <> " " <> name
                Nothing ->
                    todo_ <> " " <> show reached <> " / ?" <> " " <> name
        showLevelS :: Int -> Int -> LevelS -> String
        showLevelS reached n { gives } =
            show n <> " : " <>  gives


weightOf :: Stats -> Number
weightOf = case _ of
    SGotTotal { got } -> Int.toNumber got -- total can be 0
    SWithProgress { got, onTheWay } -> Int.toNumber got + (Int.toNumber onTheWay * 0.2)  -- total can be 0
    SFromProgress progress ->
        case gotTotalFrom progress of
            Defined { got } -> Int.toNumber got
            _ -> -0.5
    -- CompletionStatus GLT.Beaten -> 1.0
    -- CompletionStatus GLT.Completed -> 1.0
    -- CompletionStatus _ -> 1.0
    -- SCompletionStatus _ -> 1.0
    SNotRelevant -> -1.0
    SYetUnknown -> -2.0


gotTotalFromStats :: Stats -> GotTotal
gotTotalFromStats = case _ of
    SGotTotal { got, total } -> Defined { got, total }
    SWithProgress { got, total } -> Defined { got, total }
    SFromProgress progress -> gotTotalFrom progress
    -- SCompletionStatus _ -> Undefined
    SNotRelevant -> GTStatsValue
    SYetUnknown -> Undefined


gotTotalFrom :: Progress -> GotTotal
gotTotalFrom =
    case _ of
        None -> Undefined
        Unknown -> Undefined
        PText _ -> GTStatsValue
        PNumber _ -> GTStatsValue
        PInt _ -> GTStatsValue
        MeasuredI _ -> GTStatsValue
        MeasuredN _ -> GTStatsValue
        MeasuredSign _ -> GTStatsValue
        PerI _ -> GTStatsValue
        PerN _ -> GTStatsValue
        OnDate _ -> GTStatsValue
        OnTime _ -> GTStatsValue
        RangeI _ -> GTStatsValue
        RangeN _ -> GTStatsValue
        ToComplete { done } -> Defined { got : if done then 1 else 0, total : 1 }
        Task task -> Defined { got : if task == TDone then 1 else 0, total : 1 }
        ToGetI { got, total } -> Defined { got, total }
        ToGetN { got, total } -> Defined { got : Int.floor got, total : Int.floor total }
        PercentI i -> Defined { got : i, total : 100 }
        PercentN n -> Defined { got : Int.floor $ n * 100.0 , total : 100 }
        PercentSign { pct, sign } ->
            Defined { got : if (sign > 0) then Int.floor $ pct * 100.0 else 0, total : 100 }
        LevelsN { reached, levels } ->
            let
                maximumTotal = foldl max 0.0 $ _.maximum <$> levels
            in
                Defined { got : Int.floor reached, total : Int.floor maximumTotal }
        LevelsI { reached, levels } ->
            let
                maximumTotal = foldl max 0 $ _.maximum <$> levels
            in
                Defined { got : reached, total : maximumTotal }
        LevelsO { reached, levels } ->
            let
                maximumTotal = foldl max 0 $ fromMaybe 0 <$> _.mbMaximum <$> levels
            in
                Defined { got : reached, total : maximumTotal }
        LevelsS { reached, levels } -> -- FIXME: reached `0` is both reached first level and reached nothing?
            Defined { got : reached, total : Array.length levels }
        LevelsE { reached, total } -> -- FIXME: reached `0` is both reached first level and reached nothing?
            Defined { got : reached, total : total }
        LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } -> -- FIXME: can be unfair
            Defined { got : levelReached, total : totalLevels }
        LevelsP { levels } ->
            Defined
                { got : foldl (+) 0 $ (\lvl -> if lvl == TDone then 1 else 0) <$> _.proc <$> levels
                , total : Array.length levels
                }
        Error _ -> Undefined


{- JSON -}


instance ReadForeign TaskP where
    readImpl f = (readImpl f :: F String) <#> taskPFromString

instance WriteForeign TaskP where
    writeImpl = writeImpl <<< taskPToString


taskPFromString :: String -> TaskP
taskPFromString = case _ of
    "TODO" -> TTodo
    "DOING" -> TDoing
    "DONE" -> TDone
    "WAIT" -> TWait
    "CANCELED" -> TCanceled
    "NOW" -> TNow
    "LATER" -> TLater
    _ -> TTodo


taskPToString :: TaskP -> String
taskPToString = case _ of
    TTodo -> "TODO"
    TDoing -> "DOING"
    TDone -> "DONE"
    TWait -> "WAIT"
    TCanceled -> "CANCELED"
    TNow -> "NOW"
    TLater -> "LATER"


newtype PValueTag = PValueTag String
type ProgressJsonRec = { t :: PValueTag, v :: Foreign }
newtype ProgressJson = ProgressJson ProgressJsonRec
derive newtype instance ReadForeign PValueTag
derive newtype instance WriteForeign PValueTag

instance ReadForeign ProgressJson where
    readImpl f = (readImpl f :: F ProgressJsonRec) <#> ProgressJson


instance ReadForeign Progress where
    readImpl f =  (readImpl f :: F ProgressJson) >>= _decodeProgress


_decodeProgress :: ProgressJson -> F Progress -- FIXME: use method below to return just `Progress` instead of `F Progress`
_decodeProgress (ProgressJson { t, v }) = _readProgress t v
{-
either
    (NEL.toUnfoldable
        >>> map renderForeignError
        >>> String.joinWith "; "
        >>> Error)
    identity
$ ME.runExcept $ _readProgress (PValueTag t) v
-}


instance WriteForeign Progress where
    writeImpl = writeImpl <<< _writeProgress


_writeProgress :: Progress -> ProgressJsonRec
_writeProgress = case _ of
    None ->        { t : pvt "E", v : writeImpl "" }
    Unknown ->     { t : pvt "UNK", v : writeImpl "" }
    PInt i ->      { t : pvt "I", v : writeImpl i }
    PNumber n ->   { t : pvt "N", v : writeImpl n }
    PText text ->  { t : pvt "T", v : writeImpl text }
    ToComplete { done } -> { t : pvt "D", v : writeImpl done }
    PercentI i ->  { t : pvt "PCTI", v : writeImpl i }
    PercentN n ->  { t : pvt "PCT", v : writeImpl n }
    PercentSign { sign, pct } ->
        { t : pvt "PCTX", v : writeImpl { sign, pct } }
    ToGetI { got, total } ->
        { t : pvt "PI", v : writeImpl { done : got, total } }
    ToGetN { got, total } ->
        { t : pvt "PD", v : writeImpl { done : got, total } }
    OnTime timeRec ->
        { t : pvt "TIME", v : writeImpl timeRec }
    OnDate sdate ->
        { t : pvt "DATE", v : writeImpl $ CT.dateToRec sdate }
    PerI { amount, per } ->
        { t : pvt "PERI", v : writeImpl { i : amount, per } }
    PerN { amount, per } ->
        { t : pvt "PERD", v : writeImpl { d : amount, per } }
    MeasuredI { amount, measure } ->
        { t : pvt "MESI", v : writeImpl { i : amount, measure } }
    MeasuredN { amount, measure } ->
        { t : pvt "MESD", v : writeImpl { d : amount, measure } }
    MeasuredSign { sign, amount, measure } ->
        { t : pvt "MESX", v : writeImpl { d : amount, measure, sign } }
    RangeI { from, to } ->
        { t : pvt "RNGI", v : writeImpl { from, to } }
    RangeN { from, to } ->
        { t : pvt "RNGD", v : writeImpl { from, to } }
    Task taskP ->
        { t : pvt "PROC", v : writeImpl taskP }
    LevelsI levelsI ->
        { t : pvt "LVLI", v : writeImpl levelsI }
    LevelsO levelsO ->
        { t : pvt "LVLO", v : writeImpl levelsO }
    LevelsN levelsN  ->
        { t : pvt "LVLD", v : writeImpl levelsN }
    LevelsS levelsS ->
        { t : pvt "LVLS", v : writeImpl levelsS }
    LevelsE levelsE ->
        { t : pvt "LVLE", v : writeImpl levelsE }
    LevelsP levelsP ->
        { t : pvt "LVLP", v : writeImpl levelsP }
    LevelsC levelsC ->
        { t : pvt "LVLC", v : writeImpl $ convertLevelsC levelsC }
    Error err ->
        { t : pvt "X", v : writeImpl err }
    where
        pvt = PValueTag
        convertLevelsC
          { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent, date } =
          { reached : levelReached
          , levels : totalLevels
          , current : reachedAtCurrent
          , maxcurrent : maximumAtCurrent
          , date
          }


_readProgress :: PValueTag -> Foreign -> F Progress
_readProgress (PValueTag atag) frgn =
    case atag of
        "E" -> pure None
        "UNK" -> pure Unknown
        "I" -> PInt <$> (readImpl frgn :: F Int)
        "N" -> PNumber <$> (readImpl frgn :: F Number)
        "F" -> PNumber <$> (readImpl frgn :: F Number)
        "T" -> PText <$> (readImpl frgn :: F String)
        "D" -> (\b -> ToComplete { done : b }) <$> (readImpl frgn :: F Boolean)
        "PCTI" -> PercentI <$> (readImpl frgn :: F Int)
        "PCT" -> PercentN <$> (readImpl frgn :: F Number)
        "PCTX" -> (\{ sign, pct } -> PercentSign { sign, pct }) <$> (readImpl frgn :: F { sign :: Int, pct :: Number })
        "PI" -> (\{ done, total } -> ToGetI { got : done, total }) <$> (readImpl frgn :: F { done :: Int, total :: Int })
        "PD" -> (\{ done, total } -> ToGetN { got : done, total }) <$> (readImpl frgn :: F { done :: Number, total :: Number })
        "TIME" -> OnTime <$> (readImpl frgn :: F TimeRec)
        "DATE" -> OnDate <$> CT.dateFromRec <$> (readImpl frgn :: F DateRec)
        "PERI" -> (\{ i, per } -> PerI { amount : i, per }) <$> (readImpl frgn :: F { i :: Int, per :: String })
        "PERD" -> (\{ d, per } -> PerN { amount : d, per }) <$> (readImpl frgn :: F { d :: Number, per :: String })
        "MESI" -> (\{ i, measure } -> MeasuredI { amount : i, measure }) <$> (readImpl frgn :: F { i :: Int, measure :: String })
        "MESD" -> (\{ d, measure } -> MeasuredN { amount : d, measure }) <$> (readImpl frgn :: F { d :: Number, measure :: String })
        "MESX" -> (\{ d, measure, sign } -> MeasuredSign { amount : d, measure, sign }) <$> (readImpl frgn :: F { d :: Number, measure :: String, sign :: Int })
        "RNGI" -> (\{ from, to } -> RangeI { from, to }) <$> (readImpl frgn :: F { from :: Int, to :: Int })
        "RNGD" -> (\{ from, to } -> RangeN { from, to }) <$> (readImpl frgn :: F { from :: Number, to :: Number })
        "PROC" -> Task <$> (readImpl frgn :: F TaskP)
        "LVLI" -> LevelsI <$> (readImpl frgn :: F LevelsI)
        "LVLD" -> LevelsN <$> (readImpl frgn :: F LevelsN)
        "LVLO" -> LevelsO <$> (readImpl frgn :: F LevelsO)
        "LVLS" -> LevelsS <$> (readImpl frgn :: F LevelsS)
        "LVLP" -> LevelsP <$> (readImpl frgn :: F LevelsP)
        "LVLC" -> LevelsC <$> convertLevelsC <$> (readImpl frgn :: F { reached :: Int, levels :: Int, current :: Int, maxcurrent :: Int, date :: Maybe DateRec })
        "X" -> Error <$> (readImpl frgn :: F String)
        _ -> pure None
    where
        convertLevelsC
          { reached, levels, current, maxcurrent, date } =
          { levelReached : reached
          , totalLevels : levels
          , reachedAtCurrent : current
          , maximumAtCurrent : maxcurrent
          , date
          }

{-
data Completion
    = NotSet
    | NotStarted
    | Beaten
    | Completed
    | Unfinished
    | Continuous
    | Dropped
    | Status String -}