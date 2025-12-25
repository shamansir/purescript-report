module Report.Progress where

import Prelude

import Foreign (F, Foreign)

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.FunctorWithIndex (mapWithIndex)

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)

import Report.Core as CT
import Report.Task (TaskP)

type DateRec = CT.SDateRec
type TimeRec = CT.STimeRec


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


{- JSON -}


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