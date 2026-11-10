module Report.Decorators.Progress where

import Prelude

import Foreign (F, Foreign, fail, ForeignError(..))

import Data.Generic.Rep (class Generic)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either)
import Data.String (joinWith) as String
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.Array (length, all) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap)

import Control.Alt ((<|>))

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)

import Report.Core (SDate, SDateRec, STimeRec, dateFromRec, dateToRec, Relation(..), encodeRel, decodeRel) as CT
import Report.Core.ProgressRefl (NProgress(..))
import Report.Core.ProgressRefl (ReachedOrNot(..)) as CT
import Report.Core.Logic (foreignToMaybe) as CT
import Report.Convert.Keyed as CK
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Levels (LevelI, LevelN, LevelO, LevelS, LevelsC, LevelsE, LevelsI, LevelsN, LevelsO, LevelsP, LevelsS)

import Report.AtomicValue as AV

type DateRec = CT.SDateRec
type TimeRec = CT.STimeRec


{- Progress -}


data Progress
    = AV AV.AtomicValue
    | LS Levels


data Levels
    = LevelsN LevelsN -- every level has number maximum, name and probably a date when was achieved, `reached` value is also a number and holds the scored value
    | LevelsI LevelsI -- every level has integer maximum, name and probably a date when was achieved, `reached` value is also an integerr and holds the scored value
    | LevelsO LevelsO -- every level has optional integer maximum (when known), name and probably a date when was achieved, reached value is also an integer
    | LevelsS LevelsS -- every level gives something textual, `reached` value is a level index
    | LevelsE LevelsE -- exact levels are unknown, we only have a value of levels achieved and total level count
    | LevelsC LevelsC -- exact levels are unknown, we only have a value of levels achieved, total level count, and a value reached at current level and maximum value at current level
    | LevelsP LevelsP -- every level has task inside, name and probably a date when was achieved, reached value is not stored and intended to be calculated from the task data



instance Show Progress where show = progressToString
derive instance Eq Levels
derive instance Eq Progress


{- Classes -}


_avToString :: AV.AtomicValue -> String -- FIXME: use `AV.toReadableString aval` everywhere
_avToString = case _ of
    AV.None -> "<None>"
    AV.Unknown -> "<Unknown>"
    AV.PText text -> done_ <> " " <> text
    AV.PNumber n -> done_ <> " " <> show n
    AV.PInt n -> done_ <> " " <> show n
    AV.ToComplete { done } -> if done then done_ else todo_
    AV.ToGetI { got, total } ->
        let strRepr = show got <> "/" <> show total
        in if got == total then done_ <> " " <> strRepr else todo_ <> " " <> strRepr
    AV.ToGetN { got, total } ->
        let strRepr = show got <> "/" <> show total
        in if got == total then done_ <> " " <> strRepr else todo_ <> " " <> strRepr
    AV.PercentI i -> if i == 100 then done_ <> " " <> show i <> "%" else todo_ <> " " <> show i <> "%"
    AV.PercentN n -> if n >= 1.00 then done_ <> " " <> show (n * 100.0) <> "%" else todo_ <> " " <> show n <> "%"
    AV.PercentSign { sign, pct } ->
        let signChar = if sign >= 0 then "+" else "-"
        in
            if sign >= 0 && pct >= 1.00
                then done_ <> " " <> signChar <> show (pct * 100.0) <> "%"
                else todo_ <> " " <> signChar <> show pct <> "%"
    AV.MeasuredI { amount, measure } -> done_ <> " " <> show amount <> measure
    AV.MeasuredN { amount, measure } -> done_ <> " " <> show amount <> measure
    AV.MeasuredSign { sign, amount, measure } ->
        let signChar = if sign >= 0 then "+" else "-"
        in
            done_ <> " " <> signChar <> show amount <> measure
    AV.PerI { amount, per } -> done_ <> " " <> show amount <> "/" <> per
    AV.PerN { amount, per } -> done_ <> " " <> show amount <> "/" <> per
    AV.OnDate sdate -> done_ <> " @ " <> show sdate
    AV.OnTime { hrs, min, sec } -> done_ <> " : " <> show hrs <> ":" <> show min <> ":" <> show sec
    AV.RangeI { from, to } -> done_ <> " " <> show from <> "-" <> show to
    AV.RangeN { from, to } -> done_ <> " " <> show from <> "-" <> show to
    AV.PTask taskP -> show taskP
    AV.RelTime rel { hrs, min, sec } -> relation rel <> " " <> show hrs <> ":" <> show min <> ":" <> show sec
    AV.Error err -> "ERROR: " <> show err
    aval -> AV.toReadableString aval
    where
        done_ = "[V]"
        todo_ = "[ ]"
        relation = case _ of
            CT.RMoreThan -> ">"
            CT.REqual -> "="
            CT.RLessThan -> "<"


_levelsToString :: Levels -> String
_levelsToString = case _ of
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


progressToString :: Progress -> String -- FIXME: replace with AtomicValue.Format
progressToString = case _ of
    AV avVal -> _avToString avVal
    LS levels -> _levelsToString levels


{- JSON -}


newtype PValueTag = PValueTag String
type ProgressJsonRec = { t :: PValueTag, v :: Foreign }
newtype ProgressJson = ProgressJson ProgressJsonRec
derive newtype instance Eq PValueTag
-- derive newtype instance Ord PValueTag
derive newtype instance Show PValueTag
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


instance CK.EncodableKey PValueTag where
    encodeKey (PValueTag str) = str
    decodeKey str = Just $ PValueTag str
    -- default = PValueTag "UNK"


instance CK.KeyedReadForeign PValueTag Progress where
    keyedReadImpl = _readProgress


rawToProgressJson :: { t :: String, v :: Foreign } -> ProgressJson
rawToProgressJson { t, v } = ProgressJson { t : PValueTag t, v }


instance WriteForeign Progress where
    writeImpl = writeImpl <<< _writeProgress


_writeLevels :: Levels -> ProgressJsonRec
_writeLevels = case _ of
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


_unsafeConvertAVStrTag :: AV.AVStrValueTag -> PValueTag
_unsafeConvertAVStrTag (AV.AVStrValueTag avStrTag) = PValueTag avStrTag


_writeProgress :: Progress -> ProgressJsonRec
_writeProgress = case _ of
    AV aval -> unsafeConvertAVTag $ AV._writeAtomicValue aval
    LS levels -> _writeLevels levels
    where
        unsafeConvertAVTag { t, v } = { t : _unsafeConvertAVStrTag t, v }


_readLevels :: PValueTag -> Foreign -> F Levels
_readLevels (PValueTag atag) frgn =
    case atag of
        "LVLI" -> LevelsI <$> (readImpl frgn :: F LevelsI)
        "LVLD" -> LevelsN <$> (readImpl frgn :: F LevelsN)
        "LVLO" -> LevelsO <$> (readImpl frgn :: F LevelsO)
        "LVLIO" -> LevelsO <$> convertLevelsIO <$> (readImpl frgn :: F { reached :: Int, levels :: Array { maximum :: Maybe Int, name :: String, date :: Maybe DateRec } }) -- legacy, from Dhall
        "LVLS" -> LevelsS <$> (readImpl frgn :: F LevelsS)
        "LVLP" -> LevelsP <$> (readImpl frgn :: F LevelsP)
        "LVLC" -> LevelsC <$> convertLevelsC <$> (readImpl frgn :: F { reached :: Int, levels :: Int, current :: Int, maxcurrent :: Int, date :: Maybe DateRec })
        other  -> fail $ ForeignError $ "unknown levels tag: " <> other
    where
        convertLevelsIO { reached, levels } =
          { reached
          , levels : (\{ maximum, name, date } -> { mbMaximum : maximum, name, date }) <$> levels
          }
        convertLevelsC
          { reached, levels, current, maxcurrent, date } =
          { levelReached : reached
          , totalLevels : levels
          , reachedAtCurrent : current
          , maximumAtCurrent : maxcurrent
          , date
          }


_readProgress :: PValueTag -> Foreign -> F Progress
_readProgress (PValueTag atag) frgn =
    (fAVTag >>= flip AV._readAtomicValue frgn <#> AV)
    <|>
    (_readLevels (PValueTag atag) frgn <#> LS)
    where
        mbAVTag = AV.decodeAtomicValueTag $ AV.AVStrValueTag atag
        fAVTag = maybe (fail $ ForeignError $ "unknown tag: " <> atag) pure mbAVTag


fromJson :: ProgressJson -> Maybe Progress
fromJson = _decodeProgress >>> CT.foreignToMaybe


valueTagOf :: Progress -> PValueTag
valueTagOf = case _ of
    AV aval ->
        AV.valueTagOf aval
        # AV.encodeAtomicValueTag
        # _unsafeConvertAVStrTag
    LS levels ->
        case levels of
            LevelsI _ ->     PValueTag "LVLI"
            LevelsN _ ->     PValueTag "LVLD"
            LevelsO _ ->     PValueTag "LVLO"
            LevelsS _ ->     PValueTag "LVLS"
            LevelsP _ ->     PValueTag "LVLP"
            LevelsC _ ->     PValueTag "LVLC"
            LevelsE _ ->     PValueTag "LVLE"


orderOf :: PValueTag -> Int
orderOf (PValueTag pvt) = case pvt of
    "I" -> 0
    "N" -> 1
    "T" -> 2
    "D" -> 3
    "PCTI" -> 4
    "PCT" -> 5
    "PCTX" -> 6
    "PI" -> 7
    "PD" -> 8
    "TIME" -> 9
    "DATE" -> 10
    "PERI" -> 11
    "PERD" -> 12
    "MESI" -> 13
    "MESD" -> 14
    "MESX" -> 15
    "RNGI" -> 16
    "RNGD" -> 17
    "PROC" -> 18
    "LVLI" -> 19
    "LVLD" -> 20
    "LVLO" -> 21
    "LVLS" -> 22
    "LVLP" -> 23
    "LVLC" -> 24
    "LVLE" -> 25
    "RELT" -> 26
    "X" -> 27
    _ -> 1000


inlineProgress :: Array PValueTag
inlineProgress =
    [ PValueTag "E"
    , PValueTag "UNK"
    , PValueTag "I"
    , PValueTag "N"
    , PValueTag "T"
    , PValueTag "D"
    , PValueTag "PCTI"
    , PValueTag "PCT"
    , PValueTag "PCTX"
    , PValueTag "PI"
    , PValueTag "PD"
    , PValueTag "TIME"
    , PValueTag "DATE"
    , PValueTag "PERI"
    , PValueTag "PERD"
    , PValueTag "MESI"
    , PValueTag "MESD"
    , PValueTag "MESX"
    , PValueTag "RNGI"
    , PValueTag "RNGD"
    , PValueTag "PROC"
    , PValueTag "RELT"
    , PValueTag "X"
    ]


leveledProgress :: Array PValueTag
leveledProgress =
    [ PValueTag "LVLI"
    , PValueTag "LVLD"
    , PValueTag "LVLO"
    , PValueTag "LVLS"
    , PValueTag "LVLP"
    , PValueTag "LVLC"
    , PValueTag "LVLE"
    ]


allValueTags :: Array PValueTag
allValueTags =
    inlineProgress <> leveledProgress


allValueTagsCount :: Int
allValueTagsCount = Array.length allValueTags


unwrapValueTag :: PValueTag -> String
unwrapValueTag (PValueTag t) = t


instance CK.Keyed PValueTag Progress where
    keyOf = valueTagOf


loadNProgress :: Progress -> NProgress
loadNProgress =
    case _ of
        AV aval -> AV.toNProgress aval
        LS levels -> case levels of
            LevelsN { reached, levels } ->
                let
                    maximumTotal = foldl max 0.0 $ _.maximum <$> levels
                in
                    if reached >= maximumTotal then Achieved
                    else if reached == 0.0 then NotAchieved
                    else OnTheWay $ reached / maximumTotal
            LevelsI { reached, levels } ->
                let
                    maximumTotal = foldl max 0 $ _.maximum <$> levels
                in
                    if reached >= maximumTotal then Achieved
                    else if reached == 0 then NotAchieved
                    else OnTheWay $ Int.toNumber reached / Int.toNumber maximumTotal
            LevelsO { reached, levels } ->
                Skip -- Some values for levels are Maybe so we don't know for sure if user achieved something
            LevelsS { reached, levels } -> -- FIXME: reached `0` is both reached first level and reached nothing?
                if reached + 1 >= Array.length levels then Achieved else if reached == 0 then NotAchieved else OnTheWay $ Int.toNumber reached / Int.toNumber (Array.length levels)
            LevelsE { reached, total } -> -- FIXME: reached `0` is both reached first level and reached nothing?
                if reached >= total then Achieved else if reached == 0 then NotAchieved else OnTheWay $ Int.toNumber reached / Int.toNumber total
            LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } -> -- FIXME: can be unfair
                if (levelReached == totalLevels) && (reachedAtCurrent == maximumAtCurrent) then Achieved
                else if (levelReached == 0) && (totalLevels /= 0) && (reachedAtCurrent == 0) then NotAchieved
                else OnTheWay $ Int.toNumber levelReached / Int.toNumber totalLevels -- FIXME: we don't add `reachedAtCurrent`
            LevelsP { levels } ->
                if Array.all (_ == TDone) $ _.proc <$> levels
                then Achieved
                else
                    let reached = foldl (+) 0 $ (\lvl -> if lvl == TDone then 1 else 0) <$> _.proc <$> levels
                    in if reached > 0 && Array.length levels > 0 then
                        OnTheWay $ Int.toNumber reached / Int.toNumber (Array.length levels)
                    else NotAchieved


nProgressToNumber :: NProgress -> Number
nProgressToNumber = case _ of
    Achieved -> 1.0
    OnTheWay n -> min 1.0 n
    NotAchieved -> 0.0
    StatsValue -> -1.0
    Skip -> -2.0


data PTValueTag -- FIXME: use it everywhere above for consistency
    = PTAV AV.AtomicValueTag
    | PTLevelsI
    | PTLevelsO
    | PTLevelsN
    | PTLevelsS
    | PTLevelsE
    | PTLevelsP
    | PTLevelsC


derive instance Generic PTValueTag _


_vtagFrom :: PValueTag -> PTValueTag
_vtagFrom (PValueTag str) = case str of
    -- "E" -> PTNone
    -- "UNK" -> PTUnknown
    -- "I" -> PTInt
    -- "N" -> PTNumber
    -- "F" -> PTNumber
    -- "T" -> PTText
    -- "D" -> PTToComplete
    -- "PCTI" -> PTPercentI
    -- "PCT"  -> PTPercentN
    -- "PCTX" -> PTPercentSign
    -- "PI" -> PTToGetI
    -- "PD" -> PTToGetN
    -- "TIME" -> PTOnTime
    -- "DATE" -> PTOnDate
    -- "PERI" -> PTPerI
    -- "PERD" -> PTPerN
    -- "MESI" -> PTMeasuredI
    -- "MESD" -> PTMeasuredN
    -- "MESX" -> PTMeasuredSign
    -- "RNGI" -> PTRangeI
    -- "RNGD" -> PTRangeN
    -- "PROC" -> PTTask
    "LVLI" -> PTLevelsI
    "LVLD" -> PTLevelsN
    "LVLO" -> PTLevelsO
    "LVLS" -> PTLevelsS
    "LVLE" -> PTLevelsE
    "LVLP" -> PTLevelsP
    "LVLC" -> PTLevelsC
    other ->
        maybe (PTAV AV.AVTUnknown) PTAV
        $ AV.decodeAtomicValueTag
        $ AV.AVStrValueTag other


_vtagTo :: PTValueTag -> PValueTag
_vtagTo = case _ of
    -- PTNone -> "E"
    -- PTUnknown -> "UNK"
    -- PTInt -> "I"
    -- PTNumber -> "N"
    -- PTText -> "T"
    -- PTToComplete -> "D"
    -- PTPercentI -> "PCTI"
    -- PTPercentN -> "PCT"
    -- PTPercentSign -> "PCTX"
    -- PTToGetI -> "PI"
    -- PTToGetN -> "PD"
    -- PTOnTime -> "TIME"
    -- PTOnDate -> "DATE"
    -- PTPerI -> "PERI"
    -- PTPerN -> "PERD"
    -- PTMeasuredI -> "MESI"
    -- PTMeasuredN -> "MESD"
    -- PTMeasuredSign -> "MESX"
    -- PTRangeI -> "RNGI"
    -- PTRangeN -> "RNGD"
    -- PTTask -> "PROC"
    PTAV avTag ->
        avTag
        # AV.encodeAtomicValueTag
        # _unsafeConvertAVStrTag
    PTLevelsI -> pvt "LVLI"
    PTLevelsN -> pvt "LVLD"
    PTLevelsO -> pvt "LVLO"
    PTLevelsS -> pvt "LVLS"
    PTLevelsE -> pvt "LVLE"
    PTLevelsP -> pvt "LVLP"
    PTLevelsC -> pvt "LVLC"
    where pvt = PValueTag
    -- PTError -> "X"


_vtagOf :: Progress -> PTValueTag
_vtagOf = case _ of
    AV aval -> PTAV $ AV.valueTagOf aval
    LS levels ->
        case levels of
            LevelsI _ -> PTLevelsI
            LevelsN _ -> PTLevelsN
            LevelsO _ -> PTLevelsO
            LevelsS _ -> PTLevelsS
            LevelsP _ -> PTLevelsP
            LevelsC _ -> PTLevelsC
            LevelsE _ -> PTLevelsE


_levelsToAtomic :: Levels -> AV.AtomicValue /\ Array { name :: String, reached :: CT.ReachedOrNot, v :: Maybe AV.AtomicValue }
_levelsToAtomic = case _ of
    LevelsI levelsI -> levelsIVal levelsI /\ mapWithIndex (mapLevelI levelsI.reached) levelsI.levels
    LevelsN levelsN -> levelsNVal levelsN /\ mapWithIndex (mapLevelN levelsN.reached) levelsN.levels
    LevelsO levelsO -> levelsOVal levelsO /\ mapWithIndex (mapLevelO levelsO.reached) levelsO.levels
    LevelsS levelsS -> levelsSVal levelsS /\ mapWithIndex (mapLevelS levelsS.reached) levelsS.levels
    LevelsE levelsE -> levelsEVal levelsE /\ []
    LevelsC levelsC -> levelsCVal levelsC /\ pure (levelCHelper levelsC)
    LevelsP levelsP -> levelsPVal levelsP /\ mapWithIndex mapLevelP levelsP.levels
    where

        levelsIVal { levels, reached } = AV.ToGetI { got : foldl (\g lmax -> if reached >= lmax then g + 1 else g) 0 $ _.maximum <$> levels, total : Array.length levels }
        levelsNVal { levels, reached } = AV.ToGetI { got : foldl (\g lmax -> if reached >= lmax then g + 1 else g) 0 $ _.maximum <$> levels, total : Array.length levels }
        levelsEVal {  total, reached } = AV.ToGetI { got : reached, total }
        levelsSVal { levels, reached } = AV.ToGetI { got : reached, total : Array.length levels }
        levelsCVal { levelReached, totalLevels }
                                       = AV.ToGetI { got : levelReached, total : totalLevels }
        levelsPVal { levels }          = AV.ToGetI { got : foldl (\g lproc -> if isTaskPDone lproc then g + 1 else g) 0 $ _.proc <$> levels, total : Array.length levels }
        levelsOVal { levels, reached } = let mbMaxs = _.mbMaximum <$> levels in
                                         AV.ToGetIX { got : foldl (levelOGotFoldF reached) 0 mbMaxs, doing : foldl (levelODoingFoldF reached) 0 mbMaxs, total : Array.length levels }

        mapLevelI reached idx levelI = { name : levelI.name,  reached : if reached >= levelI.maximum then levelI.date # maybe CT.Reached CT.ReachedAt else CT.NotReached, v : Just $ AV.PInt levelI.maximum }
        mapLevelN reached idx levelN = { name : levelN.name,  reached : if reached >= levelN.maximum then levelN.date # maybe CT.Reached CT.ReachedAt else CT.NotReached, v : Just $ AV.PNumber levelN.maximum }
        mapLevelO reached idx levelO = { name : levelO.name,  reached : levelOHelper reached levelO.mbMaximum levelO.date, v : AV.PInt <$> levelO.mbMaximum }
        mapLevelS reached idx levelS = { name : levelS.gives, reached : if idx < reached then levelS.date # maybe CT.Reached CT.ReachedAt else CT.NotReached, v : Nothing }
        mapLevelP idx levelP         = { name : levelP.name,  reached : levelPHelper levelP.proc levelP.date, v : Just $ AV.PTask levelP.proc }

        levelOGotFoldF   reached g lmbmax = if maybe false (reached >= _) lmbmax then g + 1 else g
        levelODoingFoldF reached d lmbmax = maybe (d + 1) (\max -> d) lmbmax -- FIXME: we count only Nothing as "doing"

        levelOHelper reached mbMax mbDate =
            case mbMax, mbDate of
                Just max, Just date -> if reached >= max then CT.ReachedAt date else CT.NotReached
                Nothing,  Just date -> CT.ReachedAt date
                Just max, Nothing   -> if reached >= max then CT.Reached else CT.NotReached
                Nothing,  Nothing   -> CT.RUnknown

        levelCHelper { reachedAtCurrent, maximumAtCurrent, date } =
            { name : "Current"
            , reached : if reachedAtCurrent >= maximumAtCurrent then date # maybe CT.Reached CT.ReachedAt else CT.NotReached
            , v : Just $ AV.ToGetI { got : reachedAtCurrent, total : maximumAtCurrent }
            }

        levelPHelper taskP mbDate =
            if isTaskPDone taskP then mbDate # maybe CT.Reached CT.ReachedAt
            else if isTaskPDoing taskP then CT.InProgress
            else CT.NotReached

        isTaskPDone = case _ of
            TDone -> true
            _ -> false

        isTaskPDoing = case _ of
            TDoing -> true
            _ -> false


toAtomic :: Progress -> AV.AtomicValue /\ Array { name :: String, reached :: CT.ReachedOrNot, v :: Maybe AV.AtomicValue }
toAtomic = case _ of
    AV aval -> aval /\ []
    LS levels -> _levelsToAtomic levels