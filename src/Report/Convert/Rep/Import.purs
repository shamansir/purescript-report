module Report.Convert.Rep.Import where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Enum (fromEnum) as Enum
import Data.Foldable (fold)
import Data.Int as Int
import Data.List (List)
import Data.List (toUnfoldable) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Data.String.CodeUnits (toCharArray, fromCharArray) as CU
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import StringParser (Parser, ParseError, runParser, fail)
import StringParser (string, regex, eof, try, many, optionMaybe, tryAhead) as SP

import Report.Core as CT
import Report.Decorator (Decorator(..), Key(..))
import Report.Tabular (Tabular)
import Report.Tabular (Item(..)) as Tab
import Report.Decorators.Tabular.TabularValue (TabularAtomicValue(..))
import Report.Decorators.Tabular.TabularValue (TabValTypeKey(..)) as TV
import Report.Decorators.Progress (Progress(..), Relation(..))
import Report.Decorators.Progress (PTValueTag(..), _vtagFrom) as P
import Report.Decorators.Task (taskPFromString)
import Report.Decorators.Rating (Rating(..))
import Report.Decorators.Priority as Priority
import Report.Decorators.Tags (RawTag(..), RawTags(..))
import Report.GroupPath (GroupPath)
import Report.GroupPath as GP

import Report.Convert.Rep.Keys as RE


-- | Parsed intermediate types

type RepTabular =
  { name     :: String
  , marker   :: RE.TriMarker
  , rawValue :: NonEmptyArray String
  , parsed   :: Maybe (Tab.Item TabularAtomicValue) -- TODO support all kinds of TabularValues
  }

type RepDecorator =
  { marker   :: RE.TriMarker
  , rawValue :: NonEmptyArray String
  , parsed   :: Maybe Decorator
  }

type RepItem =
  { title      :: String
  , decorators :: Array RepDecorator
  , tags       :: Array String
  }

type RepGroup =
  { title    :: String
  , pathId   :: Maybe String
  , depth    :: Int         -- 1-based nesting level, derived from indent rank
  , path     :: Array String  -- full path from root to this group (inclusive)
  , tabulars :: Array RepTabular
  , items    :: Array RepItem
  }

type RepSubject =
  { name     :: String
  , tabulars :: Array RepTabular
  , groups   :: Array RepGroup
  }


-- | Internal type carrying the raw space-count before depth is computed

type RawRepGroup =
  { title    :: String
  , pathId   :: Maybe String
  , indent   :: Int   -- actual leading-space count of the GRP. line
  , tabulars :: Array RepTabular
  , items    :: Array RepItem
  }


-- | Parse Rep-formatted text into structured data.
-- | Groups are returned flat; `depth` reflects nesting rank regardless of indent size.

fromRep :: String -> Either ParseError (Array RepSubject)
fromRep = runParser repParser


repParser :: Parser (Array RepSubject)
repParser = do
  subjects <- toArr <$> SP.many (SP.try subjectParser)
  SP.eof
  pure subjects


-- | Subject: SBJ. <name>, then tabular entries at column 0, then flat group list.

subjectParser :: Parser RepSubject
subjectParser = do
  _         <- SP.string "SBJ. "
  name      <- restOfLine
  eol
  tabs      <- toArr <$> SP.many (SP.try $ tabularAtSpaces 0)
  rawGroups <- toArr <$> SP.many (SP.try rawGroupParser)
  pure { name, tabulars: tabs, groups: assignDepths rawGroups }


-- | Parse one group, recording its raw leading-space count.
-- | GRP. <title> [// <pathId>]
-- | Tabular entries follow at the same indent; items at any strictly greater indent.

rawGroupParser :: Parser RawRepGroup
rawGroupParser = do
  leadingSpaces <- spacesCount
  _             <- SP.string "GRP. "
  full    <- restOfLine
  eol
  let (title /\ pathId) = splitGroupTitle full
  tabs    <- toArr <$> SP.many (SP.try $ tabularAtSpaces leadingSpaces)
  items   <- toArr <$> SP.many (SP.try $ itemAt leadingSpaces)
  pure { title, pathId, indent: leadingSpaces, tabulars: tabs, items }


-- | Assign 1-based depth and full path to each group.
-- | Depth ranks unique indent levels (smallest indent = depth 1).
-- | Path is built via a stack: each group's segment replaces the stack slot at its depth,
-- | and the path is all stack slots from root to the group (inclusive).

assignDepths :: Array RawRepGroup -> Array RepGroup
assignDepths rawGroups =
  let
    sortedUnique = Array.nub $ Array.sort $ map _.indent rawGroups
    rankOf i = 1 + fromMaybe 0 (Array.findIndex (_ == i) sortedUnique)

    step (stack /\ acc) g =
      let depth    = rankOf g.indent
          seg      = groupSegment g
          newStack = Array.take (depth - 1) stack `Array.snoc` seg
      in newStack /\ Array.snoc acc
           { title: g.title, pathId: g.pathId, depth, path: newStack
           , tabulars: g.tabulars, items: g.items }

    _ /\ groups = Array.foldl step ([] /\ []) rawGroups
  in
    groups


-- | Derive the path segment for a group: use explicit pathId when present,
-- | otherwise slugify the title (lowercase, non-alphanumeric → '-').

groupSegment :: RawRepGroup -> String
groupSegment g = fromMaybe (titleToSlug g.title) g.pathId

titleToSlug :: String -> String
titleToSlug s = CU.fromCharArray $ map slugChar $ CU.toCharArray $ String.toLower s
  where
    slugChar c =
      let n = Enum.fromEnum c
      in if (n >= 97 && n <= 122) || (n >= 48 && n <= 57) then c else '-'

continuationLines :: Int -> Parser (Array String)
continuationLines spaces = do
  contLines <- SP.many $ SP.try $ do
    eol
    matchingIndent spaces
    _        <- SP.string ">>> "
    contLine <- restOfLine
    pure contLine
  pure $ List.toUnfoldable contLines


-- | Tabular entry at exactly `spaces` leading spaces:
-- |   <spaces>- <name>
-- |   <spaces>; <MARKER>. <value>

tabularAtSpaces :: Int -> Parser RepTabular
tabularAtSpaces spaces = do
  matchingIndent spaces
  _               <- SP.string "- "
  name            <- restOfLine
  eol
  matchingIndent spaces
  _               <- SP.string "; "
  (marker /\ raw) <- markerAndValue
  continuations   <- continuationLines spaces
  eol
  let triMarker = RE.TM marker
      rawNCont  = NEA.cons' raw continuations
      parsedTabular = tabularFromRep triMarker rawNCont
        <#> \v -> Tab.Item { key: name, label: name, value: v }
  pure { name, marker: triMarker, rawValue: rawNCont, parsed: parsedTabular }


-- | Item: leading spaces are read from the input and must exceed parentSpaces.
-- | Fails if the line starts with "GRP. " — that is a nested group, not an item.
-- | Decorators and tags are parsed at the same indent as the item title.

itemAt :: Int -> Parser RepItem
itemAt parentSpaces = do
  leading <- SP.regex " +"
  let itemSpaces = String.length leading
  when (itemSpaces <= parentSpaces) $ fail "item not deeper than parent group"
  notFollowedBy (SP.string "GRP. ")
  title <- restOfLine
  eol
  decs  <- toArr <$> SP.many (SP.try $ decoratorAtSpaces itemSpaces)
  tags  <- toArr <$> SP.many (SP.try $ tagAtSpaces itemSpaces)
  pure { title, decorators: decs, tags }


-- | Item decorator at exactly `spaces` leading spaces:
-- |   <spaces>: <MARKER>. <value>

decoratorAtSpaces :: Int -> Parser RepDecorator
decoratorAtSpaces spaces = do
  matchingIndent spaces
  _               <- SP.string ": "
  (marker /\ raw) <- markerAndValue
  continuations   <- continuationLines spaces
  eol
  let triMarker = RE.TM marker
      rawNCont  = NEA.cons' raw continuations
      parsedDec = decoratorFromRep (RE.decoratorKeyFromTriMarker triMarker) rawNCont
  pure { marker: triMarker, rawValue: rawNCont, parsed: parsedDec }


-- | Tag at exactly `spaces` leading spaces:
-- |   <spaces># <value>

tagAtSpaces :: Int -> Parser String
tagAtSpaces spaces = do
  matchingIndent spaces
  _ <- SP.string "# "
  t <- restOfLine
  eol
  pure t


tabularFromRep :: RE.TriMarker -> NonEmptyArray String -> Maybe TabularAtomicValue
tabularFromRep triMarker raw = RE.tftm triMarker # case _ of
  TV.TVTString        -> Just      $  TVString          fraw
  TV.TVTInt           -> TVInt    <$> Int.fromString    fraw
  TV.TVTID            -> TVID     <$> Int.fromString    fraw
  TV.TVTYear          -> TVYear   <$> Int.fromString    fraw
  TV.TVTNumber        -> TVNumber <$> Number.fromString fraw
  TV.TVTBoolean       -> parseBooleanAtomic             fraw
  TV.TVTTime          -> TVTime   <$> parseTime         fraw
  TV.TVTDate          -> TVDate   <$> parseDate         fraw
  TV.TVTDateTime      -> uncurry TVDateTime              <$> parseDateTime fraw
  TV.TVTTimeRange     -> TVTimeRange     <$> toFromToRec <$> parseRange parseTime parseTime fraw
  TV.TVTDateRange     -> TVDateRange     <$> toFromToRec <$> parseRange parseDate parseDate fraw
  TV.TVTDateTimeRange -> TVDateTimeRange <$> toFromToRec <$> bimap toDateTimeRec toDateTimeRec <$> (parseRange parseDateTime parseDateTime fraw)
  TV.TVTDecorator dk  -> TVDecorator <$> decoratorFromRep dk raw
  TV.TVTTags          -> Just $ TVTags $ RawTags $ parseTags fraw
  where
    fraw = NEA.head raw
    toDateTimeRec :: (CT.SDate /\ CT.STimeRec) -> { date :: CT.SDate, time :: CT.STimeRec }
    toDateTimeRec (date /\ time) = { date, time }
    toFromToRec :: forall f t. (f /\ t) -> { from :: f, to :: t }
    toFromToRec (from /\ to) = { from, to }


parseBooleanAtomic :: String -> Maybe TabularAtomicValue
parseBooleanAtomic = String.toLower >>> case _ of
  "true"  -> Just $ TVBoolean true
  "false" -> Just $ TVBoolean false
  "1"     -> Just $ TVBoolean true
  "0"     -> Just $ TVBoolean false
  _       -> Nothing


decoratorFromRep :: Key -> NonEmptyArray String -> Maybe Decorator
decoratorFromRep dk raw = case dk of
  KProgress pvTag -> SProgress  <$> progressFromRep (RE.tmfp $ P._vtagFrom pvTag) raw
  KDescription    -> Just        $  SDescription fraw
  KEarnedAt       -> SEarnedAt  <$> parseDate fraw
  KRating         -> PRating    <$> parseRating fraw
  KTask           -> Just        $  PTask $ taskPFromString fraw
  KReference      -> SReference <$> parseGroupPath fraw
  KPriority       -> PPriority  <$> Priority.fromString fraw
  where
    fraw = NEA.head raw


-- | Reconstruct a Progress value from a Rep marker and raw value string.
-- | Returns Nothing for unrecognised markers or malformed values.

progressFromRep :: RE.TriMarker -> NonEmptyArray String -> Maybe Progress
progressFromRep triMarker raw = RE.ptftm triMarker # case _ of
  P.PTNone         -> Just None
  P.PTUnknown      -> Just Unknown
  P.PTInt          -> PInt    <$> Int.fromString fraw
  P.PTNumber       -> PNumber <$> Number.fromString fraw
  P.PTText         -> Just $ PText fraw
  P.PTToComplete   -> Just $ ToComplete { done: (fraw == "DONE") || (fraw == "1") }
  P.PTPercentI     -> PercentI <$> Int.fromString    (stripPercent fraw)
  P.PTPercentN     -> PercentN <$> Number.fromString (stripPercent fraw)
  P.PTPercentSign  -> parsePercentSign fraw
  P.PTToGetI       -> parseToGetI fraw
  P.PTToGetN       -> parseToGetN fraw
  P.PTOnTime       -> OnTime <$> parseTime fraw
  P.PTOnDate       -> OnDate <$> parseDate fraw
  P.PTPerI         -> parsePerI fraw
  P.PTPerN         -> parsePerN fraw
  P.PTMeasuredI    -> parseMeasuredI fraw
  P.PTMeasuredN    -> parseMeasuredN fraw
  P.PTMeasuredSign -> parseMeasuredSign fraw
  P.PTRangeI       -> parseRangeI fraw
  P.PTRangeN       -> parseRangeN fraw
  P.PTTask         -> Just $ Task $ taskPFromString fraw
  P.PTLevelsI      -> Just $ LevelsI { reached: 0, levels: [] }
  P.PTLevelsN      -> Just $ LevelsN { reached: 0.0, levels: [] }
  P.PTLevelsE      -> parseLevelsE fraw
  P.PTRelTime      -> parseRelTime fraw
  P.PTError        -> Just $ Error fraw
  _      -> Nothing
  where
    fraw = NEA.head raw


-- Private parser helpers

markerAndValue :: Parser (String /\ String)
markerAndValue = do
  marker <- SP.regex "[A-Z]+"
  _      <- SP.string ". "
  value  <- restOfLine
  pure (marker /\ value)

restOfLine :: Parser String
restOfLine = SP.regex "[^\n]*"

eol :: Parser Unit
eol = void $ SP.optionMaybe $ SP.string "\n"

-- | Match exactly n spaces (n > 0); no-op for n = 0.
matchingIndent :: Int -> Parser Unit
matchingIndent 0 = pure unit
matchingIndent n = void $ SP.string $ fold $ Array.replicate n " "

notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy p =
  SP.optionMaybe (SP.tryAhead p) >>= case _ of
    Just _  -> fail "unexpected"
    Nothing -> pure unit

splitGroupTitle :: String -> String /\ Maybe String
splitGroupTitle full =
  case String.indexOf (String.Pattern " // ") full of
    Just idx -> String.take idx full /\ Just (String.drop (idx + 4) full)
    Nothing  -> full /\ Nothing

toArr :: forall a. List a -> Array a
toArr = List.toUnfoldable


-- Progress value parsers

parseToGetI :: String -> Maybe Progress
parseToGetI s = do
  (gotS /\ totS) <- splitPair s
  got   <- Int.fromString gotS
  total <- Int.fromString totS
  pure $ ToGetI { got, total }

parseToGetN :: String -> Maybe Progress
parseToGetN s = do
  (gotS /\ totS) <- splitPair s
  got   <- Number.fromString gotS
  total <- Number.fromString totS
  pure $ ToGetN { got, total }

parsePercentSign :: String -> Maybe Progress
parsePercentSign s = case splitWithSpace s of
  [signS, pctS] -> do
    sign <- case signS of
      "+" -> Just 1
      "-" -> Just $ -1
      "*" -> Just 0
      _    -> Nothing
    pct <- Number.fromString (stripPercent pctS)
    pure $ PercentSign { sign, pct }
  _ -> Nothing

parsePerI :: String -> Maybe Progress
parsePerI s = do
  idx    <- indexOfSpace s
  amount <- Int.fromString (String.take idx s)
  let per = String.drop (idx + 1) s
  pure $ PerI { amount, per }

parsePerN :: String -> Maybe Progress
parsePerN s = do
  idx    <- indexOfSpace s
  amount <- Number.fromString (String.take idx s)
  let per = String.drop (idx + 1) s
  pure $ PerN { amount, per }

parseMeasuredI :: String -> Maybe Progress
parseMeasuredI s = do
  idx    <- indexOfSpace s
  amount <- Int.fromString (String.take idx s)
  let measure = String.drop (idx + 1) s
  pure $ MeasuredI { amount, measure }

parseMeasuredN :: String -> Maybe Progress
parseMeasuredN s = do
  idx    <- indexOfSpace s
  amount <- Number.fromString (String.take idx s)
  let measure = String.drop (idx + 1) s
  pure $ MeasuredN { amount, measure }

parseMeasuredSign :: String -> Maybe Progress
parseMeasuredSign s = do
  spaceIdx1 <- indexOfSpace s
  let signS = String.take spaceIdx1 s
      rest  = String.drop (spaceIdx1 + 1) s
  sign      <- case signS of
    "+" -> Just 1
    "-" -> Just $ -1
    "*" -> Just 0
    _   -> Nothing
  spaceIdx2 <- indexOfSpace rest
  amount    <- Number.fromString (String.take spaceIdx2 rest)
  let measure = String.drop (spaceIdx2 + 1) rest
  pure $ MeasuredSign { sign, amount, measure }

parseRangeI :: String -> Maybe Progress
parseRangeI s = case splitWithSpace s of
  [fromS, toS] -> do
    from <- Int.fromString fromS
    to   <- Int.fromString toS
    pure $ RangeI { from, to }
  _ -> Nothing

parseRangeN :: String -> Maybe Progress
parseRangeN s = case splitWithSpace s of
  [fromS, toS] -> do
    from <- Number.fromString fromS
    to   <- Number.fromString toS
    pure $ RangeN { from, to }
  _ -> Nothing

parseLevelsE :: String -> Maybe Progress
parseLevelsE s = do
  (reachedS /\ totalS) <- splitPair s
  reached <- Int.fromString reachedS
  total   <- Int.fromString totalS
  pure $ LevelsE { reached, total }

parseRelTime :: String -> Maybe Progress
parseRelTime s = do
  idx  <- indexOfSpace s
  let relS  = String.take idx s
      timeS = String.drop (idx + 1) s
  rel  <- case relS of
    ">" -> Just RMoreThan
    "=" -> Just REqual
    "<" -> Just RLessThan
    _   -> Nothing
  timeRec <- parseTime timeS
  pure $ RelTime rel timeRec

parseTime :: String -> Maybe CT.STimeRec
parseTime s = case String.split (String.Pattern ":") s of
  [hrsS, minS] -> do
    hrs <- Int.fromString hrsS
    min <- Int.fromString minS
    pure { hrs, min, sec: 0 }
  [hrsS, minS, secS] -> do
    hrs <- Int.fromString hrsS
    min <- Int.fromString minS
    sec <- Int.fromString secS
    pure { hrs, min, sec }
  _ -> Nothing

-- Date format: <YYYY-MM-DD> or DD-Mon-YYYY
parseDate :: String -> Maybe CT.SDate
parseDate s
  | String.take 1 s == "<" = parseAngleBracketDate s
  | otherwise              = parseDayMonYear s

-- <YYYY-MM-DD>
parseAngleBracketDate :: String -> Maybe CT.SDate
parseAngleBracketDate s =
  let inner = String.take (String.length s - 2) (String.drop 1 s)
  in case String.split (String.Pattern "-") inner of
    [yearS, monS, dayS] -> do
      year <- Int.fromString yearS
      mon  <- Int.fromString monS
      day  <- Int.fromString dayS
      pure $ CT.dateFromRec { day, mon, year }
    _ -> Nothing

-- DD-Mon-YYYY  (Mon = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec)
parseDayMonYear :: String -> Maybe CT.SDate
parseDayMonYear s = case String.split (String.Pattern "-") s of
  [dayS, monS, yearS] -> do
    day  <- Int.fromString dayS
    mon  <- CT.parseMonth monS
    -- mon  <- monthFromThreeLetter monS
    year <- Int.fromString yearS
    pure $ CT.SDate { day, month : mon, year }
  _ -> Nothing


parseDateTime :: String -> Maybe (CT.SDate /\ CT.STimeRec)
parseDateTime s = case splitWithSpace s of
  [ dateStr, timeStr ] -> parseDT dateStr timeStr
  _ -> case String.split (String.Pattern "@") s of
    [ dateStr, timeStr ] -> parseDT dateStr timeStr
    _ -> Nothing
  where
    parseDT dateStr timeStr = do
      date <- parseDate dateStr
      time <- parseTime timeStr
      pure $ date /\ time


parseRating :: String -> Maybe Rating
parseRating s =
  case splitPair s of
    Just (valStr /\ maxStr) -> do
      val <- Number.fromString valStr
      max <- Int.fromString    maxStr
      pure $ Rating { value : val, max }
    Nothing -> Number.fromString s <#> \val -> Rating { value : val, max : 10 }


parseGroupPath :: String -> Maybe GroupPath
parseGroupPath s =
  case String.split (String.Pattern "::") s of
    [] -> Nothing
    [ one ] -> case splitWithSpace one of
      [] ->      Just $ GP.path [ one ]
      spItems -> Just $ GP.path spItems
    scItems ->   Just $ GP.path scItems


parseTags :: String -> Array RawTag
parseTags s =
  case String.split (String.Pattern ",") s of
    [ one ] -> Array.catMaybes $ parseTag <$> splitWithSpace one
    some -> Array.catMaybes $ parseTag <$> some


parseTag :: String -> Maybe RawTag
parseTag s =
  case Array.uncons $ String.split (String.Pattern "::") s of
    Just { head, tail } -> Just $ RawTag { content : NEA.cons' head tail, id : NEA.cons' head tail }
    Nothing -> Nothing


parseRange :: forall from to. (String -> Maybe from) -> (String -> Maybe to) -> String -> Maybe (from /\ to)
parseRange toFrom toTo s =
  case String.split (String.Pattern "--") s of
    [ from, to ] -> (/\) <$> toFrom from <*> toTo to
    _ -> Nothing

-- Helpers

-- | Split "a b" or "a/b" into Just (a /\ b); Nothing otherwise.
splitPair :: String -> Maybe (String /\ String)
splitPair s =
  case splitWithSpace s of
    [a, b] -> Just (a /\ b)
    _ -> case String.split (String.Pattern "/") s of
      [a, b] -> Just (a /\ b)
      _      -> Nothing

splitWithSpace :: String -> Array String
splitWithSpace = String.split $ String.Pattern " "

stripPercent :: String -> String
stripPercent s = fromMaybe s $ String.stripSuffix (String.Pattern "%") s

indexOfSpace :: String -> Maybe Int
indexOfSpace = String.indexOf $ String.Pattern " "

spacesCount :: Parser Int
spacesCount = SP.regex " *" <#> String.length

-- monthFromThreeLetter :: String -> Maybe Int
-- monthFromThreeLetter = CT.parseMonth >>> map CT.monthToInt