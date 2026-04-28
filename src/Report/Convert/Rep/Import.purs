module Report.Convert.Rep.Import where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Int as Int
import Data.List (List)
import Data.List (toUnfoldable) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import StringParser (Parser, ParseError, runParser, fail)
import StringParser (string, regex, eof, try, many, optionMaybe, tryAhead) as SP

import Report.Core as CT
import Report.Decorators.Progress (Progress(..), Relation(..))
import Report.Decorators.Task (taskPFromString)


-- | Parsed intermediate types

type RepTabular =
  { name     :: String
  , marker   :: String
  , rawValue :: String
  }

type RepDecorator =
  { marker   :: String
  , rawValue :: String
  }

type RepItem =
  { title      :: String
  , decorators :: Array RepDecorator
  , tags       :: Array String
  }

type RepGroup =
  { title    :: String
  , pathId   :: Maybe String
  , depth    :: Int   -- 1-based nesting level, derived from indent rank
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


-- | Assign 1-based depth to each group by ranking their indent levels.
-- | Groups with the smallest indent get depth 1, next get depth 2, etc.
-- | This makes depth meaningful regardless of whether 2, 4, or 8 spaces are used.

assignDepths :: Array RawRepGroup -> Array RepGroup
assignDepths rawGroups =
  let
    sortedUnique = Array.nub $ Array.sort $ map _.indent rawGroups
    rankOf i = 1 + fromMaybe 0 (Array.findIndex (_ == i) sortedUnique)
  in
    map (\g -> { title: g.title, pathId: g.pathId, depth: rankOf g.indent
               , tabulars: g.tabulars, items: g.items }) rawGroups


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
  eol
  pure { name, marker, rawValue: raw }


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
  eol
  pure { marker, rawValue: raw }


-- | Tag at exactly `spaces` leading spaces:
-- |   <spaces># <value>

tagAtSpaces :: Int -> Parser String
tagAtSpaces spaces = do
  matchingIndent spaces
  _ <- SP.string "# "
  t <- restOfLine
  eol
  pure t


-- | Reconstruct a Progress value from a Rep marker and raw value string.
-- | Returns Nothing for unrecognised markers or malformed values.

progressFromRep :: String -> String -> Maybe Progress
progressFromRep marker raw = case marker of
  "NON"  -> Just None
  "UNK"  -> Just Unknown
  "INT"  -> PInt    <$> Int.fromString raw
  "NUM"  -> PNumber <$> Number.fromString raw
  "TXT"  -> Just $ PText raw
  "CMP"  -> Just $ ToComplete { done: (raw == "DONE") || (raw == "1") }
  "PCI"  -> PercentI <$> Int.fromString raw
  "PCN"  -> PercentN <$> Number.fromString raw
  "PCTX" -> parsePercentSign raw
  "GTI"  -> parseToGetI raw
  "GTN"  -> parseToGetN raw
  "TIM"  -> OnTime <$> parseTime raw
  "DAT"  -> OnDate <$> parseDate raw
  "PPI"  -> parsePerI raw
  "PPN"  -> parsePerN raw
  "MSI"  -> parseMeasuredI raw
  "MSN"  -> parseMeasuredN raw
  "MSX"  -> parseMeasuredSign raw
  "RGI"  -> parseRangeI raw
  "RGN"  -> parseRangeN raw
  "PRG"  -> Just $ Task $ taskPFromString raw
  "LVI"  -> Just $ LevelsI { reached: 0, levels: [] }
  "LVN"  -> Just $ LevelsN { reached: 0.0, levels: [] }
  "LVE"  -> parseLevelsE raw
  "REL"  -> parseRelTime raw
  "XXX"  -> Just (Error raw)
  _      -> Nothing


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
    pct <- Number.fromString pctS
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

-- MeasuredSign export uses 4-space separator: "<sign> <amount>    <measure>"
parseMeasuredSign :: String -> Maybe Progress
parseMeasuredSign s = do
  spaceIdx     <- indexOfSpace s
  let signS = String.take spaceIdx s
      rest  = String.drop (spaceIdx + 1) s
  sign         <- case signS of
    "+" -> Just 1
    "-" -> Just $ -1
    "*" -> Just 0
    _    -> Nothing
  fourSpaceIdx <- String.indexOf (String.Pattern "    ") rest
  amount       <- Number.fromString (String.take fourSpaceIdx rest)
  let measure = String.drop (fourSpaceIdx + 4) rest
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
    mon  <- monthFromThreeLetter monS
    year <- Int.fromString yearS
    pure $ CT.dateFromRec { day, mon, year }
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

indexOfSpace :: String -> Maybe Int
indexOfSpace = String.indexOf $ String.Pattern " "

spacesCount :: Parser Int
spacesCount = SP.regex " *" <#> String.length

monthFromThreeLetter :: String -> Maybe Int
monthFromThreeLetter = case _ of
  "Jan" -> Just 1
  "Feb" -> Just 2
  "Mar" -> Just 3
  "Apr" -> Just 4
  "May" -> Just 5
  "Jun" -> Just 6
  "Jul" -> Just 7
  "Aug" -> Just 8
  "Sep" -> Just 9
  "Oct" -> Just 10
  "Nov" -> Just 11
  "Dec" -> Just 12
  _     -> Nothing

