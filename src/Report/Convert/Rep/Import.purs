module Report.Convert.Rep.Import where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Int as Int
import Data.List (List)
import Data.List (toUnfoldable) as List
import Data.Maybe (Maybe(..))
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
  , depth    :: Int
  , tabulars :: Array RepTabular
  , items    :: Array RepItem
  }

type RepSubject =
  { name     :: String
  , tabulars :: Array RepTabular
  , groups   :: Array RepGroup
  }


-- | Parse Rep-formatted text into structured data.
-- | Groups are returned flat with `depth` indicating hierarchy.

fromRep :: String -> Either ParseError (Array RepSubject)
fromRep = runParser repParser


repParser :: Parser (Array RepSubject)
repParser = do
  subjects <- toArr <$> SP.many (SP.try subjectParser)
  SP.eof
  pure subjects


-- | Subject: SBJ. <name>, then tabular entries, then flat list of groups

subjectParser :: Parser RepSubject
subjectParser = do
  _      <- SP.string "SBJ. "
  name   <- restOfLine
  eol
  tabs   <- toArr <$> SP.many (SP.try $ tabularAt 0)
  groups <- toArr <$> SP.many (SP.try groupParser)
  pure { name, tabulars: tabs, groups }


-- | Group: leading spaces encode depth (4 spaces = 1 level).
-- | GRP. <title> [// <pathId>]
-- | Followed by tabular entries at same indent, then items one level deeper.

groupParser :: Parser RepGroup
groupParser = do
  leading <- SP.regex " *"
  _       <- SP.string "GRP. "
  let depth = String.length leading `div` 4
  full    <- restOfLine
  eol
  let (title /\ pathId) = splitGroupTitle full
  tabs    <- toArr <$> SP.many (SP.try $ tabularAt depth)
  items   <- toArr <$> SP.many (SP.try $ itemAt (depth + 1))
  pure { title, pathId, depth, tabulars: tabs, items }


-- | Tabular entry at the given indent depth:
-- |   - <name>
-- |   ; <MARKER>. <value>

tabularAt :: Int -> Parser RepTabular
tabularAt depth = do
  indentBy depth
  _               <- SP.string "- "
  name            <- restOfLine
  eol
  indentBy depth
  _               <- SP.string "; "
  (marker /\ raw) <- markerAndValue
  eol
  pure { name, marker, rawValue: raw }


-- | Item at the given indent depth.
-- | Fails immediately if the line starts with "GRP. " (nested group follows).

itemAt :: Int -> Parser RepItem
itemAt depth = do
  indentBy depth
  notFollowedBy (SP.string "GRP. ")
  title <- restOfLine
  eol
  decs  <- toArr <$> SP.many (SP.try $ decoratorAt depth)
  tags  <- toArr <$> SP.many (SP.try $ tagAt depth)
  pure { title, decorators: decs, tags }


-- | Item decorator at the given indent depth:
-- |   : <MARKER>. <value>

decoratorAt :: Int -> Parser RepDecorator
decoratorAt depth = do
  indentBy depth
  _               <- SP.string ": "
  (marker /\ raw) <- markerAndValue
  eol
  pure { marker, rawValue: raw }


-- | Tag at the given indent depth:
-- |   # <tag>

tagAt :: Int -> Parser String
tagAt depth = do
  indentBy depth
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
  "TXT"  -> Just (PText raw)
  "CMP"  -> Just (ToComplete { done: raw == "DONE" })
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
  "PRG"  -> Just (Task (taskPFromString raw))
  "LVI"  -> Just (LevelsI { reached: 0, levels: [] })
  "LVN"  -> Just (LevelsN { reached: 0.0, levels: [] })
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

indentBy :: Int -> Parser Unit
indentBy 0 = pure unit
indentBy n = void $ SP.string $ fold $ Array.replicate n "    "

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
parseToGetI s = case String.split (String.Pattern " ") s of
  [gotS, totS] -> do
    got   <- Int.fromString gotS
    total <- Int.fromString totS
    pure $ ToGetI { got, total }
  _ -> Nothing

parseToGetN :: String -> Maybe Progress
parseToGetN s = case String.split (String.Pattern " ") s of
  [gotS, totS] -> do
    got   <- Number.fromString gotS
    total <- Number.fromString totS
    pure $ ToGetN { got, total }
  _ -> Nothing

parsePercentSign :: String -> Maybe Progress
parsePercentSign s = case String.split (String.Pattern " ") s of
  [signS, pctS] -> do
    sign <- case signS of
      "+1" -> Just 1
      "-1" -> Just (-1)
      "+0" -> Just 0
      _    -> Nothing
    pct <- Number.fromString pctS
    pure $ PercentSign { sign, pct }
  _ -> Nothing

parsePerI :: String -> Maybe Progress
parsePerI s = do
  idx    <- String.indexOf (String.Pattern " ") s
  amount <- Int.fromString (String.take idx s)
  let per = String.drop (idx + 1) s
  pure $ PerI { amount, per }

parsePerN :: String -> Maybe Progress
parsePerN s = do
  idx    <- String.indexOf (String.Pattern " ") s
  amount <- Number.fromString (String.take idx s)
  let per = String.drop (idx + 1) s
  pure $ PerN { amount, per }

parseMeasuredI :: String -> Maybe Progress
parseMeasuredI s = do
  idx     <- String.indexOf (String.Pattern " ") s
  amount  <- Int.fromString (String.take idx s)
  let measure = String.drop (idx + 1) s
  pure $ MeasuredI { amount, measure }

parseMeasuredN :: String -> Maybe Progress
parseMeasuredN s = do
  idx     <- String.indexOf (String.Pattern " ") s
  amount  <- Number.fromString (String.take idx s)
  let measure = String.drop (idx + 1) s
  pure $ MeasuredN { amount, measure }

-- MeasuredSign export uses 4-space separator: "<sign> <amount>    <measure>"
parseMeasuredSign :: String -> Maybe Progress
parseMeasuredSign s = do
  spaceIdx     <- String.indexOf (String.Pattern " ") s
  let signS = String.take spaceIdx s
      rest  = String.drop (spaceIdx + 1) s
  sign         <- case signS of
    "+1" -> Just 1
    "-1" -> Just (-1)
    "+0" -> Just 0
    _    -> Nothing
  fourSpaceIdx <- String.indexOf (String.Pattern "    ") rest
  amount       <- Number.fromString (String.take fourSpaceIdx rest)
  let measure = String.drop (fourSpaceIdx + 4) rest
  pure $ MeasuredSign { sign, amount, measure }

parseRangeI :: String -> Maybe Progress
parseRangeI s = case String.split (String.Pattern " ") s of
  [fromS, toS] -> do
    from <- Int.fromString fromS
    to   <- Int.fromString toS
    pure $ RangeI { from, to }
  _ -> Nothing

parseRangeN :: String -> Maybe Progress
parseRangeN s = case String.split (String.Pattern " ") s of
  [fromS, toS] -> do
    from <- Number.fromString fromS
    to   <- Number.fromString toS
    pure $ RangeN { from, to }
  _ -> Nothing

parseLevelsE :: String -> Maybe Progress
parseLevelsE s = case String.split (String.Pattern " ") s of
  [reachedS, totalS] -> do
    reached <- Int.fromString reachedS
    total   <- Int.fromString totalS
    pure $ LevelsE { reached, total }
  _ -> Nothing

parseRelTime :: String -> Maybe Progress
parseRelTime s = do
  idx  <- String.indexOf (String.Pattern " ") s
  let relS  = String.take idx s
      timeS = String.drop (idx + 1) s
  rel  <- case relS of
    "more_than" -> Just RMoreThan
    "equal"     -> Just REqual
    "less_than" -> Just RLessThan
    _           -> Nothing
  timeRec <- parseTime timeS
  pure $ RelTime rel timeRec

parseTime :: String -> Maybe CT.STimeRec
parseTime s = case String.split (String.Pattern ":") s of
  [hrsS, minS, secS] -> do
    hrs <- Int.fromString hrsS
    min <- Int.fromString minS
    sec <- Int.fromString secS
    pure { hrs, min, sec }
  _ -> Nothing

-- Date format: <YYYY-MM-DD>
parseDate :: String -> Maybe CT.SDate
parseDate s =
  let inner = String.take (String.length s - 2) (String.drop 1 s)
  in case String.split (String.Pattern "-") inner of
    [yearS, monS, dayS] -> do
      year <- Int.fromString yearS
      mon  <- Int.fromString monS
      day  <- Int.fromString dayS
      pure $ CT.dateFromRec { day, mon, year }
    _ -> Nothing
