module Report.Convert.Rep.Import.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array ((:))
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
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import StringParser (Parser, ParseError, runParser, fail)
import StringParser (string, regex, eof, try, many, optionMaybe, tryAhead) as SP

import Report.Core as CT
import Report.Chain (Chain)
import Report.Chain (fromArray) as Chain
import Utils.Data.WColor (fromHex) as WColor
import Report.Decorator (Decorator(..), Key(..))
import Report.Tabular (Item(..), Tabular(..)) as Tab
import Report.AtomicValue (AtomicValue)
import Report.AtomicValue (AtomicValue(..), AtomicValueTag(..)) as AV
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Decorators.Tabular.TabularValue (TabValTypeKey(..), TabularValueTag(..), TabularValue(..), fromDecorator) as TV
import Report.Decorators.Tabular.Logbook (Logbook(..), LogbookEntry)
import Report.Decorators.Progress (Progress(..))
import Report.Decorators.Progress (PTValueTag(..), _vtagFrom, Levels(..)) as P
import Report.Decorators.Task (taskPFromString, mbTaskPFromString)
import Report.Decorators.Rating (Rating(..))
import Report.Decorators.Priority as Priority
import Report.Decorators.Tags (RawTag(..), RawTags(..))
import Report.GroupPath (GroupPath)
import Report.GroupPath as GP
import Report.Convert.Generic (nameToId) as CG

import Report.Impl.Subject (SubjectId(..))
import Report.Convert.Rep.Keys as RE


-- | Parsed intermediate types

-- TODO: Kind of intersects with `Report.Convert.Types` : `DecoratorRec`, `TabularRec`, `ItemRec`, `SubjectRec`
--       as well as the structures in the `Report.Impl` module.

type RepTabular =
  { name     :: String
  , id       :: Maybe String
  , marker   :: RE.TriMarker
  , rawValue :: NonEmptyArray String
  , parts    :: Array RepTabularPart -- the parts of the composite value, empty for atomic ones
  , parsed   :: Maybe (Tab.Item TabularValue)
  }

-- | A value inside the composite tabular value
type RepTabularValue =
  { marker   :: RE.TriMarker
  , rawValue :: NonEmptyArray String
  , parts    :: Array RepTabularPart
  }

-- | A part of the composite tabular value: either a value or (inside `TAB`) a tabular entry
data RepTabularPart
  = RTPValue RepTabularValue
  | RTPEntry RepTabular

type RepDecorator =
  { marker   :: RE.TriMarker
  , rawValue :: NonEmptyArray String
  , parsed   :: Maybe Decorator
  }

type RepItem =
  { title      :: String
  , decorators :: Array RepDecorator
  , tags       :: Array RawTag
  , tabulars   :: Array RepTabular
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
  , id       :: Maybe SubjectId
  , tabulars :: Array RepTabular
  , groups   :: Array RepGroup
  , tags     :: Array RawTag
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
fromRep =
  String.replaceAll (String.Pattern "\n\n") (String.Replacement "\n")
  >>> String.replaceAll (String.Pattern "\n\n") (String.Replacement "\n")
  >>> runParser repParser


repParser :: Parser (Array RepSubject)
repParser = do
  subjects <- toArr <$> SP.many (SP.try subjectParser)
  SP.eof
  pure subjects


-- | Subject: SBJ. <name>, then tabular entries at column 0, then flat group list.

subjectParser :: Parser RepSubject
subjectParser = do
  _         <- SP.string "SBJ. "
  rol       <- restOfLine
  let (name /\ mbId)
            = case String.split (String.Pattern "//") rol of
                [ ]      -> "" /\ Nothing
                [ name ] -> String.trim name /\ Nothing
                [ name, id ] -> String.trim name /\ Just (SubjectId $ String.trim id)
                _        -> "" /\ Nothing
  eol
  tags      <- toArr <$> SP.many (SP.try $ tagAtSpaces 0)
  tabs      <- toArr <$> SP.many (SP.try $ tabularAtSpaces 0)
  rawGroups <- toArr <$> SP.many (SP.try rawGroupParser)
  pure { name, tabulars: tabs, groups: assignDepths rawGroups, tags : Array.catMaybes tags, id : mbId }


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
  rol             <- restOfLine
  let (name /\ mbId)
            = case String.split (String.Pattern "//") rol of
                [ ]      -> "" /\ Nothing
                [ name ] -> String.trim name /\ Nothing
                [ name, id ] -> String.trim name /\ Just (String.trim id)
                _        -> "" /\ Nothing
  eol
  { marker, rawValue, parts } <- tabularValueAtSpaces spaces
  let parsedTabular = tabularFromRep marker rawValue parts
        <#> \v -> Tab.Item { key: fromMaybe (CG.nameToId name) mbId, label: name, value: v }
  pure { name, id : mbId, marker, rawValue, parts, parsed: parsedTabular }


-- | Tabular value at exactly `spaces` leading spaces:
-- |   <spaces>; <MARKER>. <value>
-- |   <spaces>>>> <next line of the value>
-- | the composite values are followed by their parts at any deeper indent (the same for all the parts).

tabularValueAtSpaces :: Int -> Parser RepTabularValue
tabularValueAtSpaces spaces = do
  matchingIndent spaces
  _               <- SP.string "; "
  (marker /\ raw) <- markerAndValue
  continuations   <- continuationLines spaces
  eol
  let triMarker = RE.TM marker
  parts <- case RE.tvtftm triMarker of
    TV.TVTAtomic _ -> pure []
    _              -> tabularPartsDeeperThan spaces
  pure { marker: triMarker, rawValue: NEA.cons' raw continuations, parts }


tabularPartsDeeperThan :: Int -> Parser (Array RepTabularPart)
tabularPartsDeeperThan parentSpaces = do
  mbSpaces <- SP.optionMaybe $ SP.tryAhead do
    spaces <- spacesCount
    when (spaces <= parentSpaces) $ fail "not a part of the composite value"
    _ <- SP.string "- " <|> SP.string "; "
    pure spaces
  case mbSpaces of
    Nothing -> pure []
    Just spaces ->
      toArr <$> SP.many (SP.try $ (RTPEntry <$> SP.try (tabularAtSpaces spaces)) <|> (RTPValue <$> tabularValueAtSpaces spaces))


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
  tabs  <- toArr <$> SP.many (SP.try $ tabularAtSpaces itemSpaces)
  pure { title, decorators: decs, tags : Array.catMaybes tags, tabulars : tabs }


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

tagAtSpaces :: Int -> Parser (Maybe RawTag)
tagAtSpaces spaces = do
  matchingIndent spaces
  _ <- SP.string "# "
  rol <- restOfLine
  let tagId = case String.split (String.Pattern "//") rol of
        [ ]        -> String.trim rol
        [ itagId ] -> String.trim itagId
        [ itagId, itagContent ] -> String.trim itagId
        _          -> String.trim rol
  eol
  pure $ parseTag tagId


-- | See `Report.Convert.Rep.Export._tabularValueDoc` for the format of the composite values,
-- | the counts of the parts are checked.
tabularFromRep :: RE.TriMarker -> NonEmptyArray String -> Array RepTabularPart -> Maybe TabularValue
tabularFromRep triMarker raw parts = case RE.tvtftm triMarker of
  TV.TVTAtomic tvKey    -> TV.TVAtomic <$> atomicTabularFromRep tvKey raw
  TV.TVTValues          -> countedAs [ partsCount ] $ TV.TVValues <$> traverse atomicPart parts
  TV.TVTValuesNest      -> countedAs [ partsCount ] $ TV.TVValuesNest <$> traverse valuesPart parts
  TV.TVTTabulars        -> countedAs [ partsCount ] $ TV.TVTabulars <$> traverse tabularPart parts
  TV.TVTPair            -> countedAs [ 2 ] $ case parts of
                             [ partA, partB ] -> TV.TVPair <$> valuePart partA <*> valuePart partB
                             _ -> Nothing
  TV.TVTTabularsNest    -> case counts of
                             [ directCount, partsCount' ] | Array.length parts == directCount + 2 * partsCount' -> do
                               direct <- traverse tabularPart $ Array.take directCount parts
                               nestParts <- traverse nestPart $ pairs $ Array.drop directCount parts
                               pure $ TV.TVTabularsNest { direct, parts : nestParts }
                             _ -> Nothing
  TV.TVTLogbook         -> countedAs [ partsCount ] $ TV.TVLogbook <$> Logbook <$> traverse logbookEntryPart parts
  -- these are only the parts of the composite values
  TV.TVTTabular         -> Nothing
  TV.TVTLogbookEntry    -> Nothing
  where
    partsCount = Array.length parts
    -- `+++ 2` or `+++ 1 -> 2`
    counts = case String.stripPrefix (String.Pattern "+++ ") $ NEA.head raw of
      Just countsStr -> fromMaybe [] $ traverse Int.fromString $ String.split (String.Pattern " -> ") countsStr
      Nothing -> []
    countedAs :: forall a. Array Int -> Maybe a -> Maybe a
    countedAs expected mbValue = if counts == expected then mbValue else Nothing
    valuePart = case _ of
      RTPValue v -> tabularFromRep v.marker v.rawValue v.parts
      RTPEntry _ -> Nothing
    atomicPart = valuePart >=> case _ of
      TV.TVAtomic atomicV -> Just atomicV
      _ -> Nothing
    valuesPart = valuePart >=> case _ of
      TV.TVValues atomicVs -> Just atomicVs
      _ -> Nothing
    tabularsPart = valuePart >=> case _ of
      TV.TVTabulars tabulars -> Just tabulars
      _ -> Nothing
    tabularPart = case _ of
      RTPValue v | RE.tvtftm v.marker == TV.TVTTabular ->
        if String.stripPrefix (String.Pattern "+++ ") (NEA.head v.rawValue) == Just (show $ Array.length v.parts)
          then Tab.Tabular <$> traverse entryPart v.parts
          else Nothing
      _ -> Nothing
    entryPart = case _ of
      RTPEntry entry -> entry.parsed >>= \(Tab.Item item) -> case item.value of
        TV.TVAtomic atomicV -> Just $ Tab.Item item { value = atomicV }
        _ -> Nothing
      RTPValue _ -> Nothing
    nestPart (atomicPartV /\ tabularsPartV) = (/\) <$> atomicPart atomicPartV <*> tabularsPart tabularsPartV
    pairs arr = case Array.uncons arr of
      Just { head : first, tail } -> case Array.uncons tail of
        Just { head : second, tail : rest } -> (first /\ second) : pairs rest
        Nothing -> []
      Nothing -> []
    logbookEntryPart = case _ of
      RTPValue v | RE.tvtftm v.marker == TV.TVTLogbookEntry -> parseLogbookEntry $ NEA.head v.rawValue
      _ -> Nothing


-- | `[<start>] [-- <end>] <task> [<name>]`
parseLogbookEntry :: String -> Maybe LogbookEntry
parseLogbookEntry str =
  let
    words = splitWithSpace str
    isDate word = String.take 1 word == "<"
    start /\ afterStart = case Array.uncons words of
      Just { head, tail } | isDate head -> Just head /\ tail
      _ -> Nothing /\ words
    end /\ afterEnd = case afterStart of
      [ "--" ] -> Nothing /\ afterStart
      _ -> case Array.uncons afterStart of
        Just { head : "--", tail } -> case Array.uncons tail of
          Just { head, tail : rest } -> Just head /\ rest
          Nothing -> Nothing /\ afterStart
        _ -> Nothing /\ afterStart
  in do
    { head : taskStr, tail : nameWords } <- Array.uncons afterEnd
    task <- mbTaskPFromString taskStr
    startDate <- traverse (parseDate >>> map CT.dateToRec) start
    endDate <- traverse (parseDate >>> map CT.dateToRec) end
    pure { name : String.joinWith " " nameWords, task, start : startDate, end : endDate }


atomicTabularFromRep :: TV.TabValTypeKey -> NonEmptyArray String -> Maybe AtomicValue
atomicTabularFromRep tvKey raw = tvKey # case _ of
  TV.TVTString        -> Just       $  AV.PText          $ String.joinWith "\n" $ NEA.toArray raw
  TV.TVTInt           -> AV.PInt    <$> Int.fromString    fraw
  TV.TVTID            -> AV.PID     <$> Int.fromString    fraw
  TV.TVTYear          -> AV.PYear   <$> Int.fromString    fraw
  TV.TVTNumber        -> AV.PNumber <$> Number.fromString fraw
  TV.TVTBoolean       -> parseBooleanAtomic               fraw
  TV.TVTTime          -> AV.OnTime  <$> parseTime         fraw
  TV.TVTDate          -> AV.OnDate  <$> parseDate         fraw
  TV.TVTDateTime      -> uncurry AV.OnDateTime            <$> parseDateTime fraw
  TV.TVTTimeRange     -> AV.TimeRange     <$> toFromToRec <$> parseRange parseTime parseTime fraw
  TV.TVTDateRange     -> AV.DateRange     <$> toFromToRec <$> parseRange parseDate parseDate fraw
  TV.TVTDateTimeRange -> AV.DateTimeRange <$> toFromToRec <$> bimap toDateTimeRec toDateTimeRec <$> (parseRange parseDateTime parseDateTime fraw)
  TV.TVTDecorator dk  -> TV.fromDecorator <$> decoratorFromRep dk raw
  TV.TVTTags          -> Just $ AV.PRawTags $ RawTags $ parseTags fraw
  where
    fraw = NEA.head raw
    toDateTimeRec :: (CT.SDate /\ CT.STimeRec) -> { date :: CT.SDate, time :: CT.STimeRec }
    toDateTimeRec (date /\ time) = { date, time }
    toFromToRec :: forall f t. (f /\ t) -> { from :: f, to :: t }
    toFromToRec (from /\ to) = { from, to }


parseBooleanAtomic :: String -> Maybe AtomicValue
parseBooleanAtomic = String.toLower >>> case _ of
  "true"  -> Just $ AV.PBoolean true
  "false" -> Just $ AV.PBoolean false
  "1"     -> Just $ AV.PBoolean true
  "0"     -> Just $ AV.PBoolean false
  _       -> Nothing


decoratorFromRep :: Key -> NonEmptyArray String -> Maybe Decorator
decoratorFromRep dk raw = case dk of
  KProgress pvTag -> SProgress      <$> progressFromRep (RE.tmfp $ P._vtagFrom pvTag) raw
  KDescription    -> Just            $  SDescription fraw
  KEarnedAt       -> SEarnedAt      <$> parseDate fraw
  KRating         -> PRating        <$> parseRating fraw
  KTask           -> Just            $  PTask $ taskPFromString fraw
  KReference      -> SReference     <$> parseGroupPath fraw
  KJoinReference  -> SJoinReference <$> parseGroupPath fraw
  KPriority       -> PPriority      <$> Priority.fromString fraw
  where
    fraw = NEA.head raw


_atomicFromRep :: AV.AtomicValueTag -> NonEmptyArray String -> Maybe AtomicValue
_atomicFromRep avTag raw = case avTag of
  AV.AVTNone         -> Just AV.None
  AV.AVTUnknown      -> Just AV.Unknown
  AV.AVTPInt         -> AV.PInt    <$> Int.fromString fraw
  AV.AVTPNumber      -> AV.PNumber <$> Number.fromString fraw
  AV.AVTPText        -> Just $ AV.PText fraw
  AV.AVTToComplete   -> Just $ AV.ToComplete { done: (fraw == "DONE") || (fraw == "1") }
  AV.AVTPercentI     -> AV.PercentI <$> Int.fromString    (stripPercent fraw)
  AV.AVTPercentN     -> AV.PercentN <$> Number.fromString (stripPercent fraw)
  AV.AVTPercentSign  -> parsePercentSign fraw
  AV.AVTToGetI       -> parseToGetI fraw
  AV.AVTToGetN       -> parseToGetN fraw
  AV.AVTOnTime       -> AV.OnTime <$> parseTime fraw
  AV.AVTOnDate       -> AV.OnDate <$> parseDate fraw
  AV.AVTPerI         -> parsePerI fraw
  AV.AVTPerN         -> parsePerN fraw
  AV.AVTMeasuredI    -> parseMeasuredI fraw
  AV.AVTMeasuredN    -> parseMeasuredN fraw
  AV.AVTMeasuredSign -> parseMeasuredSign fraw
  AV.AVTRangeI       -> parseRangeI fraw
  AV.AVTRangeN       -> parseRangeN fraw
  AV.AVTPTask        -> Just $ AV.PTask $ taskPFromString fraw

  AV.AVTRelTime      -> parseRelTime fraw
  AV.AVTError        -> Just $ AV.Error fraw

  AV.AVTToGetIX       -> ints   >>= case _ of
                           [ got, doing, total ] -> Just $ AV.ToGetIX { got, doing, total }
                           _ -> Nothing
  AV.AVTToGetNX       -> nums   >>= case _ of
                           [ got, doing, total ] -> Just $ AV.ToGetNX { got, doing, total }
                           _ -> Nothing
  AV.AVTToGetMI       -> ints   >>= case _ of
                           [ min, got, max ] -> Just $ AV.ToGetMI { min, got, max }
                           _ -> Nothing
  AV.AVTToGetMIX      -> ints   >>= case _ of
                           [ min, got, doing, max ] -> Just $ AV.ToGetMIX { min, got, doing, max }
                           _ -> Nothing
  AV.AVTToGetMN       -> nums   >>= case _ of
                           [ min, got, max ] -> Just $ AV.ToGetMN { min, got, max }
                           _ -> Nothing
  AV.AVTToGetMNX      -> nums   >>= case _ of
                           [ min, got, doing, max ] -> Just $ AV.ToGetMNX { min, got, doing, max }
                           _ -> Nothing
  AV.AVTOnDateTime    -> uncurry AV.OnDateTime <$> parseDateTime fraw
  AV.AVTTimeRange     -> AV.TimeRange <$> toFromToRec <$> parseRange parseTime parseTime fraw
  AV.AVTDateRange     -> AV.DateRange <$> toFromToRec <$> parseRange parseDate parseDate fraw
  AV.AVTDateTimeRange -> AV.DateTimeRange <$> toFromToRec <$> bimap toDateTimeRec toDateTimeRec <$> parseRange parseDateTime parseDateTime fraw
  AV.AVTRelInt        -> parseRel Int.fromString fraw <#> uncurry AV.RelInt
  AV.AVTRelNum        -> parseRel Number.fromString fraw <#> uncurry AV.RelNum
  AV.AVTPPath         -> AV.PPath <$> parseChain fraw
  AV.AVTTags          -> AV.Tags <$> traverse parseChain (String.split (String.Pattern ",") fraw)
  AV.AVTColor         -> AV.PColor <$> WColor.fromHex fraw
  AV.AVTPRating       -> AV.PRating <$> parseRating fraw
  AV.AVTPPriority     -> AV.PPriority <$> Priority.fromString fraw
  AV.AVTPBoolean      -> parseBooleanAtomic fraw
  AV.AVTPYear         -> AV.PYear <$> Int.fromString fraw
  AV.AVTPID           -> AV.PID <$> Int.fromString fraw
  AV.AVTPRawTags      -> Just $ AV.PRawTags $ RawTags $ parseTags fraw
  where
    fraw = NEA.head raw
    ints = traverse Int.fromString $ splitWithSpace fraw
    nums = traverse Number.fromString $ splitWithSpace fraw
    toDateTimeRec :: (CT.SDate /\ CT.STimeRec) -> { date :: CT.SDate, time :: CT.STimeRec }
    toDateTimeRec (date /\ time) = { date, time }
    toFromToRec :: forall f t. (f /\ t) -> { from :: f, to :: t }
    toFromToRec (from /\ to) = { from, to }


_levelsFromRep :: P.PTValueTag -> NonEmptyArray String -> Maybe P.Levels
_levelsFromRep pvTag raw = case pvTag of
  P.PTLevelsI      -> parseLevelsI unit
  P.PTLevelsN      -> parseLevelsN unit
  P.PTLevelsO      -> parseLevelsO unit
  P.PTLevelsS      -> parseLevelsS unit
  P.PTLevelsP      -> parseLevelsP unit -- { reached: 0.0, levels: [] }
  P.PTLevelsC      -> parseLevelsC unit -- { reached: 0.0, levels: [] }
  P.PTLevelsE      -> parseLevelsE fraw
  _                -> Nothing
  where
    fraw = NEA.head raw
    -- Peel an optional date token from the front of a level entry string,
    -- parsing it with parseDate. For '<…>' tokens the bracketed content is
    -- extracted; for plain tokens the first space-separated word is tried.
    -- Returns (Just date /\ rest) when a date is found, (Nothing /\ original)
    -- when the first token does not parse as a date.
    peelDate :: String -> Maybe CT.SDate /\ String
    peelDate s
      | String.take 1 s == "<" =
          case String.indexOf (String.Pattern "> ") s of
            Just idx -> parseDate (String.take (idx + 1) s) /\ String.drop (idx + 2) s
            Nothing  -> Nothing /\ s
      | otherwise =
          case indexOfSpace s of
            Just spIdx ->
              let tok  = String.take spIdx s
                  rest = String.drop (spIdx + 1) s
              in case parseDate tok of
                Just date -> Just date /\ rest
                Nothing   -> Nothing /\ s
            Nothing -> Nothing /\ s

    parseLevelI str = do
      let (date /\ rest) = peelDate str
      spIdx   <- indexOfSpace rest
      maximum <- Int.fromString (String.take spIdx rest)
      let name = String.drop (spIdx + 1) rest
      pure { date: CT.dateToRec <$> date, maximum, name }
    parseLevelsI _ = do
      reached <- Int.fromString fraw
      let levels = Array.catMaybes $ parseLevelI <$> NEA.tail raw
      pure $ P.LevelsI { reached, levels }
    parseLevelN str = do
      let (date /\ rest) = peelDate str
      spIdx   <- indexOfSpace rest
      maximum <- Number.fromString (String.take spIdx rest)
      let name = String.drop (spIdx + 1) rest
      pure { date: CT.dateToRec <$> date, maximum, name }
    parseLevelsN _ = do
      reached <- Number.fromString fraw
      let levels = Array.catMaybes $ parseLevelN <$> NEA.tail raw
      pure $ P.LevelsN { reached, levels }

    -- maximum is optional: if the first remaining token parses as Int it is
    -- the maximum, otherwise the whole remaining string is the name.
    parseLevelO str =
      let (date /\ rest1) = peelDate str
      in Just $ case indexOfSpace rest1 of
        Just spIdx ->
          let tok   = String.take spIdx rest1
              rest2 = String.drop (spIdx + 1) rest1
          in case Int.fromString tok of
            Just maximum -> { date: CT.dateToRec <$> date, mbMaximum: Just maximum, name: rest2 }
            Nothing      -> { date: CT.dateToRec <$> date, mbMaximum: Nothing,      name: rest1 }
        Nothing ->
          { date: CT.dateToRec <$> date, mbMaximum: Nothing, name: rest1 }
    parseLevelsO _ = do
      reached <- Int.fromString fraw
      let levels = Array.catMaybes $ parseLevelO <$> NEA.tail raw
      pure $ P.LevelsO { reached, levels }

    -- After the optional date the rest is the entire `gives` string.
    parseLevelS str =
      let (date /\ rest) = peelDate str
      in Just { date: CT.dateToRec <$> date, gives: rest }
    parseLevelsS _ = do
      reached <- Int.fromString fraw
      let levels = Array.catMaybes $ parseLevelS <$> NEA.tail raw
      pure $ P.LevelsS { reached, levels }

    -- After the optional date: first token = task status, rest = name.
    parseLevelP str = do
      let (date /\ rest) = peelDate str
      spIdx <- indexOfSpace rest
      let taskStr = String.take spIdx rest
          name    = String.drop (spIdx + 1) rest
      pure { date: CT.dateToRec <$> date, proc: taskPFromString taskStr, name, endDate: Nothing }
    parseLevelsP _ =
      let levels = Array.catMaybes $ parseLevelP <$> NEA.tail raw
      in Just $ P.LevelsP { levels }

    parseLevelsC _ = case splitWithSpace fraw of
      [ levelReachedStr, totalLevelsStr, reachedAtCurrentStr, maximumAtCurrentStr, dateStr ] -> do
        levelReached     <- Int.fromString levelReachedStr
        totalLevels      <- Int.fromString totalLevelsStr
        reachedAtCurrent <- Int.fromString reachedAtCurrentStr
        maximumAtCurrent <- Int.fromString maximumAtCurrentStr
        date             <- parseDate dateStr
        pure $ P.LevelsC { date : Just $ CT.dateToRec date, levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent }
      [ levelReachedStr, totalLevelsStr, reachedAtCurrentStr, maximumAtCurrentStr ] -> do
        levelReached     <- Int.fromString levelReachedStr
        totalLevels      <- Int.fromString totalLevelsStr
        reachedAtCurrent <- Int.fromString reachedAtCurrentStr
        maximumAtCurrent <- Int.fromString maximumAtCurrentStr
        pure $ P.LevelsC { date : Nothing, levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent }
      _ -> Nothing




-- | Reconstruct a Progress value from a Rep marker and raw value string.
-- | Returns Nothing for unrecognised markers or malformed values.

progressFromRep :: RE.TriMarker -> NonEmptyArray String -> Maybe Progress
progressFromRep triMarker raw = RE.ptftm triMarker # case _ of
  P.PTAV avTag     -> AV <$> _atomicFromRep avTag raw
  levelsTag        -> LS <$> _levelsFromRep levelsTag raw


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

parseToGetI :: String -> Maybe AtomicValue
parseToGetI s = do
  (gotS /\ totS) <- splitPair s
  got   <- Int.fromString gotS
  total <- Int.fromString totS
  pure $ AV.ToGetI { got, total }

parseToGetN :: String -> Maybe AtomicValue
parseToGetN s = do
  (gotS /\ totS) <- splitPair s
  got   <- Number.fromString gotS
  total <- Number.fromString totS
  pure $ AV.ToGetN { got, total }

parsePercentSign :: String -> Maybe AtomicValue
parsePercentSign s = case splitWithSpace s of
  [signS, pctS] -> do
    sign <- case signS of
      "+" -> Just 1
      "-" -> Just $ -1
      "*" -> Just 0
      _    -> Nothing
    pct <- Number.fromString (stripPercent pctS)
    pure $ AV.PercentSign { sign, pct }
  _ -> Nothing

parsePerI :: String -> Maybe AtomicValue
parsePerI s = do
  idx    <- indexOfSpace s
  amount <- Int.fromString (String.take idx s)
  let per = String.drop (idx + 1) s
  pure $ AV.PerI { amount, per }

parsePerN :: String -> Maybe AtomicValue
parsePerN s = do
  idx    <- indexOfSpace s
  amount <- Number.fromString (String.take idx s)
  let per = String.drop (idx + 1) s
  pure $ AV.PerN { amount, per }

parseMeasuredI :: String -> Maybe AtomicValue
parseMeasuredI s = do
  idx    <- indexOfSpace s
  amount <- Int.fromString (String.take idx s)
  let measure = String.drop (idx + 1) s
  pure $ AV.MeasuredI { amount, measure }

parseMeasuredN :: String -> Maybe AtomicValue
parseMeasuredN s = do
  idx    <- indexOfSpace s
  amount <- Number.fromString (String.take idx s)
  let measure = String.drop (idx + 1) s
  pure $ AV.MeasuredN { amount, measure }

parseMeasuredSign :: String -> Maybe AtomicValue
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
  amount    <- Number.fromString $ String.take spaceIdx2 rest
  let measure = String.drop (spaceIdx2 + 1) rest
  pure $ AV.MeasuredSign { sign, amount, measure }

parseRangeI :: String -> Maybe AtomicValue
parseRangeI s = case splitWithSpace s of
  [fromS, toS] -> do
    from <- Int.fromString fromS
    to   <- Int.fromString toS
    pure $ AV.RangeI { from, to }
  _ -> Nothing

parseRangeN :: String -> Maybe AtomicValue
parseRangeN s = case splitWithSpace s of
  [fromS, toS] -> do
    from <- Number.fromString fromS
    to   <- Number.fromString toS
    pure $ AV.RangeN { from, to }
  _ -> Nothing

parseLevelsE :: String -> Maybe P.Levels
parseLevelsE s = do
  (reachedS /\ totalS) <- splitPair s
  reached <- Int.fromString reachedS
  total   <- Int.fromString totalS
  pure $ P.LevelsE { reached, total }

parseRelTime :: String -> Maybe AtomicValue
parseRelTime = parseRel parseTime >>> map (uncurry AV.RelTime)

parseRel :: forall a. (String -> Maybe a) -> String -> Maybe (CT.Relation /\ a)
parseRel parseFocus s = do
  idx  <- indexOfSpace s
  let relS   = String.take idx s
      focusS = String.drop (idx + 1) s
  rel  <- case relS of
    ">" -> Just CT.RMoreThan
    "=" -> Just CT.REqual
    "<" -> Just CT.RLessThan
    _   -> Nothing
  focus <- parseFocus focusS
  pure $ rel /\ focus

parseChain :: String -> Maybe (Chain String)
parseChain = String.split (String.Pattern "::") >>> Chain.fromArray

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

-- Date format: <YYYY-MM-DD> or <DD-Mon-YYYY> or DD-Mon-YYYY
parseDate :: String -> Maybe CT.SDate
parseDate s
  | String.take 1 s == "<" = parseAngleBracketDate s
  | otherwise              = parseDayMonYear s

-- <YYYY-MM-DD> or <DD-Mon-YYYY>
parseAngleBracketDate :: String -> Maybe CT.SDate
parseAngleBracketDate s =
  let inner = String.take (String.length s - 2) (String.drop 1 s)
  in case String.split (String.Pattern "-") inner of
    [aS, bS, cS] ->
      (do year <- Int.fromString aS
          mon  <- Int.fromString bS
          day  <- Int.fromString cS
          pure $ CT.dateFromRec { day, mon, year })
      <|> parseDayMonYear inner
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
    [ one ] | not (String.contains (String.Pattern " // ") one) -> Array.catMaybes $ parseTag <$> splitWithSpace one
    some -> Array.catMaybes $ parseTag <$> some


parseTag :: String -> Maybe RawTag
parseTag s =
  case String.split (String.Pattern " // ") s of
    [ idS, contentS ] -> do
      id      <- NEA.fromArray $ String.split (String.Pattern "::") $ String.trim idS
      content <- NEA.fromArray $ String.split (String.Pattern "::") $ String.trim contentS
      pure $ RawTag { id, content }
    _ ->
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


