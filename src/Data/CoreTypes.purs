module Input.CoreTypes where

import Prelude

import Data.Array ((:))
import Data.Array (filter, reverse, splitAt, intersperse) as Array
import Data.Date (Month(..)) as N
import Data.Either (Either(..))
import Data.Int (floor, toNumber, trunc, fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Number (pow) as Number
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray) as CU
import Data.Number.Format as NF
import Data.Tuple.Nested ((/\), type (/\))

import Foreign (F)

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


newtype SDate = SDate { day :: Int, month :: SMonth, year :: Int }

derive newtype instance Eq SDate
instance Show SDate where
    show (SDate { day, month, year }) =
        show day <> " " <> show month <> " " <> show year
instance ReadForeign SDate where
    readImpl f = (readImpl f :: F SDateRec) <#> dateFromRec
instance WriteForeign SDate where
    writeImpl = dateToRec >>> writeImpl


data Language
    = En
    | Ru
    | Es
    | De
    | Uk
    | Jp


derive instance Eq Language
derive instance Ord Language


langToCode :: Language -> String
langToCode = case _ of
    En -> "en"
    De -> "de"
    Es -> "es"
    Ru -> "ru"
    Uk -> "uk"
    Jp -> "jp"


langFromCode :: String -> Language
langFromCode = case _ of
    "en" -> En
    "de" -> De
    "es" -> Es
    "ru" -> Ru
    _    -> En


langToName :: Language -> String
langToName = case _ of
    En -> "English"
    De -> "German"
    Es -> "Spanish"
    Ru -> "Russian"
    Uk -> "Ukrainian"
    Jp -> "Japanese"


levelToName :: LanguageLevel -> String
levelToName = case _ of
    A1 -> "A1"
    A2 -> "A2"
    B1 -> "B1"
    B2 -> "B2"
    C1 -> "C1"
    C2 -> "C2"
    Native -> "Native"


instance ReadForeign Language where
    readImpl frgn = (readImpl frgn :: F String) <#> langFromCode


instance WriteForeign Language where
    writeImpl = writeImpl <<< langToCode


newtype Url = Url String


derive instance Newtype Url _


url :: String -> Url
url = Url


data SMonth
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


derive instance Eq SMonth


instance showMonth :: Show SMonth where
  show Jan = "January"
  show Feb = "February"
  show Mar = "March"
  show Apr = "April"
  show May = "May"
  show Jun = "June"
  show Jul = "July"
  show Aug = "August"
  show Sep = "September"
  show Oct = "October"
  show Nov = "November"
  show Dec = "December"


toNativeMonth :: SMonth -> N.Month
toNativeMonth = case _ of
  Jan -> N.January
  Feb -> N.February
  Mar -> N.March
  Apr -> N.April
  May -> N.May
  Jun -> N.June
  Jul -> N.July
  Aug -> N.August
  Sep -> N.September
  Oct -> N.October
  Nov -> N.November
  Dec -> N.December


newtype Year = Year Int -- TODO: replace with Year from Data.Date


derive instance Newtype Year _
derive newtype instance Show Year


type MDate = SMonth /\ Year
type StartDate = MDate
type EndDate = Either Current MDate


data Current = Current


toLeadingZero :: Int -> String
toLeadingZero n | n < 10    = "0" <> show n
toLeadingZero n | otherwise = show n


toLeadingZero2 :: Int -> String
toLeadingZero2 n | n < 10    = "00" <> show n
toLeadingZero2 n | n < 100   = "0" <> show n
toLeadingZero2 n | otherwise = show n


monthToLeadingZero :: SMonth -> String
monthToLeadingZero = case _ of
  Jan -> "01"
  Feb -> "02"
  Mar -> "03"
  Apr -> "04"
  May -> "05"
  Jun -> "06"
  Jul -> "07"
  Aug -> "08"
  Sep -> "09"
  Oct -> "10"
  Nov -> "11"
  Dec -> "12"


data LanguageLevel
    = A1
    | A2
    | B1
    | B2
    | C1
    | C2
    | Native


current = Left Current :: EndDate

starts :: SMonth -> Year -> StartDate
starts month year = month /\ year

ends :: SMonth -> Year -> EndDate
ends month year = Right $ month /\ year



--| NB: Month is starting from 01 and ends at 12
monthFromInt :: Int -> SMonth
monthFromInt = case _ of
    1 -> Jan
    2 -> Feb
    3 -> Mar
    4 -> Apr
    5 -> May
    6 -> Jun
    7 -> Jul
    8 -> Aug
    9 -> Sep
    10 -> Oct
    11 -> Nov
    12 -> Dec
    _ -> Jan


monthToInt :: SMonth -> Int
monthToInt = case _ of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12


type SDateRec = { day :: Int, mon :: Int, year :: Int }


--| NB: Month is starting from 01 and ends at 12
dateFromRec :: SDateRec -> SDate
dateFromRec { day, mon, year } =
    SDate { day, month : monthFromInt mon, year }


dateToRec :: SDate -> SDateRec
dateToRec (SDate { day, month, year }) =
    { day, mon : monthToInt month, year }


formatWithCommas :: Int -> String
formatWithCommas n =
    let
        s = show n -- Int.toStringAs Int.decimal n
        sign /\ digits =
            if String.take 1 s == "-"
                then "-" /\ String.drop 1 s
                else "" /\ s

        charArr = Array.reverse $ CU.toCharArray digits

        grouped = charArr
             # chunksOf 3
            <#> Array.reverse
            <#> CU.fromCharArray
             # Array.intersperse ","
             # Array.reverse
             # String.joinWith ""

    in sign <> grouped


formatNumWithCommas :: { fixedTo :: Maybe Int } -> Number -> String
formatNumWithCommas { fixedTo } num =
    let
        integerPart = Int.trunc num

        fractionalPart = num - Int.toNumber integerPart
    in
        formatWithCommas integerPart <> (String.drop 1 $ NF.toStringWith (NF.fixed $ fromMaybe 6 fixedTo) fractionalPart)


chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n arr
  | n <= 0    = [arr]
  | otherwise =
      case Array.splitAt n arr of
        { before, after } ->
            case after of
                [] -> [before]
                _ -> before : chunksOf n after


parseDate :: String -> Maybe SDate
parseDate = String.split (String.Pattern " ") >>> case _ of
    [ dayS, monS, yearS ] ->
        (\day month year -> SDate { day, month, year })
            <$> Int.fromString dayS
            <*> parseMonth monS
            <*> Int.fromString yearS
    _ -> Nothing


parseTZDate :: String -> Maybe SDate
parseTZDate = String.take 10 >>> String.split (String.Pattern "-") >>> case _ of
    [ yearS, monS, dayS ] ->
        (\day month year -> SDate { day, month, year })
            <$> Int.fromString dayS
            <*> (monthFromInt <$> Int.fromString monS)
            <*> Int.fromString yearS
    _ -> Nothing


parseMonth :: String -> Maybe SMonth
parseMonth = case _ of
    "Jan" -> Just Jan
    "Feb" -> Just Feb
    "Mar" -> Just Mar
    "Apr" -> Just Apr
    "May" -> Just May
    "Jun" -> Just Jun
    "Jul" -> Just Jul
    "Jun" -> Just Jun
    "Aug" -> Just Aug
    "Sep" -> Just Sep
    "Oct" -> Just Oct
    "Nov" -> Just Nov
    "Dec" -> Just Dec
    _ -> Nothing
