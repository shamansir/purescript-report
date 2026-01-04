module GameLog.Types where

import Prelude

import Foreign (Foreign, F, fail, ForeignError(..))

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String as String
import Data.Int as Int
import Data.Number as Number

import Control.Alt ((<|>))

import Yoga.JSON (class WriteForeign, class ReadForeign, writeImpl, readImpl)


type Game =
    { id :: String
    , name :: String
    , platform :: Platform
    , playtime :: { hrs :: Int, min :: Int, sec :: Int }
    }


data Platform
    = MacOS
    | PC
    | Playstation5
    | Playstation4
    | Playstation3
    | PlaystationPortable
    | Steam
    | NintendoSwitch
    | Unknown
    | WiiU
    | Android -- Google Play
    | IOS (Maybe IOSAcc)
    | Oculus -- | MetaQuest
    | Web
    | Shared
    | Platform String


data IOSAcc
    = DE
    | OG


allPlatforms =
    [ Steam
    , NintendoSwitch
    , IOS (Just DE)
    , IOS (Just OG)
    , Oculus -- | MetaQuest
    , Android -- Google Play
    , Playstation5
    , Playstation4
    , Playstation3
    , PlaystationPortable
    , Web
    , MacOS
    , PC
    -- , Unknown
    , WiiU
    , Shared
    ] :: Array Platform


parsePlatform ∷ String → Platform
parsePlatform =
    case _ of
        "PC" -> PC
        "PC (Microsoft Windows)" -> PC
        "PS5" -> Playstation5
        "PS4" -> Playstation4
        "PS3" -> Playstation3
        "PlayStation 5" -> Playstation5
        "PlayStation 4" -> Playstation4
        "PlayStation 3" -> Playstation3
        "Switch" -> NintendoSwitch
        "Nintendo Switch" -> NintendoSwitch
        "PSP" -> PlaystationPortable
        "WiiU" -> WiiU
        "Wii U" -> WiiU
        "iOS" -> IOS Nothing
        "IOS" -> IOS Nothing
        "Steam" -> Steam
        "Oculus" -> Oculus
        "MetaQuest" -> Oculus
        "Meta" -> Oculus
        "MacOS" -> MacOS
        "Mac OS" -> MacOS
        "Android" -> Android
        "Web" -> Web
        "Shared" -> Shared
        "" -> Unknown
        "?" -> Unknown
        "undefined" -> Unknown
        "null" -> Unknown
        other -> Platform other


instance Show Platform where
    show =
        case _ of
            PC -> "PC"
            Playstation5 -> "PS5"
            Playstation4 -> "PS4"
            Playstation3 -> "PS3"
            PlaystationPortable -> "PSP"
            Steam -> "Steam"
            NintendoSwitch -> "Switch"
            MacOS -> "MacOS"
            WiiU -> "WiiU"
            IOS mbAcc -> "iOS" <> case mbAcc of
                Just DE -> "-DE"
                Just OG -> "-OG"
                Nothing -> ""
            Oculus -> "Meta"
            Android -> "GPlay"
            Unknown -> "?"
            Web -> "Web"
            Shared -> "[Shared]"
            Platform other -> "<" <> other <> ">"


derive instance Eq IOSAcc
derive instance Ord IOSAcc
derive instance Eq Platform
derive instance Ord Platform


encodePlatform :: Platform -> String
encodePlatform =
    case _ of
        PC -> "PC"
        Playstation5 -> "PS5"
        Playstation4 -> "PS4"
        Playstation3 -> "PS3"
        PlaystationPortable -> "PSP"
        Steam -> "Steam"
        NintendoSwitch -> "Switch"
        MacOS -> "MacOS"
        WiiU -> "WiiU"
        IOS _ -> "iOS"
        Oculus -> "Oculus"
        Android -> "Android"
        Unknown -> "-"
        Web -> "Web"
        Shared -> "Shared"
        Platform other -> other


instance ReadForeign Platform where
    readImpl :: Foreign -> F Platform
    readImpl f = (readImpl f :: F String) <#> parsePlatform


instance WriteForeign Platform where
    writeImpl :: Platform -> Foreign
    writeImpl = encodePlatform >>> writeImpl


data Completion
    = NotSet
    | NotStarted
    | Beaten
    | Completed
    | Unfinished
    | Continuous
    | Dropped
    | Status String


derive instance Eq Completion
derive instance Ord Completion


parseCompletion ∷ String → Completion
parseCompletion =
    case _ of
        "Beaten" -> Beaten
        "Completed" -> Completed
        "Unfinished" -> Unfinished
        "Not Started" -> NotStarted
        "Continuous" -> Continuous
        "Dropped" -> Dropped
        "" -> NotSet
        "-" -> NotSet
        other -> Status other


encodeCompletion :: Completion -> String
encodeCompletion =
    case _ of
        NotSet -> "-"
        NotStarted -> "Not Started"
        Beaten -> "Beaten"
        Completed -> "Completed"
        Unfinished -> "Unfinished"
        Continuous -> "Continuous"
        Dropped -> "Dropped"
        Status status -> status


instance Show Completion where
    show =
        case _ of
            NotSet -> "-"
            NotStarted -> "Not Started"
            Beaten -> "Beaten"
            Completed -> "Completed"
            Unfinished -> "Unfinished"
            Continuous -> "Continuous"
            Dropped -> "Dropped"
            Status status -> "<" <> status <> ">"


instance ReadForeign Completion where
    readImpl :: Foreign -> F Completion
    readImpl f = (readImpl f :: F String) <#> parseCompletion


instance WriteForeign Completion where
    writeImpl :: Completion -> Foreign
    writeImpl = encodeCompletion >>> writeImpl


data PlayStatus
    = Played
    | Playing
    | Unplayed
    | Undefined
    | PlayStatus String


parsePlayStatus ∷ String → PlayStatus
parsePlayStatus =
    case _ of
        "Played" -> Played
        "Playing" -> Playing
        "Unplayed" -> Unplayed
        "" -> Undefined
        "-" -> Undefined
        other -> PlayStatus other


instance Show PlayStatus where
    show =
        case _ of
            Played -> "Played"
            Playing -> "Playing"
            Unplayed -> "Unplayed"
            Undefined -> "Undefined"
            PlayStatus status -> "<" <> status <> ">"


encodePlayStatus ∷ PlayStatus → String
encodePlayStatus =
    case _ of
        Played -> "Played"
        Playing -> "Playing"
        Unplayed -> "Unplayed"
        Undefined -> "-"
        PlayStatus status -> status


instance ReadForeign PlayStatus where
    readImpl :: Foreign -> F PlayStatus
    readImpl f = (readImpl f :: F String) <#> parsePlayStatus


instance WriteForeign PlayStatus where
    writeImpl :: PlayStatus -> Foreign
    writeImpl = encodePlayStatus >>> writeImpl


data YesNo
    = Yes
    | No


parseYesNo :: String -> YesNo
parseYesNo =
    case _ of
        "Yes" -> Yes
        "true" -> Yes
        "True" -> Yes
        _ -> No


encodeYesNo :: YesNo -> String
encodeYesNo =
    case _ of
        Yes -> "Yes"
        No -> "No"


instance ReadForeign YesNo where
    readImpl :: Foreign -> F YesNo
    readImpl f = (readImpl f :: F String) <#> parseYesNo


instance WriteForeign YesNo where
    writeImpl :: YesNo -> Foreign
    writeImpl = encodeYesNo >>> writeImpl


instance Show YesNo where
    show = case _ of
        Yes -> "Yes"
        No -> "No"


data Achievement = Achievement -- TODO


instance ReadForeign Achievement where
    readImpl _ = pure Achievement


instance WriteForeign Achievement where
    writeImpl _ = writeImpl "ACH"


-- data AchievemntsEarned
--     = AchievemntEarnedsUnknown
--     | AchievemntsEarned Int


-- data AchievemntsTotal
--     = AchievemntsTotalUnknown
--     | AchievemntsTotal Int


-- data Rating
--     = RatingNotSet
--     | Rating


type Achievements = { earned :: Maybe Int, total :: Maybe Int, list :: Array Achievement }


data Progress
    = NoProgress
    | ProgressUnknown
    | Percentage Int
    | Achievements Achievements
    | Playtime { hours :: Number }
    | Joined (NEA.NonEmptyArray Progress)
    -- | Manual String


instance Show Progress where
    show =
        case _ of
            NoProgress -> "-"
            ProgressUnknown -> "?"
            Percentage n -> show n <> "%"
            Playtime { hours } -> show hours <> "hrs"
            Achievements { earned, total } ->
                fromMaybe "?" (show <$> earned) <> "/" <> fromMaybe "?" (show <$> total)
            Joined progresses -> String.joinWith ", " $ show <$> NEA.toArray progresses


instance WriteForeign Progress where
    writeImpl =
        case _ of
            NoProgress -> writeImpl "-"
            ProgressUnknown -> writeImpl "?"
            Percentage n -> writeImpl $ "%" <> show n
            Playtime { hours } -> writeImpl $ "h" <> show hours
            Achievements { earned, total, list } ->
                writeImpl
                    { status : writeImpl $ fromMaybe "_" (show <$> earned) <> "/" <> fromMaybe "_" (show <$> total)
                    , list : writeImpl $ writeImpl <$> list
                    }
            Joined progresses ->
                writeImpl $ encodeProgress <$> NEA.toArray progresses


instance ReadForeign Progress where
    readImpl json = tryFromString json <|> tryAchievements json <|> tryJoined json
        where
            tryFromString json = do
                asString :: String <- readImpl json
                case String.take 1 asString of
                    "-" -> pure NoProgress
                    "?" -> pure ProgressUnknown
                    "%" -> case Int.fromString $ String.drop 1 asString of
                        Just n -> pure $ Percentage n
                        Nothing -> pure ProgressUnknown
                    "h" -> case Number.fromString $ String.drop 1 asString of
                        Just n -> pure $ Playtime { hours : n }
                        Nothing -> pure ProgressUnknown
                    -- FIXME: decode Achievements & Joined
                    _ -> fail $ ForeignError "not a string"
            tryAchievements json = do
                achievements :: Achievements <- readImpl json
                pure $ Achievements achievements
            tryJoined json = do
                joined :: NonEmptyArray Progress <- readImpl json
                pure $ Joined joined


encodeProgress ∷ Progress → String
encodeProgress =
    case _ of
        NoProgress -> "-"
        ProgressUnknown -> "?"
        Percentage n -> "%" <> show n
        Playtime { hours } -> "h" <> show hours
        Achievements { earned, total, list } ->
            fromMaybe "_" (show <$> earned) <> "/" <> fromMaybe "_" (show <$> total)
        Joined progresses -> String.joinWith "|" $ encodeProgress <$> NEA.toArray progresses
