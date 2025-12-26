module GameLog.Types.Game where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)

import Report.Class (class IsSubject, class IsTag, TagColors)
import Report.Modifiers.Stats (Stats)

import GameLog.Types as GLT

{- Game -}


data GameId
    = DHL String
    | LGS String (Maybe GLT.Platform)
    | BLG String GLT.Platform
    | IBL Int (Maybe GLT.Platform)
    | EPC String


derive instance Eq GameId
derive instance Ord GameId


data Source
    = S_Dhall
    | S_IBL
    | S_Logseq
    | S_Backloggery


derive instance Eq Source
derive instance Ord Source


allSources = [ S_Dhall, S_IBL, S_Logseq, S_Backloggery ] :: Array Source


newtype Game =
    Game
        { name :: String
        , gameId :: GameId
        , mbPlatform :: Maybe GLT.Platform
        , mbSource :: Maybe Source
        , stats :: Stats
        }


derive instance Newtype Game _
derive instance Eq Game -- TODO: compare by GameId+Platform?
derive instance Ord Game -- TODO: compare by GameId+Platform?


gameName :: Game -> String
gameName = unwrap >>> _.name


gameId :: Game -> GameId
gameId = unwrap >>> _.gameId



data GameTag
    = PlatformTag GLT.Platform
    | SourceTag Source


allTags :: Array GameTag
allTags =
    ( SourceTag <$> allSources )
    <>
    ( PlatformTag <$> GLT.allPlatforms )


derive instance Eq GameTag


instance IsTag GameTag where
    tagContent :: GameTag -> String
    tagContent = case _ of
        PlatformTag platform -> show platform
        SourceTag source ->
            case source of
                S_Dhall -> "Dhall"
                S_Logseq -> "LSeq"
                S_IBL -> "IBL"
                S_Backloggery -> "BLG"
    tagColors :: GameTag -> TagColors
    tagColors = case _ of
        PlatformTag _ ->
            { background : "transparent"
            , text : "black"
            , border : "black"
            }
        SourceTag _ ->
            { background : "lightgray"
            , text : "white"
            , border : "gray"
            }
    allTags :: Array GameTag
    allTags = allTags


instance IsSubject GameId GameTag Game where
    s_id = unwrap >>> _.gameId
    s_name = unwrap >>> _.name
    s_stats = unwrap >>> _.stats
    s_tags = unwrap >>> case _ of
        { mbSource, mbPlatform } ->
               (maybe [] pure $ SourceTag   <$> mbSource)
            <> (maybe [] pure $ PlatformTag <$> mbPlatform)
