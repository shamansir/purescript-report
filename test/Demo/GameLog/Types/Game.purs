module GameLog.Types.Game where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)

import Yoga.JSON (class WriteForeign, writeImpl)

import Report.Class
import Report.Core as CT
import Report.Modifiers.Stats (Stats)
import Report.Convert.Keyed (class EncodableKey, encodeKey)
import Report.Tabular as Tabular
import Report.Modifiers.Tabular.TabularValue as TV

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
        , mbTrackedAt :: Maybe CT.SDate
        , stats :: Stats
        }


derive instance Newtype Game _
instance Eq Game where eq a b = toUniqueKey a == toUniqueKey b
instance Ord Game where compare a b = compare (toUniqueKey a) (toUniqueKey b)


toUniqueKey :: Game -> String
toUniqueKey game = gameName game <> " :: " <> encodeKey (gameId game)


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
    decodeTag :: String -> Maybe GameTag
    decodeTag = decodeGameTag
    allTags :: Array GameTag
    allTags = allTags


instance EncodableKey GameId where
    encodeKey = case _ of
        DHL code -> "DHL:" <> code
        LGS code mbPlatform ->
            "LGS:" <> code <> maybe "" (\p -> "(" <> show p <> ")") mbPlatform
        BLG code platform ->
            "BLG:" <> code <> "(" <> show platform <> ")"
        IBL id mbPlatform ->
            "IBL:" <> show id <> maybe "" (\p -> "(" <> show p <> ")") mbPlatform
        EPC code ->
            "EPC:" <> code
    decodeKey = const Nothing -- TODO


instance HasTabular Game where
    i_tabular (Game gameRec) =
        Tabular.empty
            -- # Tabular.insert "name" (TV.TVString gameRec.name)
            # case gameRec.mbTrackedAt of
                Nothing -> identity
                Just trackedAt ->
                    Tabular.insert "trackedAt" $ TV.TVDate trackedAt


instance WriteForeign GameTag where
    writeImpl = tagContent >>> writeImpl


instance IsSubjectId GameId Game where
    s_id = unwrap >>> _.gameId
    s_unique = encodeKey


instance IsSubject GameId Game where
    s_name  = unwrap >>> _.name


instance HasStats Game where
    i_stats = unwrap >>> _.stats


instance HasTags GameTag Game where
    i_tags =
        unwrap >>> case _ of
        { mbSource, mbPlatform } ->
               (maybe [] pure $ SourceTag   <$> mbSource)
            <> (maybe [] pure $ PlatformTag <$> mbPlatform)


instance Show GameId where
    show = case _ of
        DHL code -> "DHL:" <> code
        LGS code mbPlatform ->
            "LGS:" <> code <> maybe "" (\p -> "(" <> show p <> ")") mbPlatform
        BLG code platform ->
            "BLG:" <> code <> "(" <> show platform <> ")"
        IBL id mbPlatform ->
            "IBL:" <> show id <> maybe "" (\p -> "(" <> show p <> ")") mbPlatform
        EPC code ->
            "EPC:" <> code


decodeGameTag :: String -> Maybe GameTag
decodeGameTag str =
    case str of
        "Dhall" -> Just $ SourceTag S_Dhall
        "LSeq"  -> Just $ SourceTag S_Logseq
        "IBL"   -> Just $ SourceTag S_IBL
        "BLG"   -> Just $ SourceTag S_Backloggery
        _       -> Just $ PlatformTag $ GLT.parsePlatform str