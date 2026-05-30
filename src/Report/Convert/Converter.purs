module Report.Convert.Converter where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)

import Foreign (ForeignError, renderForeignError)
import Foreign.Object as FO

import Data.Either (Either(..), note, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.String (toUpper, joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap, rmap)

import Data.Argonaut.Core (toObject, Json) as AG
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error as AG
import Data.Argonaut.Decode.Combinators as AG
import Data.YAML.Foreign.Decode (parseYAMLToJson)

import Node.Encoding (Encoding(..))
-- import Node.FS.Sync (readTextFile)
import Node.FS.Sync (readTextFile, writeTextFile)
-- import Node.FS.Sync (inp)
import Node.Stream as Stream
import Node.Process as Process
import Node.ChildProcess (execSync)
import Node.Buffer (toString) as Buffer

import Report.Core (ReportFormat(..))
import Report.Convert.Types (ImportError(..), Input(..), Output(..), SampleId(..), printImportError)
import Report.Decorators.Tags (RawTag, RawTagKind, TagAction)
-- import Report.Convert.Rep.Import
import Report.Impl.Subject (SubjectId)
import Report.Convert.Generic (RR, RawReport, IncludeRule(..))
import Report.Convert.Dhall (toDhall, fromDhall) as Report
import Report.Convert.Json (toJson, fromJson) as Report
import Report.Convert.Rep (toRep, fromRep) as Report
import Report.Convert.Org (toOrg, fromOrg) as Report
import Report.Convert.Text (toText) as Report
import Report.Convert.Smos (toSmos, fromSmos) as Report


type Process = TagAction RawTagKind RawTag


type Options =
    { verbose :: Boolean
    -- , logDhallJson :: Boolean
    }


data Command
    = Convert { from :: ReportFormat, to :: ReportFormat } { input :: Input, output :: Output } (Array Process)


type CommandRunResult = Either ImportError (RawReport /\ String)


defaultOptions :: Options
defaultOptions = { verbose : false }


runCommand :: Options -> Command -> Effect CommandRunResult
runCommand options command = do
    when options.verbose do
        Console.log $ commandDescription command
        Console.log "------------------------------"
        -- Console.log "------------------------------"
    returnValue <- case command of
        Convert fmt pipe _ -> do
            mbReportStr <- case pipe.input of
                FileInput filePath ->
                    case fmt.from of
                        Dhall -> do
                            jsonFromDhallBuf <- execSync $ "dhall-to-json --file " <> filePath
                            jsonFromDhallText <- Buffer.toString UTF8 jsonFromDhallBuf
                            -- when options.logDhallJson $ Console.log jsonFromDhallText
                            pure $ Just jsonFromDhallText
                        _ -> Just <$> readTextFile UTF8 filePath
                SampleIn _ -> pure Nothing
                StdInput -> readStdin
            let eConvertedReport = readReport fmt.from =<< note (FailedToReadInput pipe.input) mbReportStr
            case eConvertedReport of
                Right theReport -> do
                    let convertedReport = convertReport fmt.to theReport
                    case pipe.output of
                        Screen -> Console.log $ convertReport fmt.to theReport
                        StdOutput -> writeStdout (convertReport fmt.to theReport) *> pure unit
                        FileOutput filePath -> do
                            writeTextFile UTF8 filePath (convertReport fmt.to theReport)
                        NullOutput -> pure unit
                    pure $ Right (theReport /\ convertedReport)
                Left importError -> do
                    Console.log "Errors:\n"
                    Console.log $ printImportError importError
                    pure $ Left importError
    -- when options.verbose $ Console.log "\n------------------------------\n"
    pure returnValue


commandDescription :: Command -> String
commandDescription = case _ of
    Convert fmt pipe process ->
        if (fmt.from == fmt.to)
            then "View " <> descInput pipe.input <> " in " <> descFormat fmt.from <> " format and show it in " <> descOutput pipe.output <> ". " <> descProcess process
            else "Convert " <> descInput pipe.input <> " from " <> descFormat fmt.from <> " to " <> descFormat fmt.to <> " and write it to " <> descOutput pipe.output <> ". " <> descProcess process
    where
        descInput = case _ of
            FileInput path -> "file at " <> path
            SampleIn sampleId -> "sample"
            StdInput -> "`stdin`"
        descOutput = case _ of
            Screen -> "screen"
            FileOutput path -> "file at " <> path
            StdOutput -> "`stdout`"
            NullOutput -> "nowhere"
        descFormat = formatToStr >>> String.toUpper
        descProcess _ = "" -- TODO


readStdin :: Effect (Maybe String)
readStdin = do
    Stream.readString Process.stdin UTF8


writeStdout :: String -> Effect Boolean
writeStdout = do
    Stream.writeString Process.stdout UTF8


readReport :: ReportFormat -> String -> Either ImportError RawReport
readReport format source =
    case format of
        Rep   -> Report.fromRep   @RR @SubjectId @RawTag @RawTag source
        Json  -> Report.fromJson  @RR @SubjectId @RawTag @RawTag source
        Dhall -> Report.fromDhall @RR @SubjectId @RawTag @RawTag source
        Smos  -> Report.fromSmos  @RR @SubjectId @RawTag @RawTag source
        Org   -> Report.fromOrg   @RR @SubjectId @RawTag @RawTag source
        _ -> Left $ UnsupportedFormat format


convertReport :: ReportFormat -> RawReport -> String
convertReport format rawReport = case format of
    Json  -> rawReport # Report.toJson  @RR @SubjectId @RawTag @RawTag IncludeAll
    Dhall -> rawReport # Report.toDhall @RR @SubjectId @RawTag @RawTag IncludeAll
    Org   -> rawReport # Report.toOrg   @RR @SubjectId @RawTag @RawTag IncludeAll
    Rep   -> rawReport # Report.toRep   @RR @SubjectId @RawTag @RawTag IncludeAll
    Text  -> rawReport # Report.toText  @RR @SubjectId @RawTag @RawTag IncludeAll
    Smos  -> rawReport # Report.toSmos  @RR @SubjectId @RawTag @RawTag IncludeAll


formatFromStr :: String -> ReportFormat
formatFromStr = case _ of
    "json"  -> Json
    "dhall" -> Dhall
    "org"   -> Org
    "rep"   -> Rep
    "text"  -> Text
    "smos"  -> Smos
    _       -> Text


formatToStr :: ReportFormat -> String
formatToStr = case _ of
    Json  -> "json"
    Dhall -> "dhall"
    Org   -> "org"
    Rep   -> "rep"
    Text  -> "text"
    Smos  -> "smos"


instance DecodeJson Command where
    decodeJson s = do
        cmdObj <- maybe (Left $ AG.TypeMismatch "Command is not an object.") Right $ AG.toObject s
        from   <- formatFromStr <$> AG.getField cmdObj "from"
        to     <- formatFromStr <$> AG.getField cmdObj "to"
        input  <- (FileInput    <$> AG.getField cmdObj "in-file")  <|> (SampleIn <$> SampleId <$> AG.getField   cmdObj "in-sample")  <|> pure StdInput
        output <- (FileOutput   <$> AG.getField cmdObj "out-file") <|> (const Screen          <$>  getUnitField cmdObj "out-screen") <|> pure StdOutput
        pure $ Convert { from, to } { input, output } []
        where
            getUnitField :: FO.Object AG.Json -> String -> Either AG.JsonDecodeError Unit
            getUnitField obj f = AG.getField obj f
            -- unitVal :: Either AG.JsonDecodeError Unit
            -- unitVal = pure unit



type YamlDecodeError = Either (NonEmptyList ForeignError) AG.JsonDecodeError
type YamlDecodeResult a = Either YamlDecodeError a


decodeFromYaml :: forall a. DecodeJson a => String -> YamlDecodeResult a
decodeFromYaml = parseYAMLToJson >>> runExcept  >>> lmap Left >>> flip bind (decodeJson >>> lmap Right)


type Setup = { options :: Maybe Options, commands :: NonEmptyArray Command }


loadSetupFromYaml :: String -> YamlDecodeResult Setup
loadSetupFromYaml = decodeFromYaml


printYamlDecodeError :: YamlDecodeError -> String
printYamlDecodeError = either (NEL.toUnfoldable >>> map renderForeignError >>> String.joinWith " ; ") AG.printJsonDecodeError
