module Report.Cli where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String (toUpper) as String

import Node.Encoding (Encoding(..))
-- import Node.FS.Sync (readTextFile)
import Node.FS.Sync (readTextFile, writeTextFile)
-- import Node.FS.Sync (inp)
import Node.Stream as Stream
import Node.Process as Process
import Node.ChildProcess (execSync)
import Node.Buffer (toString) as Buffer

import Control.Alt ((<|>))

import Options.Applicative

import Report.Class
import Report.Core (ReportFormat(..))
import Report.Convert.Types
import Report.Decorators.Tags (RawTag, RawTagKind, TagAction)
-- import Report.Convert.Rep.Import
import Report.Convert.Generic (RR, IncludeRule(..))
import Report.Convert.Dhall (toDhall, fromDhall) as Report
import Report.Convert.Json (toJson, fromJson) as Report
import Report.Convert.Rep (toRep, fromRep) as Report
import Report.Convert.Org (toOrg) as Report
import Report.Convert.Text (toText) as Report



type Process = TagAction RawTagKind RawTag


data Command
    = Convert { from :: ReportFormat, to :: ReportFormat } { input :: Input, output :: Output } (Array Process)
    -- | Edit


data ModeFlag
    = FView
    | FConvert


type Options = Command


main :: Effect Unit
main = runProgram =<< execParser opts


runProgram :: Options -> Effect Unit
runProgram opts = do
    Console.log $ commandDescription opts
    Console.log "------------------------------"
    Console.log "------------------------------"
    case opts of
        Convert fmt pipe _ -> do
            mbReportStr <- case pipe.input of
                FileInput filePath ->
                    case fmt.from of
                        Dhall -> do
                            jsonFromDhallBuf <- execSync $ "dhall-to-json --file " <> filePath
                            jsonFromDhallText <- Buffer.toString UTF8 jsonFromDhallBuf
                            pure $ Just jsonFromDhallText
                        _ -> Just <$> readTextFile UTF8 filePath
                SampleIn -> pure Nothing
                StdInput -> readStdin
            let eConvertedReport = readReport fmt.from =<< note (FailedToReadInput pipe.input) mbReportStr
            case eConvertedReport of
                Right theReport ->
                    case pipe.output of
                        Screen -> Console.log $ convertReport fmt.to theReport
                        StdOutput -> writeStdout (convertReport fmt.to theReport) *> pure unit
                        FileOutput filePath -> do
                            writeTextFile UTF8 filePath (convertReport fmt.to theReport)
                Left importError -> do
                    Console.log "Errors:\n"
                    Console.log $ printImportError importError
    Console.log "\n------------------------------\n"


optsParser :: Parser Options
optsParser = ado
    theInput <- input
    theOutput <- output

    from <- from
    to <- to

    in Convert { from, to } { input : theInput, output : theOutput } [] -- TODO


input :: Parser Input
input = stdInput <|> fileInput <|> pure StdInput


output :: Parser Output
output = stdOutput <|> fileOutput <|> pure StdOutput


formatFromStr :: String -> ReportFormat
formatFromStr = case _ of
    "json" -> Json
    "dhall" -> Dhall
    "org" -> Org
    "rep" -> Rep
    "text" -> Text
    _ -> Text


formatToStr :: ReportFormat -> String
formatToStr = case _ of
    Json -> "json"
    Dhall -> "dhall"
    Org -> "org"
    Rep -> "rep"
    Text -> "text"


fileInput :: Parser Input
fileInput = FileInput <$> strOption
    (  long "file-in"
    <> short 'i'
    <> metavar "IN-FILENAME"
    <> help "Input file" )


stdInput :: Parser Input
stdInput = flag' StdInput
    (  long "stdin"
    <> help "Read from stdin" )


fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
    (  long "file-out"
    <> short 'o'
    <> metavar "OUT-FILENAME"
    <> help "Input file" )


stdOutput :: Parser Output
stdOutput = flag' StdOutput
    (  long "stdout"
    <> help "Write to stdout" )


from :: Parser ReportFormat
from = formatFromStr <$> strOption
    (  long "from"
    <> short 'f'
    <> metavar "FROM-FORMAT"
    <> value (formatToStr Rep)
    <> help "Input format" )


to :: Parser ReportFormat
to = formatFromStr <$> strOption
    (  long "to"
    <> short 't'
    <> value (formatToStr Rep)
    <> metavar "TO-FORMAT"
    <> help "Output format" )


opts :: ParserInfo Options
opts = info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Convert reports from one format to another, or just view them"
    <> header "purescript-report - unified format for displaying and manipulating different kinds of structured reports" )


commandDescription :: Command -> String
commandDescription = case _ of
    Convert fmt pipe process ->
        if (fmt.from == fmt.to)
            then "View " <> descInput pipe.input <> " in " <> descFormat fmt.from <> " format and show it in " <> descOutput pipe.output <> ". " <> descProcess process
            else "Convert " <> descInput pipe.input <> " from " <> descFormat fmt.from <> " to " <> descFormat fmt.to <> " and write it to " <> descOutput pipe.output <> ". " <> descProcess process
    where
        descInput = case _ of
            FileInput path -> "file at " <> path
            SampleIn -> "sample"
            StdInput -> "`stdin`"
        descOutput = case _ of
            Screen -> "screen"
            FileOutput path -> "file at " <> path
            StdOutput -> "`stdout`"
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
        Rep   -> Report.fromRep  @RR @SubjectId @RawTag @RawTag source
        Json  -> Report.fromJson @RR @SubjectId @RawTag @RawTag source
        Dhall -> Report.fromDhall @RR @SubjectId @RawTag @RawTag source
        _ -> Left $ UnsupportedFormat format


convertReport :: ReportFormat -> RawReport -> String
convertReport format rawReport = case format of
    Json  -> rawReport # Report.toJson  @RR @SubjectId @RawTag @RawTag IncludeAll
    Dhall -> rawReport # Report.toDhall @RR @SubjectId @RawTag @RawTag IncludeAll
    Org   -> rawReport # Report.toOrg   @RR @SubjectId @RawTag @RawTag IncludeAll
    Rep   -> rawReport # Report.toRep   @RR @SubjectId @RawTag @RawTag IncludeAll
    Text  -> rawReport # Report.toText  @RR @SubjectId @RawTag @RawTag IncludeAll


