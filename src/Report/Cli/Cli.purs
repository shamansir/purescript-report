module Report.Cli where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Foldable (fold)
import Data.String (toUpper) as String

import Node.Encoding (Encoding(..))
-- import Node.FS.Sync (readTextFile)
import Node.FS.Sync (readTextFile)
-- import Node.FS.Sync (inp)
import Node.Stream (Readable)
import Node.Stream as Stream
import Node.EventEmitter (on_)
import Node.Process as Process

import Control.Alt ((<|>))

import Options.Applicative
import Data.Semigroup ((<>))

import Report.Class
import Report.Core (ReportFormat(..))
import Report.Convert.Types
import Report.Decorators.Tags (RawTag, RawTagKind, TagAction(..))
-- import Report.Convert.Rep.Import
import Report.Convert.Rep as Report
import Report.Convert.Generic (class ToImport, RR)
import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Subject (mapTags) as SubjImpl
import Report.Impl.Item (Item(..)) as Impl
import Report.Impl.Item (mapTags) as ItemImpl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Tag (Tag(..)) as Impl


type Process = TagAction RawTagKind RawTag


data Input
    = FileInput String
    | SampleIn {- which -}
    | StdInput


data Output
    = Screen
    | StdOutput
    | FileOutput String


data Mode
    = View Input Output ReportFormat (Array Process)
    | Convert { from :: ReportFormat, to :: ReportFormat } { input :: Input, output :: Output } (Array Process)
    -- | Edit


data ModeFlag
    = FView
    | FConvert


type Options = Mode


main :: Effect Unit
main = runProgram =<< execParser opts


runProgram :: Options -> Effect Unit
runProgram opts = do
    case opts of
        View theInput theOutput srcFormat _ -> do
            mbReportStr <- case theInput of
                FileInput filePath -> Just <$> readTextFile UTF8 filePath
                SampleIn -> pure Nothing
                StdInput -> readStdin
            pure unit
        Convert fmt pipe _ -> pure unit
    Console.log $ modeDescription opts
    pure unit


optsParser :: Parser Options
optsParser = ado
    modeK <- modeFlag

    theInput <- input
    theOutput <- output

    from <- from
    to <- to

    in case modeK of
        FView -> View theInput theOutput from [] -- TODO
        FConvert -> Convert { from, to } { input : theInput, output : theOutput } [] -- TODO


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


modeFlag :: Parser ModeFlag
modeFlag =
    flag FView FConvert
    ( long "convert"
    <> short 'c'
    <> help "Convert instead of just viewing"
    )


opts :: ParserInfo Options
opts = info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for purescript-optparse" )


modeDescription :: Mode -> String
modeDescription = case _ of
    View input output format process -> "View " <> descInput input <> " in " <> descFormat format <> " format and show it in " <> descOutput output <> ". " <> descProcess process
    Convert { from, to } { input, output } process -> "Convert " <> descInput input <> " from " <> descFormat from <> " to " <> descFormat to <> " and write it to " <> descOutput output <> ". " <> descProcess process
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
        Rep -> Report.fromRep @RR @SubjectId @RawTag @RawTag source
        _ -> Left $ ImportError "Unsupported format"


-- exportTextFor = case _ of
--                 Json  -> reportToExport # Report.toJson  @x @subj_id @subj_tag @item_tag includeRule
--                 Dhall -> reportToExport # Report.toDhall @x @subj_id @subj_tag @item_tag includeRule
--                 Org   -> reportToExport # Report.toOrg   @x @subj_id @subj_tag @item_tag includeRule
--                 Rep   -> reportToExport # Report.toRep   @x @subj_id @subj_tag @item_tag includeRule
--                 Text  -> reportToExport # Report.toText  @x @subj_id @subj_tag @item_tag includeRule