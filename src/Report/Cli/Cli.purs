module Report.Cli where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)

import Data.Either (Either(..), either)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Traversable (traverse)

import Control.Alt ((<|>))

import Options.Applicative

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Report.Core (ReportFormat(..))
import Report.Convert.Types (Input(..), Output(..))
import Report.Convert.Converter (Command(..))
import Report.Convert.Converter as Conv


main :: Effect Unit
main =
    execParser commandInfo
    >>= loadCommands
    >>= traverse Conv.runCommand
    >>= mempty


type CliConfig = Either ConfigFileSource Conv.Command


loadCommands :: CliConfig -> Effect (NonEmptyArray Conv.Command)
loadCommands (Right cmd) = pure $ pure cmd
loadCommands (Left (ConfigFileSource configFilePath)) = do
    configFileText <- readTextFile UTF8 configFilePath
    either
        (Conv.printYamlDecodeError >>> throw)
        pure
        $ map _.commands
        $ Conv.commandsFromYaml configFileText


appArgsParser :: Parser CliConfig
appArgsParser =
    (Left <$> configFileSource) <|> (Right <$> pureArgsCommandParser)


pureArgsCommandParser :: Parser Conv.Command
pureArgsCommandParser = ado
    theInput <- input
    theOutput <- output

    from <- from
    to <- to

    in Convert { from, to } { input : theInput, output : theOutput } Conv.defaultOptions [] -- TODO


input :: Parser Input
input = stdInput <|> fileInput <|> pure StdInput


output :: Parser Output
output = stdOutput <|> fileOutput <|> pure StdOutput


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
from = Conv.formatFromStr <$> strOption
    (  long "from"
    <> short 'f'
    <> metavar "FROM-FORMAT"
    <> value (Conv.formatToStr Rep)
    <> help "Input format" )


to :: Parser ReportFormat
to = Conv.formatFromStr <$> strOption
    (  long "to"
    <> short 't'
    <> value (Conv.formatToStr Rep)
    <> metavar "TO-FORMAT"
    <> help "Output format" )


newtype ConfigFileSource = ConfigFileSource String


configFileSource :: Parser ConfigFileSource
configFileSource = ConfigFileSource <$> strOption
    (  long "conf"
    <> short 'c'
    <> metavar "CONFIG"
    <> help "Configuration file" )


commandInfo :: ParserInfo CliConfig
commandInfo = info (appArgsParser <**> helper)
    ( fullDesc
    <> progDesc "Convert reports from one format to another, or just view them"
    <> header "purescript-report - unified format for displaying and manipulating different kinds of structured reports" )


