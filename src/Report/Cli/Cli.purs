module Report.Cli where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Data.Foldable (fold)
import Data.String (toUpper) as String

import Control.Alt ((<|>))

import Options.Applicative
import Data.Semigroup ((<>))

import Report.Core (ReportFormat(..))


data Input
    = FileInput String
    | SampleIn {- which -}
    | StdInput


data Output
    = StdOutput
    | FileOutput String


data Mode
    = View Input ReportFormat
    | Convert { from :: ReportFormat, to :: ReportFormat } { input :: Input, output :: Output }
    -- | Edit


data ModeFlag
    = FView
    | FConvert


type Options = Mode


main :: Effect Unit
main = runProgram =<< execParser opts


runProgram :: Options -> Effect Unit
runProgram opts = do
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
        FView -> View theInput from
        FConvert -> Convert { from, to } { input : theInput, output : theOutput }



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
    View input format -> "View " <> descInput input <> " in " <> descFormat format <> " format."
    Convert { from, to } { input, output } -> "Convert " <> descInput input <> " from " <> descFormat from <> " to " <> descFormat to <> " and write it to " <> descOutput output
    where
        descInput = case _ of
            FileInput path -> "file at " <> path
            SampleIn -> "sample"
            StdInput -> "`stdin`"
        descOutput = case _ of
            FileOutput path -> "file at " <> path
            StdOutput -> "`stdout`"
        descFormat = formatToStr >>> String.toUpper