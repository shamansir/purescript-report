module Test.Export.Smos where

import Prelude

import Effect.Class (liftEffect)

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (contains, Pattern(..)) as String
import Data.Tuple.Nested ((/\))

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (fail, shouldEqual) as A

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Utils (shouldEqual) as U

import Report (Report, unfold)
import Report.Group (Group(..))
import Report.GroupPath (pathToArray) as GP
import Report.Tabular (items) as Tabular
import Report.Decorator (get, Key(..), Decorator(..)) as Dec
import Report.Decorators.Tabular.TabularValue (TabularValue(..), TabularAtomicValue(..)) as TV
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Progress (Progress(..))
import Report.Decorators.Tags (RawTag)
import Report.Convert.Types (SubjectId(..), ImportError, printImportError, Input(..), Output(..))
import Report.Convert.Generic (RR, includeAll)
import Report.Convert.Smos (fromSmos) as Smos
import Report.Convert.Converter (Command(..), runCommand, defaultOptions)
import Report.Core (ReportFormat(..), SDate(..), monthToInt) as CT

import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Item (Item(..)) as Impl


simpleSmosSamplePath = "test/games-samples/simple.smos" :: String


parseSmos :: String -> Either ImportError (Report (Impl.Subject SubjectId RawTag) Impl.Group (Impl.Item RawTag))
parseSmos = Smos.fromSmos @RR @SubjectId @RawTag @RawTag


spec :: Spec Unit
spec =
    describe "smos" do

        describe "import" do

            it "parses correct number of groups and items" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err ->
                        A.fail $ "Parse failed: " <> printImportError err
                    Right report -> do
                        let subjects = unfold report
                        Array.length subjects `A.shouldEqual` 1
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) -> do
                                -- Work, Work::Backlog, Personal
                                Array.length groups `A.shouldEqual` 3
                                -- Work: 2 leaf items
                                case Array.head groups of
                                    Nothing -> A.fail "No first group"
                                    Just (_ /\ items) ->
                                        Array.length items `A.shouldEqual` 2
                                -- Work::Backlog: 1 leaf item
                                case Array.index groups 1 of
                                    Nothing -> A.fail "No second group"
                                    Just (_ /\ items) ->
                                        Array.length items `A.shouldEqual` 1
                                -- Personal: 1 leaf item
                                case Array.index groups 2 of
                                    Nothing -> A.fail "No third group"
                                    Just (_ /\ items) ->
                                        Array.length items `A.shouldEqual` 1

            it "parses group paths correctly" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) -> do
                                case Array.head groups of
                                    Just (Group g0 /\ _) ->
                                        GP.pathToArray g0.path `A.shouldEqual` ["Work"]
                                    Nothing -> A.fail "No first group"
                                case Array.index groups 1 of
                                    Just (Group g1 /\ _) ->
                                        GP.pathToArray g1.path `A.shouldEqual` ["Work", "Backlog"]
                                    Nothing -> A.fail "No second group"
                                case Array.index groups 2 of
                                    Just (Group g2 /\ _) ->
                                        GP.pathToArray g2.path `A.shouldEqual` ["Personal"]
                                    Nothing -> A.fail "No third group"

            it "parses item titles correctly" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) -> do
                                case Array.head groups of
                                    Nothing -> A.fail "No groups"
                                    Just (_ /\ items) -> do
                                        case Array.head items of
                                            Just (Impl.Item i) -> i.title `A.shouldEqual` "Fix login bug"
                                            Nothing -> A.fail "No first item"
                                        case Array.index items 1 of
                                            Just (Impl.Item i) -> i.title `A.shouldEqual` "Write tests"
                                            Nothing -> A.fail "No second item"
                                case Array.index groups 1 of
                                    Nothing -> A.fail "No Backlog group"
                                    Just (_ /\ items) ->
                                        case Array.head items of
                                            Just (Impl.Item i) -> i.title `A.shouldEqual` "Refactor auth module"
                                            Nothing -> A.fail "No item in Backlog"
                                case Array.index groups 2 of
                                    Nothing -> A.fail "No Personal group"
                                    Just (_ /\ items) ->
                                        case Array.head items of
                                            Just (Impl.Item i) -> i.title `A.shouldEqual` "Learn PureScript"
                                            Nothing -> A.fail "No item in Personal"

            it "reads task states from state-history" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) -> do
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) -> do
                                        -- Fix login bug → DONE
                                        case Array.head items of
                                            Nothing -> A.fail "No first item"
                                            Just (Impl.Item item) ->
                                                case Dec.get Dec.KTask item.decorators of
                                                    Just (Dec.PTask TDone) -> pure unit
                                                    _ -> A.fail "Expected PTask TDone for Fix login bug"
                                        -- Write tests → TODO
                                        case Array.index items 1 of
                                            Nothing -> A.fail "No second item"
                                            Just (Impl.Item item) ->
                                                case Dec.get Dec.KTask item.decorators of
                                                    Just (Dec.PTask TTodo) -> pure unit
                                                    _ -> A.fail "Expected PTask TTodo for Write tests"
                                -- Learn PureScript → STARTED maps to TDoing
                                case Array.index groups 2 of
                                    Nothing -> A.fail "No Personal group"
                                    Just (_ /\ items) ->
                                        case Array.head items of
                                            Nothing -> A.fail "No item in Personal"
                                            Just (Impl.Item item) ->
                                                case Dec.get Dec.KTask item.decorators of
                                                    Just (Dec.PTask TDoing) -> pure unit
                                                    _ -> A.fail "Expected PTask TDoing for Learn PureScript"

            it "reads SCHEDULED timestamp into tabular" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) ->
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) ->
                                        -- "Write tests" is second item with SCHEDULED timestamp
                                        case Array.index items 1 of
                                            Nothing -> A.fail "No second item"
                                            Just (Impl.Item item) -> do
                                                let tabRows = Tabular.items item.tabular
                                                case Array.find (\r -> (unwrap r).key == "SCHEDULED") tabRows of
                                                    Nothing -> A.fail "No SCHEDULED tabular entry"
                                                    Just _ -> pure unit

            it "reads contents into description decorator" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) ->
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) ->
                                        case Array.head items of
                                            Nothing -> A.fail "No first item"
                                            Just (Impl.Item item) ->
                                                case Dec.get Dec.KDescription item.decorators of
                                                    Just (Dec.SDescription desc) ->
                                                        desc `A.shouldEqual` "This is a known login issue"
                                                    _ -> A.fail "Expected SDescription for Fix login bug"

            it "reads state-history time into earned-at decorator" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) ->
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) ->
                                        case Array.head items of
                                            Nothing -> A.fail "No first item"
                                            Just (Impl.Item item) ->
                                                case Dec.get Dec.KEarnedAt item.decorators of
                                                    Just (Dec.SEarnedAt (CT.SDate d)) -> do
                                                        d.year `A.shouldEqual` 2025
                                                        CT.monthToInt d.month `A.shouldEqual` 1
                                                        d.day `A.shouldEqual` 15
                                                    _ -> A.fail "Expected SEarnedAt for Fix login bug"

            it "reads logbook into a LevelsP tabular entry" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) ->
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) ->
                                        case Array.head items of
                                            Nothing -> A.fail "No first item"
                                            Just (Impl.Item item) -> do
                                                let tabRows = Tabular.items item.tabular
                                                -- logbook entry should be last tabular row (after SCHEDULED on "Write tests"; this item has no timestamps)
                                                let mbLogRow = Array.find (\r -> (unwrap r).key == "logbook") tabRows
                                                case mbLogRow of
                                                    Nothing -> A.fail "No logbook tabular entry"
                                                    Just logRow ->
                                                        case (unwrap logRow).value of
                                                            TV.TVAtomic (TV.TVDecorator (Dec.SProgress (LevelsP { levels }))) -> do
                                                                Array.length levels `A.shouldEqual` 2
                                                                case Array.head levels of
                                                                    Nothing -> A.fail "No first level"
                                                                    Just lv -> do
                                                                        lv.proc `A.shouldEqual` TDone
                                                                        lv.name `A.shouldEqual` "entry-1"
                                                            _ -> A.fail "Expected LevelsP in logbook tabular"

            it "stores full state-history as a LevelsP tabular entry" do
                fileText <- liftEffect $ readTextFile UTF8 simpleSmosSamplePath
                case parseSmos fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) ->
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) ->
                                        -- "Write tests" is second item, has two history entries
                                        case Array.index items 1 of
                                            Nothing -> A.fail "No second item"
                                            Just (Impl.Item item) -> do
                                                let tabRows = Tabular.items item.tabular
                                                let mbShRow = Array.find (\r -> (unwrap r).key == "state-history") tabRows
                                                case mbShRow of
                                                    Nothing -> A.fail "No state-history tabular entry"
                                                    Just shRow ->
                                                        case (unwrap shRow).value of
                                                            TV.TVAtomic (TV.TVDecorator (Dec.SProgress (LevelsP { levels }))) -> do
                                                                Array.length levels `A.shouldEqual` 2
                                                                case Array.head levels of
                                                                    Nothing -> A.fail "No first level"
                                                                    Just lv -> lv.proc `A.shouldEqual` TTodo
                                                                case Array.index levels 1 of
                                                                    Nothing -> A.fail "No second level"
                                                                    Just lv -> lv.proc `A.shouldEqual` TDoing
                                                            _ -> A.fail "Expected LevelsP in state-history tabular"

        describe "export" do

            it "round-trips smos→smos preserving structure" do
                let cmdToRun = Convert
                        { from: CT.Smos, to: CT.Smos }
                        { input: FileInput simpleSmosSamplePath, output: NullOutput }
                        defaultOptions []
                commandResult <- liftEffect $ runCommand cmdToRun
                either
                    (\err -> A.fail $ "Failed: " <> printImportError err)
                    (\(_ /\ smosStr) -> smosStr `U.shouldEqual` expectedRoundTripSmos)
                    commandResult

            it "produces smos from rep input with version header" do
                let cmdToRun = Convert
                        { from: CT.Rep, to: CT.Smos }
                        { input: FileInput "test/games-samples/AstralChain.rep"
                        , output: NullOutput
                        }
                        defaultOptions []
                commandResult <- liftEffect $ runCommand cmdToRun
                either
                    (\err -> A.fail $ "Failed: " <> printImportError err)
                    (\(_ /\ smosStr) -> do
                        String.contains (String.Pattern "version: 2") smosStr `A.shouldEqual` true
                        String.contains (String.Pattern "header: File") smosStr `A.shouldEqual` true
                        String.contains (String.Pattern "header: Stats") smosStr `A.shouldEqual` true
                    )
                    commandResult


-- Expected output after smos → smos round-trip.
-- Paths use Title-case headers, not the original lowercase path= from the file.
-- yaml-next sorts object keys alphabetically (Map-backed).
-- contents appears before header (c < h), real state times preserved via SEarnedAt.
expectedRoundTripSmos :: String
expectedRoundTripSmos = """value:
  - entry:
      header: Work
      properties:
        path: Work
    forest:
      - entry:
          contents: This is a known login issue
          header: Fix login bug
          logbook:
            - end: '2025-01-14 00:00:00.000'
              start: '2025-01-14 00:00:00.000'
            - end: '2025-01-15 00:00:00.000'
              start: '2025-01-15 00:00:00.000'
          state-history:
            - state: DONE
              time: '2025-01-15 00:00:00.000'
          tags:
            - bug
      - entry:
          header: Write tests
          state-history:
            - state: TODO
              time: '2025-01-15 00:00:00.000'
            - state: STARTED
              time: '2025-01-10 00:00:00.000'
          timestamps:
            SCHEDULED: <2025-01-20>
      - entry:
          header: Backlog
          properties:
            path: Work::Backlog
        forest:
          - entry:
              header: Refactor auth module
              state-history:
                - state: TODO
                  time: '2025-01-01 00:00:00.000'
  - entry:
      header: Personal
      properties:
        path: Personal
    forest:
      - entry:
          header: Learn PureScript
          state-history:
            - state: STARTED
              time: '2025-01-10 00:00:00.000'
          tags:
            - hobby
version: 2
"""
