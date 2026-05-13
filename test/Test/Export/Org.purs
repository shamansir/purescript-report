module Test.Export.Org where

import Prelude

import Effect.Class (liftEffect)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (fail, shouldEqual) as A

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Report (Report, unfold)
import Report.Group (Group(..))
import Report.GroupPath (pathToArray) as GP
import Report.Tabular (items) as Tabular
import Report.Decorator (get, Key(..), Decorator(..)) as Dec
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Progress (Progress(..))
import Report.Decorators.Tags (RawTag)
import Report.Convert.Types (SubjectId(..), ImportError, printImportError)
import Report.Convert.Generic (RR, includeAll)
import Report.Convert.Org (fromOrg) as Org
import Report.Core (ReportFormat(..)) as CT
import Report.Decorators.Tabular.TabularValue (TabularValue(..), TabularAtomicValue(..)) as TV
import Data.Tuple.Nested ((/\))

import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Item (Item(..)) as Impl


simpleOrgSamplePath = "test/games-samples/simple.org" :: String


parseOrg :: String -> Either ImportError (Report (Impl.Subject SubjectId RawTag) Impl.Group (Impl.Item RawTag))
parseOrg = Org.fromOrg @RR @SubjectId @RawTag @RawTag


spec :: Spec Unit
spec =
    describe "org" do

        describe "import" do

            it "parses correct number of groups and items" do
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
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
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
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
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
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

            it "reads task states from heading keyword" do
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
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
                                -- Learn PureScript → STARTED → TDoing
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
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
                    Left err -> A.fail $ printImportError err
                    Right report -> do
                        let subjects = unfold report
                        case Array.head subjects of
                            Nothing -> A.fail "No subjects"
                            Just (_ /\ groups) ->
                                case Array.head groups of
                                    Nothing -> A.fail "No Work group"
                                    Just (_ /\ items) ->
                                        -- "Write tests" is second item with SCHEDULED
                                        case Array.index items 1 of
                                            Nothing -> A.fail "No second item"
                                            Just (Impl.Item item) -> do
                                                let tabRows = Tabular.items item.tabular
                                                case Array.find (\r -> (unwrap r).key == "SCHEDULED") tabRows of
                                                    Nothing -> A.fail "No SCHEDULED tabular entry"
                                                    Just _  -> pure unit

            it "reads logbook into a LevelsP tabular entry" do
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
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
                                                case Array.find (\r -> (unwrap r).key == "logbook") tabRows of
                                                    Nothing -> A.fail "No logbook tabular entry"
                                                    Just logRow ->
                                                        case (unwrap logRow).value of
                                                            TV.TVAtomic (TV.TVDecorator (Dec.SProgress (LevelsP { levels }))) -> do
                                                                Array.length levels `A.shouldEqual` 2
                                                                case Array.head levels of
                                                                    Nothing -> A.fail "No first level"
                                                                    Just lv -> lv.proc `A.shouldEqual` TDone
                                                            _ -> A.fail "Expected LevelsP in logbook tabular"

            it "reads tags from heading suffix" do
                fileText <- liftEffect $ readTextFile UTF8 simpleOrgSamplePath
                case parseOrg fileText of
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
                                                let tags = unwrap item.tags
                                                in Array.length tags `A.shouldEqual` 1
