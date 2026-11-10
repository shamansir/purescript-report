module Test.AtomicValue where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.String (joinWith, trim) as String
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Effect.Exception (Error, try)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (warn) as Console
import Foreign (Foreign)
import Node.Buffer (toString) as Buffer
import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

import Yoga.JSON (read_, readJSON_, writeJSON) as JSON

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import Dodo as D

import Report.Core (SDate(..), Relation(..), monthFromInt) as CT
import Report.Chain (Chain)
import Report.Chain (fromArray, fromNEArray) as Chain
import Report.AtomicValue (AtomicValue(..))
import Report.AtomicValue (valueTagOf, encodeAtomicValueTag, toReadableString) as AV
import Report.Decorators.Progress (Progress(..), PTValueTag(..))
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Rating (Rating(..))
import Report.Decorators.Priority (priorityA, priorityNum)
import Report.Decorators.Tags (RawTag(..), RawTags(..))
import Report.Convert.Rep.Export (_atomicDocLines) as Rep
import Report.Convert.Rep.Import.Parser (_atomicFromRep) as Rep
import Report.Convert.Rep.Keys (ptftm) as Rep
import Report.Convert.Text.Decorators.Progress (encodeProgress, decodeProgress) as Text
import Report.Convert.Dhall.Export (RenderedAs(..), _atomicToDhall) as Dhall
import Report.Convert.Org.Export (_atomicProperties) as Org
import Report.Convert.Org.Import.Properties (propertyToAtomic) as OrgProps
import Utils.Data.WColor (fromHex) as WColor


-- the number of constructors in `AtomicValue`, update when adding new ones
atomicValueConstructorsCount :: Int
atomicValueConstructorsCount = 44


date :: Int -> Int -> Int -> CT.SDate
date day mon year = CT.SDate { day, month : CT.monthFromInt mon, year }


chain :: Array String -> Chain String
chain = Chain.fromArray >>> fromMaybe (Chain.fromNEArray $ NEA.singleton "?")


rawTag :: Array String -> Array String -> RawTag
rawTag id content =
    RawTag
        { id : fromMaybe (NEA.singleton "?") $ NEA.fromArray id
        , content : fromMaybe (NEA.singleton "?") $ NEA.fromArray content
        }


-- at least one sample of every `AtomicValue` option, with some edge cases
samples :: Array AtomicValue
samples =
    [ None
    , Unknown
    , PText "Some text"
    , PNumber 2.5
    , PNumber (-0.75)
    , PInt 42
    , PInt (-7)
    , PBoolean true
    , PBoolean false
    , PYear 2025
    , PID 1234
    , ToComplete { done : true }
    , ToComplete { done : false }
    , ToGetI { got : 3, total : 10 }
    , ToGetN { got : 1.5, total : 4.0 }
    , ToGetIX { got : 3, doing : 2, total : 10 }
    , ToGetNX { got : 1.5, doing : 0.5, total : 4.0 }
    , ToGetMI { min : -5, got : 0, max : 5 }
    , ToGetMN { min : 1.0, got : 2.5, max : 10.0 }
    , ToGetMIX { min : 0, got : 3, doing : 1, max : 7 }
    , ToGetMNX { min : 0.0, got : 3.5, doing : 1.5, max : 7.0 }
    , PercentI 45
    , PercentN 0.45
    , PercentSign { sign : -1, pct : 0.55 }
    , PercentSign { sign : 0, pct : 0.1 }
    , MeasuredI { amount : 12, measure : "km" }
    , MeasuredN { amount : 2005.7, measure : "ft" }
    , MeasuredSign { sign : 1, amount : 32.0, measure : "%" }
    , PerI { amount : 5, per : "day" }
    , PerN { amount : 1.5, per : "sec" }
    , OnDate $ date 12 8 2025
    , OnTime { hrs : 7, min : 54, sec : 3 }
    , OnDateTime (date 1 1 2024) { hrs : 23, min : 5, sec : 0 }
    , TimeRange { from : { hrs : 7, min : 0, sec : 0 }, to : { hrs : 8, min : 30, sec : 15 } }
    , DateRange { from : date 1 2 2024, to : date 3 4 2025 }
    , DateTimeRange
        { from : { date : date 1 2 2024, time : { hrs : 1, min : 2, sec : 3 } }
        , to   : { date : date 3 4 2025, time : { hrs : 4, min : 5, sec : 6 } }
        }
    , RangeI { from : 1, to : 5 }
    , RangeI { from : -3, to : -1 }
    , RangeN { from : 0.5, to : 2.5 }
    , RelTime CT.RMoreThan { hrs : 40, min : 0, sec : 0 }
    , RelInt CT.RLessThan 10
    , RelNum CT.REqual 3.25
    , PTask TDone
    , PTask TWait
    , PPath $ chain [ "games", "switch", "zelda" ]
    , Tags [ chain [ "platform", "switch" ], chain [ "genre" ] ]
    , PRawTags $ RawTags [ rawTag [ "platform", "switch" ] [ "Switch" ], rawTag [ "rpg" ] [ "RPG" ] ]
    , PRating $ Rating { value : 7.5, max : 10 }
    , PPriority priorityA
    , PPriority $ priorityNum 3
    , Error "something went wrong"
    ]
    <> (Array.fromFoldable $ PColor <$> WColor.fromHex "#ff8800")


describeAV :: AtomicValue -> String
describeAV aval = show (AV.encodeAtomicValueTag $ AV.valueTagOf aval) <> " " <> AV.toReadableString aval


roundTrips :: forall m. MonadThrow Error m => String -> (AtomicValue -> Maybe AtomicValue) -> m Unit
roundTrips format roundTrip = samplesCameBack format samples $ roundTrip <$> samples


-- compares samples with what came back from the conversion, in the same order
samplesCameBack :: forall m. MonadThrow Error m => String -> Array AtomicValue -> Array (Maybe AtomicValue) -> m Unit
samplesCameBack format theSamples results =
    if Array.length results /= Array.length theSamples
        then fail $ format <> ": expected " <> show (Array.length theSamples) <> " values, got " <> show (Array.length results)
        else
            case Array.catMaybes $ Array.zipWith check theSamples results of
                [] -> pure unit
                failures -> fail $ format <> ":\n" <> String.joinWith "\n" failures
    where
        check aval = case _ of
            Just result | result == aval -> Nothing
            Just result -> Just $ describeAV aval <> " came back as " <> describeAV result
            Nothing -> Just $ describeAV aval <> " failed to be read back"


viaJson :: AtomicValue -> Maybe AtomicValue
viaJson = JSON.writeJSON >>> JSON.readJSON_


viaRep :: AtomicValue -> Maybe AtomicValue
viaRep aval =
    let
        marker /\ docLines = Rep._atomicDocLines aval
        lines = D.print D.plainText D.twoSpaces <$> docLines
    in case Rep.ptftm marker of
        PTAV avTag -> Rep._atomicFromRep avTag lines
        _ -> Nothing


-- all the samples as a Dhall list of tagged values, to be converted with `dhall-to-json`
samplesAsDhall :: String
samplesAsDhall =
    "let T = ../test/games-samples/Types.dhall in\n[ "
        <> String.joinWith "\n, " (toDhall <$> samples)
        <> "\n]\n"
    where
        toDhall aval = "T.tag " <> case Dhall._atomicToDhall aval of
            Dhall.OneLine doc -> print doc
            Dhall.MutliLine docs -> String.joinWith "\n" $ print <$> docs
        print = D.print D.plainText D.twoSpaces


viaDhall :: Aff (Maybe (Array (Maybe AtomicValue)))
viaDhall = liftEffect do
    let dhallPath = "./output/atomic-value-samples.dhall"
    writeTextFile UTF8 dhallPath samplesAsDhall
    result <- try $ execSync ("dhall-to-json --file " <> dhallPath) >>= Buffer.toString UTF8
    pure $ case result of
        Left _ -> Nothing
        Right jsonStr -> Just $ map JSON.read_ $ fromMaybe [] (JSON.readJSON_ jsonStr :: Maybe (Array Foreign))


-- `None` and `Unknown` are the absence of the value, so they are not written to org properties
orgSamples :: Array AtomicValue
orgSamples = Array.filter (not <<< isEmptyValue) samples
    where
        isEmptyValue = case _ of
            None -> true
            Unknown -> true
            _ -> false


orgProperties :: AtomicValue -> Array (String /\ String)
orgProperties = Org._atomicProperties >>> map (map $ D.print D.plainText D.twoSpaces)


viaOrg :: AtomicValue -> Maybe AtomicValue
viaOrg aval = case orgProperties aval of
    [ key /\ value ] -> OrgProps.propertyToAtomic key (String.trim value)
    _ -> Nothing


viaEncodedValue :: AtomicValue -> Maybe AtomicValue
viaEncodedValue aval =
    uncurry Text.decodeProgress (Text.encodeProgress $ AV aval) >>= case _ of
        AV result -> Just result
        LS _ -> Nothing


spec :: Spec Unit
spec =
    describe "atomic values" do
        it "samples cover all the options" do
            let tags = Array.nubEq $ AV.encodeAtomicValueTag <$> AV.valueTagOf <$> samples
            Array.length tags `shouldEqual` atomicValueConstructorsCount
        it "round-trip through JSON" do
            roundTrips "JSON" viaJson
        it "round-trip through REP" do
            roundTrips "REP" viaRep
        it "round-trip through encoded (editable) value" do
            roundTrips "EncodedValue" viaEncodedValue
        it "round-trip through org properties" do
            samplesCameBack "Org" orgSamples $ viaOrg <$> orgSamples
        it "round-trip through Dhall (requires `dhall-to-json`)" do
            viaDhall >>= case _ of
                Just results -> samplesCameBack "Dhall" samples results
                Nothing -> liftEffect $ Console.warn "`dhall-to-json` is not available (run tests in `nix develop`), skipped the Dhall round-trip"
