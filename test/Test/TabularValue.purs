module Test.TabularValue where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\))
import Data.FunctorWithIndex (mapWithIndex)

import Yoga.JSON (readJSON_, writeJSON) as JSON

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

import Report (build, unfold)
import Report.Core (Relation(..)) as CT
import Report.Group (Group(..))
import Report.Group (mkg, setTabular) as Group
import Report.AtomicValue (AtomicValue(..))
import Report.Tabular (Tabular, Item(..), fromArray', items) as Tabular
import Report.Decorators.Tabular.TabularValue (TabularValue(..))
import Report.Decorators.Tabular.Logbook (Logbook(..))
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (RawTag)
import Report.Decorators.Tags (Tags(..)) as Tags
import Report.Decorators.Stats (defaultStats) as Stats
import Report.Decorator (empty) as Decorators
import Report.Impl.Subject (Subject(..), SubjectId(..)) as Impl
import Report.Impl.Item (Item(..)) as Impl
import Report.Convert.Types (printImportError)
import Report.Convert.Generic (RR, RawReport, includeAll)
import Report.Convert.Rep (toRep, fromRep) as Rep

import Test.AtomicValue (samples, date) as AV


table :: Tabular.Tabular AtomicValue
table =
    Tabular.fromArray'
        [ { key : "count", label : "Count", value : PInt 3 }
        , { key : "name", label : "Full Name", value : PText "Some name" }
        , { key : "when", label : "When", value : OnDate $ AV.date 1 2 2024 }
        , { key : "rel", label : "Relation", value : RelInt CT.REqual 5 }
        ]


emptyTable :: Tabular.Tabular AtomicValue
emptyTable = Tabular.fromArray' []


logbook :: Logbook
logbook =
    Logbook
        [ { name : "first run", task : TDone, start : Just { day : 1, mon : 2, year : 2024 }, end : Just { day : 3, mon : 4, year : 2025 } }
        , { name : "", task : TTodo, start : Nothing, end : Nothing }
        , { name : "only end", task : TCanceled, start : Nothing, end : Just { day : 5, mon : 6, year : 2025 } }
        , { name : "only start", task : TDoing, start : Just { day : 7, mon : 8, year : 2023 }, end : Nothing }
        , { name : "wait", task : TWait, start : Nothing, end : Nothing }
        , { name : "now", task : TNow, start : Nothing, end : Nothing }
        , { name : "later", task : TLater, start : Nothing, end : Nothing }
        ]


-- every atomic value, and every composite value, including the empty and the nested ones
samples :: Array TabularValue
samples =
    (TVAtomic <$> AV.samples)
    <>
    [ TVAtomic $ PText "first line\nsecond line"
    , TVAtomic $ PText ""
    , TVValues [ PInt 1, PText "a", PText "+++ 2", AV.date 1 1 2020 # OnDate ]
    , TVValues []
    , TVValuesNest [ [ PInt 1, PInt 2 ], [ PText "x" ], [] ]
    , TVValuesNest []
    , TVTabulars [ table, emptyTable, table ]
    , TVTabulars []
    , TVPair (TVAtomic $ PInt 1) (TVValues [ PText "a", PText "b" ])
    , TVPair (TVPair (TVTabulars [ table ]) (TVLogbook logbook)) (TVValuesNest [ [ PInt 7 ] ])
    , TVTabularsNest { direct : [ table ], parts : [ PInt 5 /\ [ table, emptyTable ], PText "part" /\ [] ] }
    , TVTabularsNest { direct : [], parts : [] }
    , TVLogbook logbook
    , TVLogbook $ Logbook []
    ]


samplesTabular :: Array (Tabular.Item TabularValue)
samplesTabular =
    mapWithIndex
        (\idx value -> Tabular.Item { key : "v" <> show idx, label : "Value " <> show idx, value })
        samples


sampleReport :: RawReport
sampleReport =
    build
        [ Impl.Subject
            { id : Impl.SubjectId "subj"
            , name : "Subject"
            , stats : Stats.defaultStats
            , tabular : Tabular.fromArray' (unwrapItem <$> samplesTabular)
            , tags : []
            }
          /\
            [ Group.setTabular (Tabular.fromArray' (unwrapItem <$> samplesTabular)) (Group.mkg [ "group" ] "Group")
              /\
                [ Impl.Item
                    { title : "Item"
                    , decorators : Decorators.empty
                    , tags : Tags.Tags ([] :: Array RawTag)
                    , tabular : Tabular.fromArray' (unwrapItem <$> samplesTabular)
                    }
                ]
            , Group.mkg [ "group", "nested" ] "Nested Group"
              /\
                [ Impl.Item
                    { title : "Nested Item"
                    , decorators : Decorators.empty
                    , tags : Tags.Tags ([] :: Array RawTag)
                    , tabular : Tabular.fromArray' (unwrapItem <$> samplesTabular)
                    }
                ]
            ]
        ]
    where
        unwrapItem (Tabular.Item rec) = rec


-- compares the tabular values with the samples, in the same order
tabularCameBack :: String -> Array (Tabular.Item TabularValue) -> Maybe String
tabularCameBack place results =
    if Array.length results /= Array.length samplesTabular
        then Just $ place <> ": expected " <> show (Array.length samplesTabular) <> " values, got " <> show (Array.length results)
        else
            case Array.catMaybes $ Array.zipWith check samplesTabular results of
                [] -> Nothing
                failures -> Just $ place <> ":\n" <> String.joinWith "\n" failures
    where
        -- `TabularValue` has no `Show` instance, so the values are shown as JSON
        check (Tabular.Item sample) (Tabular.Item result) =
            if sample.key == result.key && sample.label == result.label && sample.value == result.value
                then Nothing
                else Just $ "  " <> sample.key <> " " <> JSON.writeJSON sample.value <> "\n    came back as " <> result.key <> " " <> JSON.writeJSON result.value


spec :: Spec Unit
spec =
    describe "tabular values" do
        it "round-trip through JSON" do
            let
                check sample = case (JSON.readJSON_ $ JSON.writeJSON sample) :: Maybe TabularValue of
                    Just result | result == sample -> Nothing
                    Just result -> Just $ JSON.writeJSON sample <> " came back as " <> JSON.writeJSON result
                    Nothing -> Just $ JSON.writeJSON sample <> " failed to be read back"
            case Array.mapMaybe check samples of
                [] -> pure unit
                failures -> fail $ "JSON:\n" <> String.joinWith "\n" failures
        it "round-trip through REP (subject, groups and items)" do
            let repStr = Rep.toRep @RR @Impl.SubjectId @RawTag @RawTag includeAll sampleReport
            case (Rep.fromRep @RR @Impl.SubjectId @RawTag @RawTag repStr :: _ RawReport) of
                Left err -> fail $ "Parse failed: " <> printImportError err <> "\n" <> repStr
                Right report ->
                    let
                        subjectsWithGroups = unfold report
                        subjectsTabulars = subjectsWithGroups <#> \(Impl.Subject subj /\ _) -> "subject" /\ Tabular.items subj.tabular
                        groups = Array.concat $ subjectsWithGroups <#> \(_ /\ groupsWithItems) -> groupsWithItems
                        groupsTabulars = groups <#> \(Group group /\ _) -> ("group " <> group.title) /\ Tabular.items group.tabular
                        itemsTabulars = Array.concat $ groups <#> \(_ /\ theItems) -> theItems <#> \(Impl.Item item) -> ("item " <> item.title) /\ Tabular.items item.tabular
                        -- the nested group has no tabular values
                        expected = subjectsTabulars <> Array.filter (\(place /\ _) -> place /= "group Nested Group") groupsTabulars <> itemsTabulars
                    in
                        if Array.length expected /= 4
                            then fail $ "Expected one subject, one group and two items with the tabular values, got: " <> String.joinWith ", " (expected <#> \(place /\ _) -> place)
                            else case Array.mapMaybe (\(place /\ tabItems) -> tabularCameBack place tabItems) expected of
                                [] -> pure unit
                                failures -> fail $ String.joinWith "\n" failures
