module Test.ParseTree where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, it, itOnly, describe, pending')
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

-- import Report (Report)
-- import Report as Report
import Report.Class (class HasStats, class IsGroup)
import Report.GroupPath as GP
import Report.Decorators.Stats as ST

import Report.Convert.TreeCommand.Import (parseTree) as RI

newtype SampleGroup = SG (Array String)


derive newtype instance Eq SampleGroup
derive newtype instance Ord SampleGroup
derive newtype instance Show SampleGroup

instance IsGroup SampleGroup where
    g_title _ = "" -- Not used
    g_path (SG items) = GP.pathFromArray items

instance HasStats SampleGroup where
    i_stats _ = ST.SYetUnknown -- Not used



isSampleDirectory :: String -> Boolean
isSampleDirectory s = (s # String.drop (String.length s - 2) # String.take 1) /= "d"


isPursFile :: String -> Boolean
isPursFile s = (s # String.drop (String.length s - 5)) == ".purs"


treeSourceA = """/Some/Directory/
└── d1
    ├── d1-i1
    ├── d1-i2
    ├── d1-i3
    └── d1-i4
""" :: String


treeSourceB = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
└── d3
    ├── d3-d1
    │   ├── d3-d1-i1
    │   ├── d3-d1-i2
    │   ├── d3-d1-i3
    │   └── d3-d1-i4
    ├── d3-d2
    │   ├── d3-d2-i1
    │   ├── d3-d2-i2
    │   └── d3-d2-i3
    └── d3-d3
        ├── d3-d3-i1
        ├── d3-d3-i2
        ├── d3-d3-i3
        ├── d3-d3-i4
        └── d3-d3-i5
""" :: String



treeSourceC = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
├── d3
│   ├── d3-d1
│   │   ├── d3-d1-i1
│   │   ├── d3-d1-i2
│   │   ├── d3-d1-i3
│   │   └── d3-d1-i4
│   ├── d3-d2
│   │   ├── d3-d2-i1
│   │   ├── d3-d2-i2
│   │   └── d3-d2-i3
│   └── d3-d3
│       ├── d3-d3-i1
│       ├── d3-d3-i2
│       ├── d3-d3-i3
│       ├── d3-d3-i4
│       └── d3-d3-i5
└── d4
    ├── d4-i1
    ├── d4-i2
    ├── d4-d1
    │   ├── d4-d1-i1
    │   ├── d4-d1-i2
    │   └── d4-d1-i3
    └── d4-d2
        └── d4-d2-i1
""" :: String


treeSourceD = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
├── d3
│   ├── d3-d1
│   │   ├── d3-d1-i1
│   │   ├── d3-d1-i2
│   │   ├── d3-d1-i3
│   │   └── d3-d1-i4
│   ├── d3-d2
│   │   ├── d3-d2-i1
│   │   ├── d3-d2-i2
│   │   └── d3-d2-i3
│   └── d3-d3
│       ├── d3-d3-i1
│       ├── d3-d3-i2
│       ├── d3-d3-i3
│       ├── d3-d3-i4
│       └── d3-d3-i5
├── d4
│   ├── d4-i1
│   ├── d4-i2
│   ├── d4-d1
│   │   ├── d4-d1-i1
│   │   ├── d4-d1-i2
│   │   └── d4-d1-i3
│   └── d4-d2
│       └── d4-d2-i1
├── d5
│   ├── d5-i1
│   ├── d5-d1
│   │   └── d5-d1-d1
│   │       ├── d5-d1-d1-d1
│   │       │   ├── d5-d1-d1-d1-i1
│   │       │   ├── d5-d1-d1-d1-d1
│   │       │   │   ├── d5-d1-d1-d1-d1-d1
│   │       │   │   │   ├── d5-d1-d1-d1-d1-d1-i1
│   │       │   │   │   └── d5-d1-d1-d1-d1-d1-i2
│   │       │   │   ├── d5-d1-d1-d1-d1-d2
│   │       │   │   │   ├── d5-d1-d1-d1-d1-d2-i1
│   │       │   │   │   └── d5-d1-d1-d1-d1-d2-i2
│   │       │   │   ├── d5-d1-d1-d1-d1-i1
│   │       │   │   └── d5-d1-d1-d1-d1-i2
│   │       │   └── d5-d1-d1-d1-i2
│   │       └── d5-d1-d1-i1
│   ├── d5-i2
│   ├── d5-i3
│   ├── d5-d2
│   │   └── d5-d2-d1
│   │       ├── d5-d2-d1-d1
│   │       │   └── d5-d2-d1-d1-i1
│   │       └── d5-d2-d1-i1
│   ├── d5-d3
│   │   └── d5-d3-d1
│   │       ├── d5-d3-d1-i1
│   │       ├── d5-d3-d1-d1
│   │       │   ├── d5-d3-d1-d1-i1
│   │       │   └── d5-d3-d1-d1-i2
│   │       └── d5-d3-d1-i2
│   └── d5-i4
└── i1
""" :: String


treeSourceE = """/Users/shamansir/.config/nvim
├── init.lua
├── lazy-lock.json
├── lazyvim.json
└── lua
    ├── current-theme.lua
    ├── lspconfig.lua
    └── shamansir
        ├── core
        │   ├── init.lua
        │   ├── keymaps.lua
        │   └── options.lua
        ├── lazy.lua
        └── plugins
            ├── _oil.lua
            ├── _wilder.lua
            ├── colorscheme.lua
            ├── lualine.lua
            ├── mason.lua
            ├── mini.lua
            ├── neo-tree.lua
            ├── neominimap.lua
            ├── nui.lua
            ├── nvim-cmp.lua
            ├── orgmode.lua
            ├── purescript.lua
            ├── showkeys.lua
            ├── snacks.lua
            ├── strudel.lua
            ├── telescope.lua
            ├── todo-comments.lua
            ├── treesitter.lua
            ├── trouble.lua
            ├── undotree.lua
            ├── whichkey.lua
            └── whitespace.lua
""" :: String


treeSourceF = """./src
├── Report
│   ├── Builder.purs
│   ├── Chain.purs
│   ├── Class.purs
│   ├── Convert
│   │   ├── Dhall
│   │   │   ├── Export.purs
│   │   │   └── Import.purs
│   │   ├── Dhall.purs
│   │   ├── Generic.purs
│   │   ├── Json.purs
│   │   ├── Keyed.purs
│   │   ├── Org
│   │   │   ├── Export.purs
│   │   │   └── Import.purs
│   │   ├── Org.purs
│   │   ├── Text
│   │   │   ├── Modifiers
│   │   │   │   ├── Priority.purs
│   │   │   │   ├── Progress.purs
│   │   │   │   ├── Rating.purs
│   │   │   │   ├── Stats.purs
│   │   │   │   ├── Tags.purs
│   │   │   │   └── Task.purs
│   │   │   ├── Prefix.purs
│   │   │   └── Suffix.purs
│   │   └── Types.purs
│   ├── Core
│   │   └── Logic.purs
│   ├── Core.purs
│   ├── Group.purs
│   ├── GroupPath.purs
│   ├── Impl
│   │   ├── Group.purs
│   │   ├── Item.purs
│   │   ├── Subject.purs
│   │   └── Tag.purs
│   ├── Modifiers
│   │   ├── Class
│   │   │   └── ValueModify.purs
│   │   ├── Priority.purs
│   │   ├── Progress.purs
│   │   ├── Q
│   │   │   ├── Prefix.purs
│   │   │   ├── Suffix.purs
│   │   │   └── Tabular.purs
│   │   ├── Rating.purs
│   │   ├── Stats
│   │   │   └── Collect.purs
│   │   ├── Stats.purs
│   │   ├── Tabular
│   │   │   ├── ExtField.purs
│   │   │   └── TabularValue.purs
│   │   ├── Tags.purs
│   │   └── Task.purs
│   ├── Modifiers.purs
│   ├── Modify.purs
│   ├── Prefix.purs
│   ├── Suffix.purs
│   ├── Tabular.purs
│   ├── Types.purs
│   └── Web
│       ├── Component.purs
│       ├── GroupPath.purs
│       ├── Helpers.purs
│       ├── Modifiers
│       │   ├── Progress.purs
│       │   ├── Stats.purs
│       │   ├── Tags
│       │   │   └── Colors.purs
│       │   ├── Tags.purs
│       │   └── Task.purs
│       ├── Modifiers.purs
│       ├── Navigation.purs
│       ├── Prefix.purs
│       ├── Suffix.purs
│       └── Tabular.purs
├── Report.purs
└── Utils
    ├── Data
    │   ├── Array
    │   │   └── Extra.purs
    │   └── Map
    │       └── Extra.purs
    └── Report
        ├── Grouping.purs
        ├── Pages.purs
        └── Pagination.purs""" :: String


sampleResultA :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultA =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        ]
    ]


sampleResultB :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultB =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3" ] /\ [ ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        ]
    ]



sampleResultC :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultC =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3" ] /\ [ ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        , SG [ "d4" ] /\ [ "d4-i1", "d4-i2" ]
        , SG [ "d4", "d4-d1" ] /\ [ "d4-d1-i1", "d4-d1-i2", "d4-d1-i3" ]
        , SG [ "d4", "d4-d2" ] /\ [ "d4-d2-i1" ]
        ]
    ]



sampleResultD :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultD =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ "i1" ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3" ] /\ [ ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        , SG [ "d4" ] /\ [ "d4-i1", "d4-i2" ]
        , SG [ "d4", "d4-d1" ] /\ [ "d4-d1-i1", "d4-d1-i2", "d4-d1-i3" ]
        , SG [ "d4", "d4-d2" ] /\ [ "d4-d2-i1" ]
        , SG [ "d5" ] /\ [ "d5-i1", "d5-i2", "d5-i3", "d5-i4" ]
        , SG [ "d5", "d5-d1" ] /\ [ ]
        , SG [ "d5", "d5-d1", "d5-d1-d1" ] /\ [ "d5-d1-d1-i1" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-i1", "d5-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-d1-i1", "d5-d1-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1", "d5-d1-d1-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-d1-d1-i1", "d5-d1-d1-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1", "d5-d1-d1-d1-d1-d2" ] /\ [ "d5-d1-d1-d1-d1-d2-i1", "d5-d1-d1-d1-d1-d2-i2" ]
        , SG [ "d5", "d5-d2" ] /\ [ ]
        , SG [ "d5", "d5-d2", "d5-d2-d1" ] /\ [ "d5-d2-d1-i1" ]
        , SG [ "d5", "d5-d2", "d5-d2-d1", "d5-d2-d1-d1" ] /\ [ "d5-d2-d1-d1-i1" ]
        , SG [ "d5", "d5-d3" ] /\ [ ]
        , SG [ "d5", "d5-d3", "d5-d3-d1" ] /\ [ "d5-d3-d1-i1", "d5-d3-d1-i2" ]
        , SG [ "d5", "d5-d3", "d5-d3-d1", "d5-d3-d1-d1" ] /\ [ "d5-d3-d1-d1-i1", "d5-d3-d1-d1-i2" ]
        ]
    ]


sampleResultE :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultE =
    [  "/Users/shamansir/.config/nvim" /\
        [ SG [ "." ] /\ [ "init.lua", "lazy-lock.json", "lazyvim.json", "lua" ]
        , SG [ "lua" ] /\ [ "current-theme.lua", "lspconfig.lua", "shamansir" ]
        , SG [ "lua", "shamansir" ] /\ [ "core", "lazy.lua", "plugins" ]
        , SG [ "lua", "shamansir", "core" ] /\ [ "init.lua", "keymaps.lua", "options.lua" ]
        , SG [ "lua", "shamansir", "plugins" ] /\ [ "_oil.lua", "_wilder.lua", "colorscheme.lua", "lualine.lua", "mason.lua", "mini.lua", "neo-tree.lua", "neominimap.lua", "nui.lua", "nvim-cmp.lua", "orgmode.lua", "purescript.lua", "showkeys.lua", "snacks.lua", "strudel.lua", "telescope.lua", "todo-comments.lua", "treesitter.lua", "trouble.lua", "undotree.lua", "whichkey.lua", "whitespace.lua" ]
        ]
    ]


sampleResultF :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultF =
    [  "./src" /\
        [ SG [ "." ] /\ [ "Report", "Report.purs", "Utils" ]
        , SG [ "Report" ] /\ [ "Builder.purs", "Chain.purs", "Class.purs", "Convert", "Core", "Core.purs", "Group.purs", "GroupPath.purs", "Impl", "Modifiers", "Modifiers.purs", "Modify.purs", "Prefix.purs", "Suffix.purs", "Tabular.purs", "Types.purs", "Web" ]
        , SG [ "Report", "Convert" ] /\ [ "Dhall", "Dhall.purs", "Generic.purs", "Json.purs", "Keyed.purs", "Org", "Org.purs", "Text", "Types.purs" ]
        , SG [ "Report", "Convert", "Dhall" ] /\ [ "Export.purs", "Import.purs" ]
        , SG [ "Report", "Convert", "Org" ] /\ [ "Export.purs", "Import.purs" ]
        , SG [ "Report", "Convert", "Text" ] /\ [ "Modifiers", "Prefix.purs", "Suffix.purs" ]
        , SG [ "Report", "Convert", "Text", "Modifiers" ] /\ [ "Priority.purs", "Progress.purs", "Rating.purs", "Stats.purs", "Tags.purs", "Task.purs" ]
        , SG [ "Report", "Core" ] /\ [ "Logic.purs" ]
        , SG [ "Report", "Impl" ] /\ [ "Group.purs", "Item.purs", "Subject.purs", "Tag.purs" ]
        , SG [ "Report", "Modifiers" ] /\ [ "Class", "Priority.purs", "Progress.purs", "Q", "Rating.purs", "Stats", "Stats.purs", "Tabular", "Tags.purs", "Task.purs" ]
        , SG [ "Report", "Modifiers", "Class" ] /\ [ "ValueModify.purs" ]
        , SG [ "Report", "Modifiers", "Q" ] /\ [ "Prefix.purs", "Suffix.purs", "Tabular.purs" ]
        , SG [ "Report", "Modifiers", "Stats" ] /\ [ "Collect.purs" ]
        , SG [ "Report", "Modifiers", "Tabular" ] /\ [ "ExtField.purs", "TabularValue.purs" ]
        , SG [ "Report", "Web" ] /\ [ "Component.purs", "GroupPath.purs", "Helpers.purs", "Modifiers", "Modifiers.purs", "Navigation.purs", "Prefix.purs", "Suffix.purs", "Tabular.purs" ]
        , SG [ "Report", "Web", "Modifiers" ] /\ [ "Progress.purs", "Stats.purs", "Tags", "Tags.purs", "Task.purs" ]
        , SG [ "Report", "Web", "Modifiers", "Tags" ] /\ [ "Colors.purs" ]
        , SG [ "Utils" ] /\ [ "Data", "Report" ]
        , SG [ "Utils", "Data" ] /\ [ "Array", "Map" ]
        , SG [ "Utils", "Data", "Array" ] /\ [ "Extra.purs" ]
        , SG [ "Utils", "Data", "Map" ] /\ [ "Extra.purs" ]
        , SG [ "Utils", "Report" ] /\ [ "Grouping.purs", "Pages.purs", "Pagination.purs" ]
        ]
    ]


sampleResultF' :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultF' =
    [  "./src" /\
        [ SG [ "." ] /\ [ "Report.purs" ]
        , SG [ "Report" ] /\ [ "Builder.purs", "Chain.purs", "Class.purs", "Core.purs", "Group.purs", "GroupPath.purs", "Modifiers.purs", "Modify.purs", "Prefix.purs", "Suffix.purs", "Tabular.purs", "Types.purs" ]
        , SG [ "Report", "Convert" ] /\ [ "Dhall.purs", "Generic.purs", "Json.purs", "Keyed.purs", "Org.purs", "Types.purs" ]
        , SG [ "Report", "Convert", "Dhall" ] /\ [ "Export.purs", "Import.purs" ]
        , SG [ "Report", "Convert", "Org" ] /\ [ "Export.purs", "Import.purs" ]
        , SG [ "Report", "Convert", "Text" ] /\ [ "Prefix.purs", "Suffix.purs" ]
        , SG [ "Report", "Convert", "Text", "Modifiers" ] /\ [ "Priority.purs", "Progress.purs", "Rating.purs", "Stats.purs", "Tags.purs", "Task.purs" ]
        , SG [ "Report", "Core" ] /\ [ "Logic.purs" ]
        , SG [ "Report", "Impl" ] /\ [ "Group.purs", "Item.purs", "Subject.purs", "Tag.purs" ]
        , SG [ "Report", "Modifiers" ] /\ [ "Priority.purs", "Progress.purs", "Rating.purs", "Stats.purs", "Tags.purs", "Task.purs" ]
        , SG [ "Report", "Modifiers", "Class" ] /\ [ "ValueModify.purs" ]
        , SG [ "Report", "Modifiers", "Q" ] /\ [ "Prefix.purs", "Suffix.purs", "Tabular.purs" ]
        , SG [ "Report", "Modifiers", "Stats" ] /\ [ "Collect.purs" ]
        , SG [ "Report", "Modifiers", "Tabular" ] /\ [ "ExtField.purs", "TabularValue.purs" ]
        , SG [ "Report", "Web" ] /\ [ "Component.purs", "GroupPath.purs", "Helpers.purs", "Modifiers.purs", "Navigation.purs", "Prefix.purs", "Suffix.purs", "Tabular.purs" ]
        , SG [ "Report", "Web", "Modifiers" ] /\ [ "Progress.purs", "Stats.purs", "Tags.purs", "Task.purs" ]
        , SG [ "Report", "Web", "Modifiers", "Tags" ] /\ [ "Colors.purs" ]
        , SG [ "Utils" ] /\ [ ]
        , SG [ "Utils", "Data" ] /\ [ ]
        , SG [ "Utils", "Data", "Array" ] /\ [ "Extra.purs" ]
        , SG [ "Utils", "Data", "Map" ] /\ [ "Extra.purs" ]
        , SG [ "Utils", "Report" ] /\ [ "Grouping.purs", "Pages.purs", "Pagination.purs" ]
        ]
    ]


parseAndFilter :: (String -> Boolean) -> String -> Array (String /\ Array (SampleGroup /\ Array String))
parseAndFilter filterF = RI.parseTree SG >>> map (map $ map $ Array.filter filterF) >>> pure


justParse :: String -> Array (String /\ Array (SampleGroup /\ Array String))
justParse = RI.parseTree SG >>> pure


spec :: Spec Unit
spec = do
  describe "parsing tree" $ do
    it "parses tree properly: A" $
        parseAndFilter isSampleDirectory treeSourceA `shouldEqual` sampleResultA
    it "parses tree properly: B" $
        parseAndFilter isSampleDirectory treeSourceB `shouldEqual` sampleResultB
    it "parses tree properly: C" $
       parseAndFilter isSampleDirectory treeSourceC `shouldEqual` sampleResultC
    it "parses tree properly: D" $
       parseAndFilter isSampleDirectory treeSourceD `shouldEqual` sampleResultD
    it "parses tree properly: E" $
       justParse treeSourceE `shouldEqual` sampleResultE
    it "parses tree properly: F" $
       justParse treeSourceF `shouldEqual` sampleResultF
    it "parses tree properly: F'" $
       parseAndFilter isPursFile treeSourceF `shouldEqual` sampleResultF'
