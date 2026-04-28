module Test.Export.RepDodo where

import Prelude

import Effect.Class (liftEffect)

import Yoga.JSON (readJSON) as JSON
import Foreign (ForeignError, renderForeignError) as F

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.String as String

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (fail, shouldEqual) as A

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import StringParser (printParserError) as SP

import Test.Utils (shouldEqual) as U

import GameLog.Dhall (FromDhall, dhallToAchievements) as GL
import GameLog.Types.Game (GameId, GameTag) as GL
import GameLog.Types.ManyGamesStats (GamesReport, fromArray, RawAchievements) as GL
import GameLog.Types.Achievement (Tag) as GL

import Report (toReport)
import Report.Convert.Generic (includeAll) as R
import Report.Convert.Rep (toRep) as D
import Report.Convert.Rep.Import (fromRep) as I


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


spec :: Spec Unit
spec =
    describe "convert" do
      it "to `rep` (plain)" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> A.fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportRep = D.toRep @GL.RawAchievements @GL.GameId @GL.GameTag @GL.Tag R.includeAll glReport
                reportRep `U.shouldEqual` expectedRep
            )
            eDhallGameCollection

      it "from `rep` (plain)" do
        case I.fromRep expectedRep of
          Left err ->
            A.fail $ "Parse failed: " <> SP.printParserError err
          Right subjects -> do
            -- one subject
            Array.length subjects `A.shouldEqual` 1
            case Array.head subjects of
              Nothing -> A.fail "No subjects parsed"
              Just subj -> do
                -- subject name and tabulars
                subj.name `A.shouldEqual` "Astral Chain"
                Array.length subj.tabulars `A.shouldEqual` 4
                (_.name     <$> Array.head subj.tabulars) `A.shouldEqual` Just "Id"
                (_.rawValue <$> Array.head subj.tabulars) `A.shouldEqual` Just "DHL:astral-chain"
                (_.name     <$> Array.index subj.tabulars 3) `A.shouldEqual` Just "TrackedAt"
                (_.rawValue <$> Array.index subj.tabulars 3) `A.shouldEqual` Just "<2025-08-12>"
                -- 16 groups in flat list (subjects + nested all at same level)
                Array.length subj.groups `A.shouldEqual` 16
                -- first group: File
                case Array.head subj.groups of
                  Nothing -> A.fail "No groups parsed"
                  Just g0 -> do
                    g0.title  `A.shouldEqual` "File"
                    g0.depth  `A.shouldEqual` 1
                    g0.pathId `A.shouldEqual` Just "00-file"
                    Array.length g0.tabulars `A.shouldEqual` 2
                    (_.name     <$> Array.head g0.tabulars) `A.shouldEqual` Just "Path"
                    (_.rawValue <$> Array.head g0.tabulars) `A.shouldEqual` Just "00-file"
                    -- 8 items in File (no subgroups)
                    Array.length g0.items `A.shouldEqual` 8
                    case Array.head g0.items of
                      Nothing -> A.fail "No items in File group"
                      Just item0 -> do
                        item0.title `A.shouldEqual` "Time"
                        (_.marker   <$> Array.head item0.decorators) `A.shouldEqual` Just "TIM"
                        (_.rawValue <$> Array.head item0.decorators) `A.shouldEqual` Just "07:54:00"
                    case Array.last g0.items of
                      Nothing -> A.fail "No last item in File group"
                      Just item7 -> do
                        item7.title `A.shouldEqual` "Play Style"
                        (_.rawValue <$> Array.head item7.decorators) `A.shouldEqual` Just "Pt Standard"
                -- Stats group: 1 item only (Hero/Weapons/… are separate groups in flat list)
                case Array.index subj.groups 1 of
                  Nothing -> A.fail "No Stats group"
                  Just g1 -> do
                    g1.title `A.shouldEqual` "Stats"
                    g1.depth `A.shouldEqual` 1
                    Array.length g1.items `A.shouldEqual` 1
                    case Array.head g1.items of
                      Nothing -> A.fail "No items in Stats group"
                      Just item0 -> do
                        item0.title `A.shouldEqual` "Order Completion"
                        (_.marker   <$> Array.head item0.decorators) `A.shouldEqual` Just "GTI"
                        (_.rawValue <$> Array.head item0.decorators) `A.shouldEqual` Just "67 185"
                -- deepest groups at depth 3 carry correct depth field
                case Array.index subj.groups 5 of
                  Nothing -> A.fail "No Chapters group"
                  Just g5 -> do
                    g5.title `A.shouldEqual` "Chapters"
                    g5.depth `A.shouldEqual` 3
                    Array.length g5.items `A.shouldEqual` 11


expectedRep = """SBJ. Astral Chain
- Id
; TXT. DHL:astral-chain
- Platform
; TXT. TODO
- Playtime
; TXT. TODO
- TrackedAt
; DAT. <2025-08-12>
    GRP. File // 00-file
    - Path
    ; TXT. 00-file
    - Index
    ; INT. 0
        Time
        : TIM. 07:54:00
        Duration
        : TIM. 36:48:56
        Date
        : DAT. <2001-01-11>
        Name
        : TXT. File 11
        Chapter
        : TXT. Reckoning
        Rank
        : TXT. Riotsu
        Grade
        : TXT. Expert
        Play Style
        : TXT. Pt Standard
    GRP. Stats // 01-stats
    - Path
    ; TXT. 01-stats
    - Index
    ; INT. 1
        Order Completion
        : GTI. 67 185
        GRP. Hero // 00-hero
        - Path
        ; TXT. 01-stats 00-hero
        - Index
        ; INT. 2
            Health
            : RGI. 1200 1200
            Resonance
            : PCN. 1.0
            ATK
            : INT. 50
            Money
            : MSI. 628 G
            Gene Codes
            : INT. 514
            Duty Evaluation
            : INT. 41832
            Order Completion
            : PCN. 0.36
        GRP. Weapons // 01-weapons
        - Path
        ; TXT. 01-stats 01-weapons
        - Index
        ; INT. 3
            X-Baton Level
            : INT. 6
            Baton Atk
            : INT. 130
            Blaster Atk
            : INT. 58
            Gladius Atk
            : INT. 282
            Limiter Refill
            : PCN. 1.1
            Command Skills Unlocked
            : INT. 5
            Legatus Level
            : INT. 4
            AED Recovery
            : PCN. 1.0
            AED Speed
            : MSI. 7 sec
            Limiter Max
            : INT. 130
            Additional AED Batteries
            : INT. 1
        GRP. Basic // 02-basic
        - Path
        ; TXT. 01-stats 02-basic
        - Index
        ; INT. 4
            All Pure Platinum
            : GTI. 0 1
            Red Cases Closed
            : LVI. .
            Blue Cases Closed
            : LVI. .
            Sword Scholar
            : GTI. 0 1
            Sword Master
            : GTI. 0 1
            Arrow Scholar
            : GTI. 1 1
            Arrow Master
            : GTI. 0 1
            Arm Scholar
            : GTI. 1 1
            Arm Master
            : GTI. 0 1
            Beast Scholar
            : GTI. 1 1
            Beast Master
            : GTI. 0 1
            Axe Scholar
            : GTI. 0 1
            Axe Master
            : GTI. 0 1
            Legionis Dominus
            : GTI. 0 0
            X-Baton Master
            : GTI. 0 0
            Legatus Master
            : GTI. 0 0
            GRP. Chapters // 00-chapters
            - Path
            ; TXT. 01-stats 02-basic 00-chapters
            - Index
            ; INT. 5
                Startup
                : GTI. 1 1
                Awake
                : GTI. 1 1
                Link
                : GTI. 1 1
                Siedge
                : GTI. 1 1
                Accord
                : GTI. 1 1
                Complicit
                : GTI. 1 1
                Wild
                : GTI. 1 1
                Peace
                : GTI. 1 1
                Salvation
                : GTI. 1 1
                Madness
                : GTI. 1 1
                Reckoning
                : GTI. 1 1
            GRP. S+ // 01-s-plus
            - Path
            ; TXT. 01-stats 02-basic 01-s-plus
            - Index
            ; INT. 6
                Startup
                : GTI. 0 1
                Awake
                : GTI. 0 1
                Link
                : GTI. 0 1
                Siedge
                : GTI. 0 1
                Accord
                : GTI. 0 1
                Complicit
                : GTI. 0 1
                Wild
                : GTI. 0 1
                Peace
                : GTI. 0 1
                Salvation
                : GTI. 0 1
                Madness
                : GTI. 0 1
                Reckoning
                : GTI. 0 1
            GRP. Rank // 03-rank
            - Path
            ; TXT. 01-stats 02-basic 03-rank
            - Index
            ; INT. 7
                Silver
                : GTI. 0 0
                Gold
                : GTI. 0 0
                Platinum
                : GTI. 0 0
                Pure Platinum
                : GTI. 0 0
        GRP. Combat // 03-combat
        - Path
        ; TXT. 01-stats 03-combat
        - Index
        ; INT. 8
            Chimeradicator
            : LVI. .
            Itemanical
            : LVI. .
            Round Sword Master
            : GTI. 5 30
            Triple Sword Master
            : GTI. 0 30
            Slow Shot Master
            : GTI. 7 30
            Air Shot Master
            : GTI. 30 30
            Gravity Storm Master
            : GTI. 0 30
            Round Bullet Master
            : GTI. 0 30
            Howl Master
            : GTI. 7 30
            Chain Drive Master
            : GTI. 7 30
            Crash Bomb Master
            : GTI. 0 30
            Blue Shield Master
            : GTI. 0 30
            Hit Rush Master
            : GTI. 9 30
            Auto-Bind Master
            : GTI. 4 30
            Speed Start Master
            : GTI. 0 30
            Sync Keep Master
            : GTI. 0 30
            Power Charge Master
            : GTI. 30 30
            Sync Attack Master
            : GTI. 30 30
            Chain Jump Attack Master
            : GTI. 30 30
            Chain Counter Attack Master
            : GTI. 18 30
            Finishing Move Master
            : GTI. 30 30
            Fusion Master
            : GTI. 2 10
            Stealth Bind Master
            : GTI. 27 30
            Simply Stunning
            : GTI. 30 30
            Baton Maniac
            : GTI. 50000 50000
            Blaster Maniac
            : GTI. 40354 50000
            Gladius Maniac
            : GTI. 50000 50000
            Sharpshooter
            : GTI. 10 10
            Absolute K-9 Unit
            : GTI. 3 10
            Armed & Dangerous
            : GTI. 5 10
            Parry Professional
            : GTI. 0 10
            Ted's Best Customer
            : GTI. 0 5
        GRP. Collection // 04-collection
        - Path
        ; TXT. 01-stats 04-collection
        - Index
        ; INT. 9
            Supply Snatcher
            : LVI. .
            Finders Keepers
            : LVI. .
            Red Matter Reducer
            : GTI. 1000 1000
            ???
            : UNK. .
            ???
            : UNK. .
            Know Your Enemy
            : LVI. .
            People Watcher
            : GTI. 1 20
            It's Who You Know
            : GTI. 1 40
            Ask Tourist
            : GTI. 2 28
            Blueshifter
            : LVI. .
            Retirement Fund
            : LVI. .
            Gene Code Glutton
            : LVI. .
            Hard Worker
            : GTI. 100000 100000
            Model Officer
            : GTI. 465057 500000
            Long Arm of the Law
            : GTI. 465057 2000000
            Ark Beautification Society
            : GTI. 10 10
            ???
            : UNK. .
            ???
            : UNK. .
            Fashionista
            : GTI. 13 39
            GRP. Red Matter Remover // 00-red-matter
            - Path
            ; TXT. 01-stats 04-collection 00-red-matter
            - Index
            ; INT. 10
                File 01
                : GTI. 0 1
                File 02
                : GTI. 0 1
                File 03
                : GTI. 1 1
                File 04
                : GTI. 1 1
                File 05
                : GTI. 0 1
                File 06
                : GTI. 1 1
                File 07
                : GTI. 1 1
                File 08
                : GTI. 0 1
                File 09
                : GTI. 1 1
                File 10
                : GTI. 1 1
                File 11
                : GTI. 0 1
            GRP. Slime Splatter // 01-slime
            - Path
            ; TXT. 01-stats 04-collection 01-slime
            - Index
            ; INT. 11
                File 01
                : GTI. 0 1
                File 02
                : GTI. 0 1
                File 03
                : GTI. 0 1
                File 04
                : GTI. 0 1
                File 05
                : GTI. 0 1
                File 06
                : GTI. 0 1
                File 07
                : GTI. 0 1
                File 08
                : GTI. 0 1
                File 09
                : GTI. 0 1
                File 10
                : GTI. 0 1
                File 11
                : GTI. 0 1
            GRP. Feline Friend // 02-feline
            - Path
            ; TXT. 01-stats 04-collection 02-feline
            - Index
            ; INT. 12
                File 01
                : GTI. 0 1
                File 02
                : GTI. 0 1
                File 03
                : GTI. 0 1
                File 04
                : GTI. 1 1
                File 05
                : GTI. 0 1
                File 06
                : GTI. 1 1
                File 07
                : GTI. 1 1
                File 08
                : GTI. 1 1
                File 09
                : GTI. 0 1
                File 10
                : GTI. 0 1
                File 11
                : GTI. 0 1
            GRP. Nature Calls // 03-nature
            - Path
            ; TXT. 01-stats 04-collection 03-nature
            - Index
            ; INT. 13
                HQ
                : CMP. TODO
                File 01
                : GTI. 0 1
                File 02
                : GTI. 0 1
                File 03
                : GTI. 1 1
                File 04
                : GTI. 1 1
                File 05
                : GTI. 1 1
                File 06
                : GTI. 0 1
                File 07
                : GTI. 1 1
                File 08
                : GTI. 0 1
                File 09
                : GTI. 1 1
                File 10
                : GTI. 1 1
                File 11
                : GTI. 1 1
        GRP. Unique // 05-unique
        - Path
        ; TXT. 01-stats 05-unique
        - Index
        ; INT. 14
            A Who's Who of Hermits
            : GTI. 0 0
            Lappy's Helium Hullaballoo
            : GTI. 0 0
            Sweet Release
            : GTI. 0 0
            Summer Avalanche
            : GTI. 0 0
            Serious Student
            : CMP. DONE
            Doggy Door
            : GTI. 0 0
            Amateur Photographer
            : GTI. 1 1
            Precious Memories
            : GTI. 24 36
            ???
            : UNK. .
            ???
            : UNK. .
            ???
            : UNK. .
            Astral Perfection
            : GTI. 0 0
            GRP. Photo // 06-photo
            - Path
            ; TXT. 01-stats 05-unique 06-photo
            - Index
            ; INT. 15
                I See You!
                : GTI. 0 0
                Daddy Dearest
                : GTI. 0 0
                Major Malfunction
                : GTI. 0 0
                Get Me Down!
                : GTI. 0 0
                Get Well Soon
                : GTI. 0 0
                Uppers Delight
                : GTI. 0 0
                Do NOT show Marie!
                : GTI. 0 0
                Hello, Partner!
                : GTI. 0 0
                Best Friends!
                : GTI. 0 0
                Packing Heat!
                : GTI. 0 0
                Comfort and Justice!
                : GTI. 0 0""" :: String