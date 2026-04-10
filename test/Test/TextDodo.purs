module Test.TextDodo where

import Prelude

import Effect.Class (liftEffect)

import Yoga.JSON (readJSON) as JSON
import Foreign (ForeignError, renderForeignError) as F

import Data.Either (Either, either)
import Data.List.NonEmpty as NEL
import Data.String as String

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (fail)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Utils (shouldEqual) as U

import GameLog.Dhall (FromDhall, dhallToAchievements) as GL
import GameLog.Types.Game (GameId, GameTag) as GL
import GameLog.Types.ManyGamesStats (GamesReport, fromArray, RawAchievements) as GL
import GameLog.Types.Achievement (Tag) as GL

import Report (toReport)
import Report.Convert.Generic (includeAll) as R
import Report.Convert.Text (toText) as D


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


spec :: Spec Unit
spec =
    describe "convert" do
      it "text (plain)" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportDhall = D.toText @GL.RawAchievements @GL.GameId @GL.GameTag @GL.Tag R.includeAll glReport
                reportDhall `U.shouldEqual` expectedDhallWithGroupStats
            )
            eDhallGameCollection

        -- either
        --     (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
        --     (\achs -> "isAwesome" `U.shouldEqual` "isAwesome")
        --     reportDhall


expectedDhallNoGroupStats = """# Astral Chain
    - TrackedAt: 12-Aug-2025

    0. File 00-file
        Time : 07:54:00
        Duration : 36:48:56
        Date : 11-Jan-2001
        Name : File 11
        Chapter : Reckoning
        Rank : Riotsu
        Grade : Expert
        Play Style : Pt Standard

    1. Stats 01-stats
        Order Completion : 67/185

        2. Hero 01-stats::00-hero
            Health : 1200--1200
            Resonance : 1.0%
            ATK : 50
            Money : 628G
            Gene Codes : 514
            Duty Evaluation : 41832
            Order Completion : 0.36%

        3. Weapons 01-stats::01-weapons
            X-Baton Level : 6
            Baton Atk : 130
            Blaster Atk : 58
            Gladius Atk : 282
            Limiter Refill : 1.1%
            Command Skills Unlocked : 5
            Legatus Level : 4
            AED Recovery : 1.0%
            AED Speed : 7sec
            Limiter Max : 130
            Additional AED Batteries : 1

        4. Basic 01-stats::02-basic
            All Pure Platinum : 0/1
            Red Cases Closed
            Blue Cases Closed
            Sword Scholar : 0/1
            Sword Master : 0/1
            Arrow Scholar : 1/1
            Arrow Master : 0/1
            Arm Scholar : 1/1
            Arm Master : 0/1
            Beast Scholar : 1/1
            Beast Master : 0/1
            Axe Scholar : 0/1
            Axe Master : 0/1
            Legionis Dominus : 0/0
            X-Baton Master : 0/0
            Legatus Master : 0/0

            5. Chapters 01-stats::02-basic::00-chapters
                Startup : 1/1
                Awake : 1/1
                Link : 1/1
                Siedge : 1/1
                Accord : 1/1
                Complicit : 1/1
                Wild : 1/1
                Peace : 1/1
                Salvation : 1/1
                Madness : 1/1
                Reckoning : 1/1

            6. S+ 01-stats::02-basic::01-s-plus
                Startup : 0/1
                Awake : 0/1
                Link : 0/1
                Siedge : 0/1
                Accord : 0/1
                Complicit : 0/1
                Wild : 0/1
                Peace : 0/1
                Salvation : 0/1
                Madness : 0/1
                Reckoning : 0/1

            7. Rank 01-stats::02-basic::03-rank
                Silver : 0/0
                Gold : 0/0
                Platinum : 0/0
                Pure Platinum : 0/0

        8. Combat 01-stats::03-combat
            Chimeradicator
            Itemanical
            Round Sword Master : 5/30
            Triple Sword Master : 0/30
            Slow Shot Master : 7/30
            Air Shot Master : 30/30
            Gravity Storm Master : 0/30
            Round Bullet Master : 0/30
            Howl Master : 7/30
            Chain Drive Master : 7/30
            Crash Bomb Master : 0/30
            Blue Shield Master : 0/30
            Hit Rush Master : 9/30
            Auto-Bind Master : 4/30
            Speed Start Master : 0/30
            Sync Keep Master : 0/30
            Power Charge Master : 30/30
            Sync Attack Master : 30/30
            Chain Jump Attack Master : 30/30
            Chain Counter Attack Master : 18/30
            Finishing Move Master : 30/30
            Fusion Master : 2/10
            Stealth Bind Master : 27/30
            Simply Stunning : 30/30
            Baton Maniac : 50000/50000
            Blaster Maniac : 40354/50000
            Gladius Maniac : 50000/50000
            Sharpshooter : 10/10
            Absolute K-9 Unit : 3/10
            Armed & Dangerous : 5/10
            Parry Professional : 0/10
            Ted's Best Customer : 0/5

        9. Collection 01-stats::04-collection
            Supply Snatcher
            Finders Keepers
            Red Matter Reducer : 1000/1000
            ???
            ???
            Know Your Enemy
            People Watcher : 1/20
            It's Who You Know : 1/40
            Ask Tourist : 2/28
            Blueshifter
            Retirement Fund
            Gene Code Glutton
            Hard Worker : 100000/100000
            Model Officer : 465057/500000
            Long Arm of the Law : 465057/2000000
            Ark Beautification Society : 10/10
            ???
            ???
            Fashionista : 13/39

            10. Red Matter Remover 01-stats::04-collection::00-red-matter
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 1/1
                File 04 : 1/1
                File 05 : 0/1
                File 06 : 1/1
                File 07 : 1/1
                File 08 : 0/1
                File 09 : 1/1
                File 10 : 1/1
                File 11 : 0/1

            11. Slime Splatter 01-stats::04-collection::01-slime
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 0/1
                File 04 : 0/1
                File 05 : 0/1
                File 06 : 0/1
                File 07 : 0/1
                File 08 : 0/1
                File 09 : 0/1
                File 10 : 0/1
                File 11 : 0/1

            12. Feline Friend 01-stats::04-collection::02-feline
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 0/1
                File 04 : 1/1
                File 05 : 0/1
                File 06 : 1/1
                File 07 : 1/1
                File 08 : 1/1
                File 09 : 0/1
                File 10 : 0/1
                File 11 : 0/1

            13. Nature Calls 01-stats::04-collection::03-nature
                [ ] HQ
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 1/1
                File 04 : 1/1
                File 05 : 1/1
                File 06 : 0/1
                File 07 : 1/1
                File 08 : 0/1
                File 09 : 1/1
                File 10 : 1/1
                File 11 : 1/1

        14. Unique 01-stats::05-unique
            A Who's Who of Hermits : 0/0
            Lappy's Helium Hullaballoo : 0/0
            Sweet Release : 0/0
            Summer Avalanche : 0/0
            [X] Serious Student
            Doggy Door : 0/0
            Amateur Photographer : 1/1
            Precious Memories : 24/36
            ???
            ???
            ???
            Astral Perfection : 0/0

            15. Photo 01-stats::05-unique::06-photo
                I See You! : 0/0
                Daddy Dearest : 0/0
                Major Malfunction : 0/0
                Get Me Down! : 0/0
                Get Well Soon : 0/0
                Uppers Delight : 0/0
                Do NOT show Marie! : 0/0
                Hello, Partner! : 0/0
                Best Friends! : 0/0
                Packing Heat! : 0/0
                Comfort and Justice! : 0/0
""" :: String


expectedDhallWithGroupStats = """# Astral Chain 50/166
    - TrackedAt: 12-Aug-2025

    0. File 00-file
        Time : 07:54:00
        Duration : 36:48:56
        Date : 11-Jan-2001
        Name : File 11
        Chapter : Reckoning
        Rank : Riotsu
        Grade : Expert
        Play Style : Pt Standard

    1. Stats 0/1 01-stats
        Order Completion : 67/185

        2. Hero 1/2 01-stats::00-hero
            Health : 1200--1200
            Resonance : 1.0%
            ATK : 50
            Money : 628G
            Gene Codes : 514
            Duty Evaluation : 41832
            Order Completion : 0.36%

        3. Weapons 2/2 01-stats::01-weapons
            X-Baton Level : 6
            Baton Atk : 130
            Blaster Atk : 58
            Gladius Atk : 282
            Limiter Refill : 1.1%
            Command Skills Unlocked : 5
            Legatus Level : 4
            AED Recovery : 1.0%
            AED Speed : 7sec
            Limiter Max : 130
            Additional AED Batteries : 1

        4. Basic 3/16 01-stats::02-basic
            All Pure Platinum : 0/1
            Red Cases Closed
            Blue Cases Closed
            Sword Scholar : 0/1
            Sword Master : 0/1
            Arrow Scholar : 1/1
            Arrow Master : 0/1
            Arm Scholar : 1/1
            Arm Master : 0/1
            Beast Scholar : 1/1
            Beast Master : 0/1
            Axe Scholar : 0/1
            Axe Master : 0/1
            Legionis Dominus : 0/0
            X-Baton Master : 0/0
            Legatus Master : 0/0

            5. Chapters 11/11 01-stats::02-basic::00-chapters
                Startup : 1/1
                Awake : 1/1
                Link : 1/1
                Siedge : 1/1
                Accord : 1/1
                Complicit : 1/1
                Wild : 1/1
                Peace : 1/1
                Salvation : 1/1
                Madness : 1/1
                Reckoning : 1/1

            6. S+ 0/11 01-stats::02-basic::01-s-plus
                Startup : 0/1
                Awake : 0/1
                Link : 0/1
                Siedge : 0/1
                Accord : 0/1
                Complicit : 0/1
                Wild : 0/1
                Peace : 0/1
                Salvation : 0/1
                Madness : 0/1
                Reckoning : 0/1

            7. Rank 0/4 01-stats::02-basic::03-rank
                Silver : 0/0
                Gold : 0/0
                Platinum : 0/0
                Pure Platinum : 0/0

        8. Combat 10/32 01-stats::03-combat
            Chimeradicator
            Itemanical
            Round Sword Master : 5/30
            Triple Sword Master : 0/30
            Slow Shot Master : 7/30
            Air Shot Master : 30/30
            Gravity Storm Master : 0/30
            Round Bullet Master : 0/30
            Howl Master : 7/30
            Chain Drive Master : 7/30
            Crash Bomb Master : 0/30
            Blue Shield Master : 0/30
            Hit Rush Master : 9/30
            Auto-Bind Master : 4/30
            Speed Start Master : 0/30
            Sync Keep Master : 0/30
            Power Charge Master : 30/30
            Sync Attack Master : 30/30
            Chain Jump Attack Master : 30/30
            Chain Counter Attack Master : 18/30
            Finishing Move Master : 30/30
            Fusion Master : 2/10
            Stealth Bind Master : 27/30
            Simply Stunning : 30/30
            Baton Maniac : 50000/50000
            Blaster Maniac : 40354/50000
            Gladius Maniac : 50000/50000
            Sharpshooter : 10/10
            Absolute K-9 Unit : 3/10
            Armed & Dangerous : 5/10
            Parry Professional : 0/10
            Ted's Best Customer : 0/5

        9. Collection 4/19 01-stats::04-collection
            Supply Snatcher
            Finders Keepers
            Red Matter Reducer : 1000/1000
            ???
            ???
            Know Your Enemy
            People Watcher : 1/20
            It's Who You Know : 1/40
            Ask Tourist : 2/28
            Blueshifter
            Retirement Fund
            Gene Code Glutton
            Hard Worker : 100000/100000
            Model Officer : 465057/500000
            Long Arm of the Law : 465057/2000000
            Ark Beautification Society : 10/10
            ???
            ???
            Fashionista : 13/39

            10. Red Matter Remover 6/11 01-stats::04-collection::00-red-matter
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 1/1
                File 04 : 1/1
                File 05 : 0/1
                File 06 : 1/1
                File 07 : 1/1
                File 08 : 0/1
                File 09 : 1/1
                File 10 : 1/1
                File 11 : 0/1

            11. Slime Splatter 0/11 01-stats::04-collection::01-slime
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 0/1
                File 04 : 0/1
                File 05 : 0/1
                File 06 : 0/1
                File 07 : 0/1
                File 08 : 0/1
                File 09 : 0/1
                File 10 : 0/1
                File 11 : 0/1

            12. Feline Friend 4/11 01-stats::04-collection::02-feline
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 0/1
                File 04 : 1/1
                File 05 : 0/1
                File 06 : 1/1
                File 07 : 1/1
                File 08 : 1/1
                File 09 : 0/1
                File 10 : 0/1
                File 11 : 0/1

            13. Nature Calls 7/12 01-stats::04-collection::03-nature
                [ ] HQ
                File 01 : 0/1
                File 02 : 0/1
                File 03 : 1/1
                File 04 : 1/1
                File 05 : 1/1
                File 06 : 0/1
                File 07 : 1/1
                File 08 : 0/1
                File 09 : 1/1
                File 10 : 1/1
                File 11 : 1/1

        14. Unique 2/12 01-stats::05-unique
            A Who's Who of Hermits : 0/0
            Lappy's Helium Hullaballoo : 0/0
            Sweet Release : 0/0
            Summer Avalanche : 0/0
            [X] Serious Student
            Doggy Door : 0/0
            Amateur Photographer : 1/1
            Precious Memories : 24/36
            ???
            ???
            ???
            Astral Perfection : 0/0

            15. Photo 0/11 01-stats::05-unique::06-photo
                I See You! : 0/0
                Daddy Dearest : 0/0
                Major Malfunction : 0/0
                Get Me Down! : 0/0
                Get Well Soon : 0/0
                Uppers Delight : 0/0
                Do NOT show Marie! : 0/0
                Hello, Partner! : 0/0
                Best Friends! : 0/0
                Packing Heat! : 0/0
                Comfort and Justice! : 0/0
""" :: String