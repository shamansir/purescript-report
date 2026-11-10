module Test.Export.OrgDodo where

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
import Report.Convert.Org (toOrg) as D


onlyFewGamesFilePath = "test/games-samples/only-few-games.fromDhall.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


spec :: Spec Unit
spec =
    describe "convert" do
      it "org" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportDhall = D.toOrg @GL.RawAchievements @GL.GameId @GL.GameTag @GL.Tag R.includeAll glReport
                reportDhall `U.shouldEqual` expectedDhall
            )
            eDhallGameCollection

        -- either
        --     (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
        --     (\achs -> "isAwesome" `U.shouldEqual` "isAwesome")
        --     reportDhall


expectedDhall = """* Astral Chain
:PROPERTIES:
:Id: DHL:astral-chain
:Platform: TODO
:Playtime: TODO
:TrackedAt: <2025-08-12>
:END:
** File
:PROPERTIES:
:Path: 00-file
:Index: 0
:END:
*** Time : 07:54:00
:PROPERTIES:
:Title: Time
:RAV_ONTIME: 07:54:00
:END:
*** Duration : 36:48:56
:PROPERTIES:
:Title: Duration
:RAV_ONTIME: 36:48:56
:END:
*** Date : <2001-01-11>
:PROPERTIES:
:Title: Date
:RAV_ONDATE: <2001-01-11>
:END:
*** Name : File 11
:PROPERTIES:
:Title: Name
:RAV_TEXTVAL: File 11
:END:
*** Chapter : Reckoning
:PROPERTIES:
:Title: Chapter
:RAV_TEXTVAL: Reckoning
:END:
*** Rank : Riotsu
:PROPERTIES:
:Title: Rank
:RAV_TEXTVAL: Riotsu
:END:
*** Grade : Expert
:PROPERTIES:
:Title: Grade
:RAV_TEXTVAL: Expert
:END:
*** Play Style : Pt Standard
:PROPERTIES:
:Title: Play Style
:RAV_TEXTVAL: Pt Standard
:END:
** Stats
:PROPERTIES:
:Path: 01-stats
:Index: 1
:END:
*** Order Completion : 67/185
:PROPERTIES:
:Title: Order Completion
:RAV_TOGETI: 67 185
:END:
*** Hero
:PROPERTIES:
:Path: 01-stats 00-hero
:Index: 2
:END:
**** Health : 1200--1200
:PROPERTIES:
:Title: Health
:RAV_RANGEI: 1200 1200
:END:
**** Resonance : 1.0%
:PROPERTIES:
:Title: Resonance
:RAV_PERCENTN: 1.0
:END:
**** ATK : 50
:PROPERTIES:
:Title: ATK
:RAV_INTVAL: 50
:END:
**** Money : 628G
:PROPERTIES:
:Title: Money
:RAV_MESI: 628 G
:END:
**** Gene Codes : 514
:PROPERTIES:
:Title: Gene Codes
:RAV_INTVAL: 514
:END:
**** Duty Evaluation : 41832
:PROPERTIES:
:Title: Duty Evaluation
:RAV_INTVAL: 41832
:END:
**** Order Completion : 0.36%
:PROPERTIES:
:Title: Order Completion
:RAV_PERCENTN: 0.36
:END:
*** Weapons
:PROPERTIES:
:Path: 01-stats 01-weapons
:Index: 3
:END:
**** X-Baton Level : 6
:PROPERTIES:
:Title: X-Baton Level
:RAV_INTVAL: 6
:END:
**** Baton Atk : 130
:PROPERTIES:
:Title: Baton Atk
:RAV_INTVAL: 130
:END:
**** Blaster Atk : 58
:PROPERTIES:
:Title: Blaster Atk
:RAV_INTVAL: 58
:END:
**** Gladius Atk : 282
:PROPERTIES:
:Title: Gladius Atk
:RAV_INTVAL: 282
:END:
**** Limiter Refill : 1.1%
:PROPERTIES:
:Title: Limiter Refill
:RAV_PERCENTN: 1.1
:END:
**** Command Skills Unlocked : 5
:PROPERTIES:
:Title: Command Skills Unlocked
:RAV_INTVAL: 5
:END:
**** Legatus Level : 4
:PROPERTIES:
:Title: Legatus Level
:RAV_INTVAL: 4
:END:
**** AED Recovery : 1.0%
:PROPERTIES:
:Title: AED Recovery
:RAV_PERCENTN: 1.0
:END:
**** AED Speed : 7sec
:PROPERTIES:
:Title: AED Speed
:RAV_MESI: 7 sec
:END:
**** Limiter Max : 130
:PROPERTIES:
:Title: Limiter Max
:RAV_INTVAL: 130
:END:
**** Additional AED Batteries : 1
:PROPERTIES:
:Title: Additional AED Batteries
:RAV_INTVAL: 1
:END:
*** Basic
:PROPERTIES:
:Path: 01-stats 02-basic
:Index: 4
:END:
**** All Pure Platinum : 0/1
:PROPERTIES:
:Title: All Pure Platinum
:RAV_TOGETI: 0 1
:END:
**** Red Cases Closed
**** Blue Cases Closed
**** Sword Scholar : 0/1
:PROPERTIES:
:Title: Sword Scholar
:RAV_TOGETI: 0 1
:END:
**** Sword Master : 0/1
:PROPERTIES:
:Title: Sword Master
:RAV_TOGETI: 0 1
:END:
**** Arrow Scholar : 1/1
:PROPERTIES:
:Title: Arrow Scholar
:RAV_TOGETI: 1 1
:END:
**** Arrow Master : 0/1
:PROPERTIES:
:Title: Arrow Master
:RAV_TOGETI: 0 1
:END:
**** Arm Scholar : 1/1
:PROPERTIES:
:Title: Arm Scholar
:RAV_TOGETI: 1 1
:END:
**** Arm Master : 0/1
:PROPERTIES:
:Title: Arm Master
:RAV_TOGETI: 0 1
:END:
**** Beast Scholar : 1/1
:PROPERTIES:
:Title: Beast Scholar
:RAV_TOGETI: 1 1
:END:
**** Beast Master : 0/1
:PROPERTIES:
:Title: Beast Master
:RAV_TOGETI: 0 1
:END:
**** Axe Scholar : 0/1
:PROPERTIES:
:Title: Axe Scholar
:RAV_TOGETI: 0 1
:END:
**** Axe Master : 0/1
:PROPERTIES:
:Title: Axe Master
:RAV_TOGETI: 0 1
:END:
**** Legionis Dominus : 0/0
:PROPERTIES:
:Title: Legionis Dominus
:RAV_TOGETI: 0 0
:END:
**** X-Baton Master : 0/0
:PROPERTIES:
:Title: X-Baton Master
:RAV_TOGETI: 0 0
:END:
**** Legatus Master : 0/0
:PROPERTIES:
:Title: Legatus Master
:RAV_TOGETI: 0 0
:END:
**** Chapters
:PROPERTIES:
:Path: 01-stats 02-basic 00-chapters
:Index: 5
:END:
***** Startup : 1/1
:PROPERTIES:
:Title: Startup
:RAV_TOGETI: 1 1
:END:
***** Awake : 1/1
:PROPERTIES:
:Title: Awake
:RAV_TOGETI: 1 1
:END:
***** Link : 1/1
:PROPERTIES:
:Title: Link
:RAV_TOGETI: 1 1
:END:
***** Siedge : 1/1
:PROPERTIES:
:Title: Siedge
:RAV_TOGETI: 1 1
:END:
***** Accord : 1/1
:PROPERTIES:
:Title: Accord
:RAV_TOGETI: 1 1
:END:
***** Complicit : 1/1
:PROPERTIES:
:Title: Complicit
:RAV_TOGETI: 1 1
:END:
***** Wild : 1/1
:PROPERTIES:
:Title: Wild
:RAV_TOGETI: 1 1
:END:
***** Peace : 1/1
:PROPERTIES:
:Title: Peace
:RAV_TOGETI: 1 1
:END:
***** Salvation : 1/1
:PROPERTIES:
:Title: Salvation
:RAV_TOGETI: 1 1
:END:
***** Madness : 1/1
:PROPERTIES:
:Title: Madness
:RAV_TOGETI: 1 1
:END:
***** Reckoning : 1/1
:PROPERTIES:
:Title: Reckoning
:RAV_TOGETI: 1 1
:END:
**** S+
:PROPERTIES:
:Path: 01-stats 02-basic 01-s-plus
:Index: 6
:END:
***** Startup : 0/1
:PROPERTIES:
:Title: Startup
:RAV_TOGETI: 0 1
:END:
***** Awake : 0/1
:PROPERTIES:
:Title: Awake
:RAV_TOGETI: 0 1
:END:
***** Link : 0/1
:PROPERTIES:
:Title: Link
:RAV_TOGETI: 0 1
:END:
***** Siedge : 0/1
:PROPERTIES:
:Title: Siedge
:RAV_TOGETI: 0 1
:END:
***** Accord : 0/1
:PROPERTIES:
:Title: Accord
:RAV_TOGETI: 0 1
:END:
***** Complicit : 0/1
:PROPERTIES:
:Title: Complicit
:RAV_TOGETI: 0 1
:END:
***** Wild : 0/1
:PROPERTIES:
:Title: Wild
:RAV_TOGETI: 0 1
:END:
***** Peace : 0/1
:PROPERTIES:
:Title: Peace
:RAV_TOGETI: 0 1
:END:
***** Salvation : 0/1
:PROPERTIES:
:Title: Salvation
:RAV_TOGETI: 0 1
:END:
***** Madness : 0/1
:PROPERTIES:
:Title: Madness
:RAV_TOGETI: 0 1
:END:
***** Reckoning : 0/1
:PROPERTIES:
:Title: Reckoning
:RAV_TOGETI: 0 1
:END:
**** Rank
:PROPERTIES:
:Path: 01-stats 02-basic 03-rank
:Index: 7
:END:
***** Silver : 0/0
:PROPERTIES:
:Title: Silver
:RAV_TOGETI: 0 0
:END:
***** Gold : 0/0
:PROPERTIES:
:Title: Gold
:RAV_TOGETI: 0 0
:END:
***** Platinum : 0/0
:PROPERTIES:
:Title: Platinum
:RAV_TOGETI: 0 0
:END:
***** Pure Platinum : 0/0
:PROPERTIES:
:Title: Pure Platinum
:RAV_TOGETI: 0 0
:END:
*** Combat
:PROPERTIES:
:Path: 01-stats 03-combat
:Index: 8
:END:
**** Chimeradicator
**** Itemanical
**** Round Sword Master : 5/30
:PROPERTIES:
:Title: Round Sword Master
:RAV_TOGETI: 5 30
:END:
**** Triple Sword Master : 0/30
:PROPERTIES:
:Title: Triple Sword Master
:RAV_TOGETI: 0 30
:END:
**** Slow Shot Master : 7/30
:PROPERTIES:
:Title: Slow Shot Master
:RAV_TOGETI: 7 30
:END:
**** Air Shot Master : 30/30
:PROPERTIES:
:Title: Air Shot Master
:RAV_TOGETI: 30 30
:END:
**** Gravity Storm Master : 0/30
:PROPERTIES:
:Title: Gravity Storm Master
:RAV_TOGETI: 0 30
:END:
**** Round Bullet Master : 0/30
:PROPERTIES:
:Title: Round Bullet Master
:RAV_TOGETI: 0 30
:END:
**** Howl Master : 7/30
:PROPERTIES:
:Title: Howl Master
:RAV_TOGETI: 7 30
:END:
**** Chain Drive Master : 7/30
:PROPERTIES:
:Title: Chain Drive Master
:RAV_TOGETI: 7 30
:END:
**** Crash Bomb Master : 0/30
:PROPERTIES:
:Title: Crash Bomb Master
:RAV_TOGETI: 0 30
:END:
**** Blue Shield Master : 0/30
:PROPERTIES:
:Title: Blue Shield Master
:RAV_TOGETI: 0 30
:END:
**** Hit Rush Master : 9/30
:PROPERTIES:
:Title: Hit Rush Master
:RAV_TOGETI: 9 30
:END:
**** Auto-Bind Master : 4/30
:PROPERTIES:
:Title: Auto-Bind Master
:RAV_TOGETI: 4 30
:END:
**** Speed Start Master : 0/30
:PROPERTIES:
:Title: Speed Start Master
:RAV_TOGETI: 0 30
:END:
**** Sync Keep Master : 0/30
:PROPERTIES:
:Title: Sync Keep Master
:RAV_TOGETI: 0 30
:END:
**** Power Charge Master : 30/30
:PROPERTIES:
:Title: Power Charge Master
:RAV_TOGETI: 30 30
:END:
**** Sync Attack Master : 30/30
:PROPERTIES:
:Title: Sync Attack Master
:RAV_TOGETI: 30 30
:END:
**** Chain Jump Attack Master : 30/30
:PROPERTIES:
:Title: Chain Jump Attack Master
:RAV_TOGETI: 30 30
:END:
**** Chain Counter Attack Master : 18/30
:PROPERTIES:
:Title: Chain Counter Attack Master
:RAV_TOGETI: 18 30
:END:
**** Finishing Move Master : 30/30
:PROPERTIES:
:Title: Finishing Move Master
:RAV_TOGETI: 30 30
:END:
**** Fusion Master : 2/10
:PROPERTIES:
:Title: Fusion Master
:RAV_TOGETI: 2 10
:END:
**** Stealth Bind Master : 27/30
:PROPERTIES:
:Title: Stealth Bind Master
:RAV_TOGETI: 27 30
:END:
**** Simply Stunning : 30/30
:PROPERTIES:
:Title: Simply Stunning
:RAV_TOGETI: 30 30
:END:
**** Baton Maniac : 50000/50000
:PROPERTIES:
:Title: Baton Maniac
:RAV_TOGETI: 50000 50000
:END:
**** Blaster Maniac : 40354/50000
:PROPERTIES:
:Title: Blaster Maniac
:RAV_TOGETI: 40354 50000
:END:
**** Gladius Maniac : 50000/50000
:PROPERTIES:
:Title: Gladius Maniac
:RAV_TOGETI: 50000 50000
:END:
**** Sharpshooter : 10/10
:PROPERTIES:
:Title: Sharpshooter
:RAV_TOGETI: 10 10
:END:
**** Absolute K-9 Unit : 3/10
:PROPERTIES:
:Title: Absolute K-9 Unit
:RAV_TOGETI: 3 10
:END:
**** Armed & Dangerous : 5/10
:PROPERTIES:
:Title: Armed & Dangerous
:RAV_TOGETI: 5 10
:END:
**** Parry Professional : 0/10
:PROPERTIES:
:Title: Parry Professional
:RAV_TOGETI: 0 10
:END:
**** Ted's Best Customer : 0/5
:PROPERTIES:
:Title: Ted's Best Customer
:RAV_TOGETI: 0 5
:END:
*** Collection
:PROPERTIES:
:Path: 01-stats 04-collection
:Index: 9
:END:
**** Supply Snatcher
**** Finders Keepers
**** Red Matter Reducer : 1000/1000
:PROPERTIES:
:Title: Red Matter Reducer
:RAV_TOGETI: 1000 1000
:END:
**** ???
**** ???
**** Know Your Enemy
**** People Watcher : 1/20
:PROPERTIES:
:Title: People Watcher
:RAV_TOGETI: 1 20
:END:
**** It's Who You Know : 1/40
:PROPERTIES:
:Title: It's Who You Know
:RAV_TOGETI: 1 40
:END:
**** Ask Tourist : 2/28
:PROPERTIES:
:Title: Ask Tourist
:RAV_TOGETI: 2 28
:END:
**** Blueshifter
**** Retirement Fund
**** Gene Code Glutton
**** Hard Worker : 100000/100000
:PROPERTIES:
:Title: Hard Worker
:RAV_TOGETI: 100000 100000
:END:
**** Model Officer : 465057/500000
:PROPERTIES:
:Title: Model Officer
:RAV_TOGETI: 465057 500000
:END:
**** Long Arm of the Law : 465057/2000000
:PROPERTIES:
:Title: Long Arm of the Law
:RAV_TOGETI: 465057 2000000
:END:
**** Ark Beautification Society : 10/10
:PROPERTIES:
:Title: Ark Beautification Society
:RAV_TOGETI: 10 10
:END:
**** ???
**** ???
**** Fashionista : 13/39
:PROPERTIES:
:Title: Fashionista
:RAV_TOGETI: 13 39
:END:
**** Red Matter Remover
:PROPERTIES:
:Path: 01-stats 04-collection 00-red-matter
:Index: 10
:END:
***** File 01 : 0/1
:PROPERTIES:
:Title: File 01
:RAV_TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Title: File 02
:RAV_TOGETI: 0 1
:END:
***** File 03 : 1/1
:PROPERTIES:
:Title: File 03
:RAV_TOGETI: 1 1
:END:
***** File 04 : 1/1
:PROPERTIES:
:Title: File 04
:RAV_TOGETI: 1 1
:END:
***** File 05 : 0/1
:PROPERTIES:
:Title: File 05
:RAV_TOGETI: 0 1
:END:
***** File 06 : 1/1
:PROPERTIES:
:Title: File 06
:RAV_TOGETI: 1 1
:END:
***** File 07 : 1/1
:PROPERTIES:
:Title: File 07
:RAV_TOGETI: 1 1
:END:
***** File 08 : 0/1
:PROPERTIES:
:Title: File 08
:RAV_TOGETI: 0 1
:END:
***** File 09 : 1/1
:PROPERTIES:
:Title: File 09
:RAV_TOGETI: 1 1
:END:
***** File 10 : 1/1
:PROPERTIES:
:Title: File 10
:RAV_TOGETI: 1 1
:END:
***** File 11 : 0/1
:PROPERTIES:
:Title: File 11
:RAV_TOGETI: 0 1
:END:
**** Slime Splatter
:PROPERTIES:
:Path: 01-stats 04-collection 01-slime
:Index: 11
:END:
***** File 01 : 0/1
:PROPERTIES:
:Title: File 01
:RAV_TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Title: File 02
:RAV_TOGETI: 0 1
:END:
***** File 03 : 0/1
:PROPERTIES:
:Title: File 03
:RAV_TOGETI: 0 1
:END:
***** File 04 : 0/1
:PROPERTIES:
:Title: File 04
:RAV_TOGETI: 0 1
:END:
***** File 05 : 0/1
:PROPERTIES:
:Title: File 05
:RAV_TOGETI: 0 1
:END:
***** File 06 : 0/1
:PROPERTIES:
:Title: File 06
:RAV_TOGETI: 0 1
:END:
***** File 07 : 0/1
:PROPERTIES:
:Title: File 07
:RAV_TOGETI: 0 1
:END:
***** File 08 : 0/1
:PROPERTIES:
:Title: File 08
:RAV_TOGETI: 0 1
:END:
***** File 09 : 0/1
:PROPERTIES:
:Title: File 09
:RAV_TOGETI: 0 1
:END:
***** File 10 : 0/1
:PROPERTIES:
:Title: File 10
:RAV_TOGETI: 0 1
:END:
***** File 11 : 0/1
:PROPERTIES:
:Title: File 11
:RAV_TOGETI: 0 1
:END:
**** Feline Friend
:PROPERTIES:
:Path: 01-stats 04-collection 02-feline
:Index: 12
:END:
***** File 01 : 0/1
:PROPERTIES:
:Title: File 01
:RAV_TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Title: File 02
:RAV_TOGETI: 0 1
:END:
***** File 03 : 0/1
:PROPERTIES:
:Title: File 03
:RAV_TOGETI: 0 1
:END:
***** File 04 : 1/1
:PROPERTIES:
:Title: File 04
:RAV_TOGETI: 1 1
:END:
***** File 05 : 0/1
:PROPERTIES:
:Title: File 05
:RAV_TOGETI: 0 1
:END:
***** File 06 : 1/1
:PROPERTIES:
:Title: File 06
:RAV_TOGETI: 1 1
:END:
***** File 07 : 1/1
:PROPERTIES:
:Title: File 07
:RAV_TOGETI: 1 1
:END:
***** File 08 : 1/1
:PROPERTIES:
:Title: File 08
:RAV_TOGETI: 1 1
:END:
***** File 09 : 0/1
:PROPERTIES:
:Title: File 09
:RAV_TOGETI: 0 1
:END:
***** File 10 : 0/1
:PROPERTIES:
:Title: File 10
:RAV_TOGETI: 0 1
:END:
***** File 11 : 0/1
:PROPERTIES:
:Title: File 11
:RAV_TOGETI: 0 1
:END:
**** Nature Calls
:PROPERTIES:
:Path: 01-stats 04-collection 03-nature
:Index: 13
:END:
***** [ ] HQ
:PROPERTIES:
:Title: HQ
:RAV_TOCOMPLETE: false
:END:
***** File 01 : 0/1
:PROPERTIES:
:Title: File 01
:RAV_TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Title: File 02
:RAV_TOGETI: 0 1
:END:
***** File 03 : 1/1
:PROPERTIES:
:Title: File 03
:RAV_TOGETI: 1 1
:END:
***** File 04 : 1/1
:PROPERTIES:
:Title: File 04
:RAV_TOGETI: 1 1
:END:
***** File 05 : 1/1
:PROPERTIES:
:Title: File 05
:RAV_TOGETI: 1 1
:END:
***** File 06 : 0/1
:PROPERTIES:
:Title: File 06
:RAV_TOGETI: 0 1
:END:
***** File 07 : 1/1
:PROPERTIES:
:Title: File 07
:RAV_TOGETI: 1 1
:END:
***** File 08 : 0/1
:PROPERTIES:
:Title: File 08
:RAV_TOGETI: 0 1
:END:
***** File 09 : 1/1
:PROPERTIES:
:Title: File 09
:RAV_TOGETI: 1 1
:END:
***** File 10 : 1/1
:PROPERTIES:
:Title: File 10
:RAV_TOGETI: 1 1
:END:
***** File 11 : 1/1
:PROPERTIES:
:Title: File 11
:RAV_TOGETI: 1 1
:END:
*** Unique
:PROPERTIES:
:Path: 01-stats 05-unique
:Index: 14
:END:
**** A Who's Who of Hermits : 0/0
:PROPERTIES:
:Title: A Who's Who of Hermits
:RAV_TOGETI: 0 0
:END:
**** Lappy's Helium Hullaballoo : 0/0
:PROPERTIES:
:Title: Lappy's Helium Hullaballoo
:RAV_TOGETI: 0 0
:END:
**** Sweet Release : 0/0
:PROPERTIES:
:Title: Sweet Release
:RAV_TOGETI: 0 0
:END:
**** Summer Avalanche : 0/0
:PROPERTIES:
:Title: Summer Avalanche
:RAV_TOGETI: 0 0
:END:
**** [X] Serious Student
:PROPERTIES:
:Title: Serious Student
:RAV_TOCOMPLETE: true
:END:
**** Doggy Door : 0/0
:PROPERTIES:
:Title: Doggy Door
:RAV_TOGETI: 0 0
:END:
**** Amateur Photographer : 1/1
:PROPERTIES:
:Title: Amateur Photographer
:RAV_TOGETI: 1 1
:END:
**** Precious Memories : 24/36
:PROPERTIES:
:Title: Precious Memories
:RAV_TOGETI: 24 36
:END:
**** ???
**** ???
**** ???
**** Astral Perfection : 0/0
:PROPERTIES:
:Title: Astral Perfection
:RAV_TOGETI: 0 0
:END:
**** Photo
:PROPERTIES:
:Path: 01-stats 05-unique 06-photo
:Index: 15
:END:
***** I See You! : 0/0
:PROPERTIES:
:Title: I See You!
:RAV_TOGETI: 0 0
:END:
***** Daddy Dearest : 0/0
:PROPERTIES:
:Title: Daddy Dearest
:RAV_TOGETI: 0 0
:END:
***** Major Malfunction : 0/0
:PROPERTIES:
:Title: Major Malfunction
:RAV_TOGETI: 0 0
:END:
***** Get Me Down! : 0/0
:PROPERTIES:
:Title: Get Me Down!
:RAV_TOGETI: 0 0
:END:
***** Get Well Soon : 0/0
:PROPERTIES:
:Title: Get Well Soon
:RAV_TOGETI: 0 0
:END:
***** Uppers Delight : 0/0
:PROPERTIES:
:Title: Uppers Delight
:RAV_TOGETI: 0 0
:END:
***** Do NOT show Marie! : 0/0
:PROPERTIES:
:Title: Do NOT show Marie!
:RAV_TOGETI: 0 0
:END:
***** Hello, Partner! : 0/0
:PROPERTIES:
:Title: Hello, Partner!
:RAV_TOGETI: 0 0
:END:
***** Best Friends! : 0/0
:PROPERTIES:
:Title: Best Friends!
:RAV_TOGETI: 0 0
:END:
***** Packing Heat! : 0/0
:PROPERTIES:
:Title: Packing Heat!
:RAV_TOGETI: 0 0
:END:
***** Comfort and Justice! : 0/0
:PROPERTIES:
:Title: Comfort and Justice!
:RAV_TOGETI: 0 0
:END:""" :: String