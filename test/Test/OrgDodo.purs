module Test.OrgDodo where

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


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


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
:Name: Time
:ONTIME: 07:54:00
:END:
*** Duration : 36:48:56
:PROPERTIES:
:Name: Duration
:ONTIME: 36:48:56
:END:
*** Date : <2001-01-11>
:PROPERTIES:
:Name: Date
:ONDATE: <2001-01-11>
:END:
*** Name : File 11
:PROPERTIES:
:Name: Name
:TEXTVAL: File 11
:END:
*** Chapter : Reckoning
:PROPERTIES:
:Name: Chapter
:TEXTVAL: Reckoning
:END:
*** Rank : Riotsu
:PROPERTIES:
:Name: Rank
:TEXTVAL: Riotsu
:END:
*** Grade : Expert
:PROPERTIES:
:Name: Grade
:TEXTVAL: Expert
:END:
*** Play Style : Pt Standard
:PROPERTIES:
:Name: Play Style
:TEXTVAL: Pt Standard
:END:
** Stats
:PROPERTIES:
:Path: 01-stats
:Index: 1
:END:
*** Order Completion : 67/185
:PROPERTIES:
:Name: Order Completion
:TOGETI: 67 185
:END:
*** Hero
:PROPERTIES:
:Path: 01-stats 00-hero
:Index: 2
:END:
**** Health : 1200--1200
:PROPERTIES:
:Name: Health
:RANGEI: 1200 1200
:END:
**** Resonance : 1.0%
:PROPERTIES:
:Name: Resonance
:PERCENTN: 1.0
:END:
**** ATK : 50
:PROPERTIES:
:Name: ATK
:INTVAL: 50
:END:
**** Money : 628G
:PROPERTIES:
:Name: Money
:MESI: 628 G
:END:
**** Gene Codes : 514
:PROPERTIES:
:Name: Gene Codes
:INTVAL: 514
:END:
**** Duty Evaluation : 41832
:PROPERTIES:
:Name: Duty Evaluation
:INTVAL: 41832
:END:
**** Order Completion : 0.36%
:PROPERTIES:
:Name: Order Completion
:PERCENTN: 0.36
:END:
*** Weapons
:PROPERTIES:
:Path: 01-stats 01-weapons
:Index: 3
:END:
**** X-Baton Level : 6
:PROPERTIES:
:Name: X-Baton Level
:INTVAL: 6
:END:
**** Baton Atk : 130
:PROPERTIES:
:Name: Baton Atk
:INTVAL: 130
:END:
**** Blaster Atk : 58
:PROPERTIES:
:Name: Blaster Atk
:INTVAL: 58
:END:
**** Gladius Atk : 282
:PROPERTIES:
:Name: Gladius Atk
:INTVAL: 282
:END:
**** Limiter Refill : 1.1%
:PROPERTIES:
:Name: Limiter Refill
:PERCENTN: 1.1
:END:
**** Command Skills Unlocked : 5
:PROPERTIES:
:Name: Command Skills Unlocked
:INTVAL: 5
:END:
**** Legatus Level : 4
:PROPERTIES:
:Name: Legatus Level
:INTVAL: 4
:END:
**** AED Recovery : 1.0%
:PROPERTIES:
:Name: AED Recovery
:PERCENTN: 1.0
:END:
**** AED Speed : 7sec
:PROPERTIES:
:Name: AED Speed
:MESI: 7 sec
:END:
**** Limiter Max : 130
:PROPERTIES:
:Name: Limiter Max
:INTVAL: 130
:END:
**** Additional AED Batteries : 1
:PROPERTIES:
:Name: Additional AED Batteries
:INTVAL: 1
:END:
*** Basic
:PROPERTIES:
:Path: 01-stats 02-basic
:Index: 4
:END:
**** All Pure Platinum : 0/1
:PROPERTIES:
:Name: All Pure Platinum
:TOGETI: 0 1
:END:
**** Red Cases Closed
**** Blue Cases Closed
**** Sword Scholar : 0/1
:PROPERTIES:
:Name: Sword Scholar
:TOGETI: 0 1
:END:
**** Sword Master : 0/1
:PROPERTIES:
:Name: Sword Master
:TOGETI: 0 1
:END:
**** Arrow Scholar : 1/1
:PROPERTIES:
:Name: Arrow Scholar
:TOGETI: 1 1
:END:
**** Arrow Master : 0/1
:PROPERTIES:
:Name: Arrow Master
:TOGETI: 0 1
:END:
**** Arm Scholar : 1/1
:PROPERTIES:
:Name: Arm Scholar
:TOGETI: 1 1
:END:
**** Arm Master : 0/1
:PROPERTIES:
:Name: Arm Master
:TOGETI: 0 1
:END:
**** Beast Scholar : 1/1
:PROPERTIES:
:Name: Beast Scholar
:TOGETI: 1 1
:END:
**** Beast Master : 0/1
:PROPERTIES:
:Name: Beast Master
:TOGETI: 0 1
:END:
**** Axe Scholar : 0/1
:PROPERTIES:
:Name: Axe Scholar
:TOGETI: 0 1
:END:
**** Axe Master : 0/1
:PROPERTIES:
:Name: Axe Master
:TOGETI: 0 1
:END:
**** Legionis Dominus : 0/0
:PROPERTIES:
:Name: Legionis Dominus
:TOGETI: 0 0
:END:
**** X-Baton Master : 0/0
:PROPERTIES:
:Name: X-Baton Master
:TOGETI: 0 0
:END:
**** Legatus Master : 0/0
:PROPERTIES:
:Name: Legatus Master
:TOGETI: 0 0
:END:
**** Chapters
:PROPERTIES:
:Path: 01-stats 02-basic 00-chapters
:Index: 5
:END:
***** Startup : 1/1
:PROPERTIES:
:Name: Startup
:TOGETI: 1 1
:END:
***** Awake : 1/1
:PROPERTIES:
:Name: Awake
:TOGETI: 1 1
:END:
***** Link : 1/1
:PROPERTIES:
:Name: Link
:TOGETI: 1 1
:END:
***** Siedge : 1/1
:PROPERTIES:
:Name: Siedge
:TOGETI: 1 1
:END:
***** Accord : 1/1
:PROPERTIES:
:Name: Accord
:TOGETI: 1 1
:END:
***** Complicit : 1/1
:PROPERTIES:
:Name: Complicit
:TOGETI: 1 1
:END:
***** Wild : 1/1
:PROPERTIES:
:Name: Wild
:TOGETI: 1 1
:END:
***** Peace : 1/1
:PROPERTIES:
:Name: Peace
:TOGETI: 1 1
:END:
***** Salvation : 1/1
:PROPERTIES:
:Name: Salvation
:TOGETI: 1 1
:END:
***** Madness : 1/1
:PROPERTIES:
:Name: Madness
:TOGETI: 1 1
:END:
***** Reckoning : 1/1
:PROPERTIES:
:Name: Reckoning
:TOGETI: 1 1
:END:
**** S+
:PROPERTIES:
:Path: 01-stats 02-basic 01-s-plus
:Index: 6
:END:
***** Startup : 0/1
:PROPERTIES:
:Name: Startup
:TOGETI: 0 1
:END:
***** Awake : 0/1
:PROPERTIES:
:Name: Awake
:TOGETI: 0 1
:END:
***** Link : 0/1
:PROPERTIES:
:Name: Link
:TOGETI: 0 1
:END:
***** Siedge : 0/1
:PROPERTIES:
:Name: Siedge
:TOGETI: 0 1
:END:
***** Accord : 0/1
:PROPERTIES:
:Name: Accord
:TOGETI: 0 1
:END:
***** Complicit : 0/1
:PROPERTIES:
:Name: Complicit
:TOGETI: 0 1
:END:
***** Wild : 0/1
:PROPERTIES:
:Name: Wild
:TOGETI: 0 1
:END:
***** Peace : 0/1
:PROPERTIES:
:Name: Peace
:TOGETI: 0 1
:END:
***** Salvation : 0/1
:PROPERTIES:
:Name: Salvation
:TOGETI: 0 1
:END:
***** Madness : 0/1
:PROPERTIES:
:Name: Madness
:TOGETI: 0 1
:END:
***** Reckoning : 0/1
:PROPERTIES:
:Name: Reckoning
:TOGETI: 0 1
:END:
**** Rank
:PROPERTIES:
:Path: 01-stats 02-basic 03-rank
:Index: 7
:END:
***** Silver : 0/0
:PROPERTIES:
:Name: Silver
:TOGETI: 0 0
:END:
***** Gold : 0/0
:PROPERTIES:
:Name: Gold
:TOGETI: 0 0
:END:
***** Platinum : 0/0
:PROPERTIES:
:Name: Platinum
:TOGETI: 0 0
:END:
***** Pure Platinum : 0/0
:PROPERTIES:
:Name: Pure Platinum
:TOGETI: 0 0
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
:Name: Round Sword Master
:TOGETI: 5 30
:END:
**** Triple Sword Master : 0/30
:PROPERTIES:
:Name: Triple Sword Master
:TOGETI: 0 30
:END:
**** Slow Shot Master : 7/30
:PROPERTIES:
:Name: Slow Shot Master
:TOGETI: 7 30
:END:
**** Air Shot Master : 30/30
:PROPERTIES:
:Name: Air Shot Master
:TOGETI: 30 30
:END:
**** Gravity Storm Master : 0/30
:PROPERTIES:
:Name: Gravity Storm Master
:TOGETI: 0 30
:END:
**** Round Bullet Master : 0/30
:PROPERTIES:
:Name: Round Bullet Master
:TOGETI: 0 30
:END:
**** Howl Master : 7/30
:PROPERTIES:
:Name: Howl Master
:TOGETI: 7 30
:END:
**** Chain Drive Master : 7/30
:PROPERTIES:
:Name: Chain Drive Master
:TOGETI: 7 30
:END:
**** Crash Bomb Master : 0/30
:PROPERTIES:
:Name: Crash Bomb Master
:TOGETI: 0 30
:END:
**** Blue Shield Master : 0/30
:PROPERTIES:
:Name: Blue Shield Master
:TOGETI: 0 30
:END:
**** Hit Rush Master : 9/30
:PROPERTIES:
:Name: Hit Rush Master
:TOGETI: 9 30
:END:
**** Auto-Bind Master : 4/30
:PROPERTIES:
:Name: Auto-Bind Master
:TOGETI: 4 30
:END:
**** Speed Start Master : 0/30
:PROPERTIES:
:Name: Speed Start Master
:TOGETI: 0 30
:END:
**** Sync Keep Master : 0/30
:PROPERTIES:
:Name: Sync Keep Master
:TOGETI: 0 30
:END:
**** Power Charge Master : 30/30
:PROPERTIES:
:Name: Power Charge Master
:TOGETI: 30 30
:END:
**** Sync Attack Master : 30/30
:PROPERTIES:
:Name: Sync Attack Master
:TOGETI: 30 30
:END:
**** Chain Jump Attack Master : 30/30
:PROPERTIES:
:Name: Chain Jump Attack Master
:TOGETI: 30 30
:END:
**** Chain Counter Attack Master : 18/30
:PROPERTIES:
:Name: Chain Counter Attack Master
:TOGETI: 18 30
:END:
**** Finishing Move Master : 30/30
:PROPERTIES:
:Name: Finishing Move Master
:TOGETI: 30 30
:END:
**** Fusion Master : 2/10
:PROPERTIES:
:Name: Fusion Master
:TOGETI: 2 10
:END:
**** Stealth Bind Master : 27/30
:PROPERTIES:
:Name: Stealth Bind Master
:TOGETI: 27 30
:END:
**** Simply Stunning : 30/30
:PROPERTIES:
:Name: Simply Stunning
:TOGETI: 30 30
:END:
**** Baton Maniac : 50000/50000
:PROPERTIES:
:Name: Baton Maniac
:TOGETI: 50000 50000
:END:
**** Blaster Maniac : 40354/50000
:PROPERTIES:
:Name: Blaster Maniac
:TOGETI: 40354 50000
:END:
**** Gladius Maniac : 50000/50000
:PROPERTIES:
:Name: Gladius Maniac
:TOGETI: 50000 50000
:END:
**** Sharpshooter : 10/10
:PROPERTIES:
:Name: Sharpshooter
:TOGETI: 10 10
:END:
**** Absolute K-9 Unit : 3/10
:PROPERTIES:
:Name: Absolute K-9 Unit
:TOGETI: 3 10
:END:
**** Armed & Dangerous : 5/10
:PROPERTIES:
:Name: Armed & Dangerous
:TOGETI: 5 10
:END:
**** Parry Professional : 0/10
:PROPERTIES:
:Name: Parry Professional
:TOGETI: 0 10
:END:
**** Ted's Best Customer : 0/5
:PROPERTIES:
:Name: Ted's Best Customer
:TOGETI: 0 5
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
:Name: Red Matter Reducer
:TOGETI: 1000 1000
:END:
**** ???
**** ???
**** Know Your Enemy
**** People Watcher : 1/20
:PROPERTIES:
:Name: People Watcher
:TOGETI: 1 20
:END:
**** It's Who You Know : 1/40
:PROPERTIES:
:Name: It's Who You Know
:TOGETI: 1 40
:END:
**** Ask Tourist : 2/28
:PROPERTIES:
:Name: Ask Tourist
:TOGETI: 2 28
:END:
**** Blueshifter
**** Retirement Fund
**** Gene Code Glutton
**** Hard Worker : 100000/100000
:PROPERTIES:
:Name: Hard Worker
:TOGETI: 100000 100000
:END:
**** Model Officer : 465057/500000
:PROPERTIES:
:Name: Model Officer
:TOGETI: 465057 500000
:END:
**** Long Arm of the Law : 465057/2000000
:PROPERTIES:
:Name: Long Arm of the Law
:TOGETI: 465057 2000000
:END:
**** Ark Beautification Society : 10/10
:PROPERTIES:
:Name: Ark Beautification Society
:TOGETI: 10 10
:END:
**** ???
**** ???
**** Fashionista : 13/39
:PROPERTIES:
:Name: Fashionista
:TOGETI: 13 39
:END:
**** Red Matter Remover
:PROPERTIES:
:Path: 01-stats 04-collection 00-red-matter
:Index: 10
:END:
***** File 01 : 0/1
:PROPERTIES:
:Name: File 01
:TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Name: File 02
:TOGETI: 0 1
:END:
***** File 03 : 1/1
:PROPERTIES:
:Name: File 03
:TOGETI: 1 1
:END:
***** File 04 : 1/1
:PROPERTIES:
:Name: File 04
:TOGETI: 1 1
:END:
***** File 05 : 0/1
:PROPERTIES:
:Name: File 05
:TOGETI: 0 1
:END:
***** File 06 : 1/1
:PROPERTIES:
:Name: File 06
:TOGETI: 1 1
:END:
***** File 07 : 1/1
:PROPERTIES:
:Name: File 07
:TOGETI: 1 1
:END:
***** File 08 : 0/1
:PROPERTIES:
:Name: File 08
:TOGETI: 0 1
:END:
***** File 09 : 1/1
:PROPERTIES:
:Name: File 09
:TOGETI: 1 1
:END:
***** File 10 : 1/1
:PROPERTIES:
:Name: File 10
:TOGETI: 1 1
:END:
***** File 11 : 0/1
:PROPERTIES:
:Name: File 11
:TOGETI: 0 1
:END:
**** Slime Splatter
:PROPERTIES:
:Path: 01-stats 04-collection 01-slime
:Index: 11
:END:
***** File 01 : 0/1
:PROPERTIES:
:Name: File 01
:TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Name: File 02
:TOGETI: 0 1
:END:
***** File 03 : 0/1
:PROPERTIES:
:Name: File 03
:TOGETI: 0 1
:END:
***** File 04 : 0/1
:PROPERTIES:
:Name: File 04
:TOGETI: 0 1
:END:
***** File 05 : 0/1
:PROPERTIES:
:Name: File 05
:TOGETI: 0 1
:END:
***** File 06 : 0/1
:PROPERTIES:
:Name: File 06
:TOGETI: 0 1
:END:
***** File 07 : 0/1
:PROPERTIES:
:Name: File 07
:TOGETI: 0 1
:END:
***** File 08 : 0/1
:PROPERTIES:
:Name: File 08
:TOGETI: 0 1
:END:
***** File 09 : 0/1
:PROPERTIES:
:Name: File 09
:TOGETI: 0 1
:END:
***** File 10 : 0/1
:PROPERTIES:
:Name: File 10
:TOGETI: 0 1
:END:
***** File 11 : 0/1
:PROPERTIES:
:Name: File 11
:TOGETI: 0 1
:END:
**** Feline Friend
:PROPERTIES:
:Path: 01-stats 04-collection 02-feline
:Index: 12
:END:
***** File 01 : 0/1
:PROPERTIES:
:Name: File 01
:TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Name: File 02
:TOGETI: 0 1
:END:
***** File 03 : 0/1
:PROPERTIES:
:Name: File 03
:TOGETI: 0 1
:END:
***** File 04 : 1/1
:PROPERTIES:
:Name: File 04
:TOGETI: 1 1
:END:
***** File 05 : 0/1
:PROPERTIES:
:Name: File 05
:TOGETI: 0 1
:END:
***** File 06 : 1/1
:PROPERTIES:
:Name: File 06
:TOGETI: 1 1
:END:
***** File 07 : 1/1
:PROPERTIES:
:Name: File 07
:TOGETI: 1 1
:END:
***** File 08 : 1/1
:PROPERTIES:
:Name: File 08
:TOGETI: 1 1
:END:
***** File 09 : 0/1
:PROPERTIES:
:Name: File 09
:TOGETI: 0 1
:END:
***** File 10 : 0/1
:PROPERTIES:
:Name: File 10
:TOGETI: 0 1
:END:
***** File 11 : 0/1
:PROPERTIES:
:Name: File 11
:TOGETI: 0 1
:END:
**** Nature Calls
:PROPERTIES:
:Path: 01-stats 04-collection 03-nature
:Index: 13
:END:
***** [ ] HQ
:PROPERTIES:
:Name: HQ
:TOCOMPLETE: false
:END:
***** File 01 : 0/1
:PROPERTIES:
:Name: File 01
:TOGETI: 0 1
:END:
***** File 02 : 0/1
:PROPERTIES:
:Name: File 02
:TOGETI: 0 1
:END:
***** File 03 : 1/1
:PROPERTIES:
:Name: File 03
:TOGETI: 1 1
:END:
***** File 04 : 1/1
:PROPERTIES:
:Name: File 04
:TOGETI: 1 1
:END:
***** File 05 : 1/1
:PROPERTIES:
:Name: File 05
:TOGETI: 1 1
:END:
***** File 06 : 0/1
:PROPERTIES:
:Name: File 06
:TOGETI: 0 1
:END:
***** File 07 : 1/1
:PROPERTIES:
:Name: File 07
:TOGETI: 1 1
:END:
***** File 08 : 0/1
:PROPERTIES:
:Name: File 08
:TOGETI: 0 1
:END:
***** File 09 : 1/1
:PROPERTIES:
:Name: File 09
:TOGETI: 1 1
:END:
***** File 10 : 1/1
:PROPERTIES:
:Name: File 10
:TOGETI: 1 1
:END:
***** File 11 : 1/1
:PROPERTIES:
:Name: File 11
:TOGETI: 1 1
:END:
*** Unique
:PROPERTIES:
:Path: 01-stats 05-unique
:Index: 14
:END:
**** A Who's Who of Hermits : 0/0
:PROPERTIES:
:Name: A Who's Who of Hermits
:TOGETI: 0 0
:END:
**** Lappy's Helium Hullaballoo : 0/0
:PROPERTIES:
:Name: Lappy's Helium Hullaballoo
:TOGETI: 0 0
:END:
**** Sweet Release : 0/0
:PROPERTIES:
:Name: Sweet Release
:TOGETI: 0 0
:END:
**** Summer Avalanche : 0/0
:PROPERTIES:
:Name: Summer Avalanche
:TOGETI: 0 0
:END:
**** [X] Serious Student
:PROPERTIES:
:Name: Serious Student
:TOCOMPLETE: true
:END:
**** Doggy Door : 0/0
:PROPERTIES:
:Name: Doggy Door
:TOGETI: 0 0
:END:
**** Amateur Photographer : 1/1
:PROPERTIES:
:Name: Amateur Photographer
:TOGETI: 1 1
:END:
**** Precious Memories : 24/36
:PROPERTIES:
:Name: Precious Memories
:TOGETI: 24 36
:END:
**** ???
**** ???
**** ???
**** Astral Perfection : 0/0
:PROPERTIES:
:Name: Astral Perfection
:TOGETI: 0 0
:END:
**** Photo
:PROPERTIES:
:Path: 01-stats 05-unique 06-photo
:Index: 15
:END:
***** I See You! : 0/0
:PROPERTIES:
:Name: I See You!
:TOGETI: 0 0
:END:
***** Daddy Dearest : 0/0
:PROPERTIES:
:Name: Daddy Dearest
:TOGETI: 0 0
:END:
***** Major Malfunction : 0/0
:PROPERTIES:
:Name: Major Malfunction
:TOGETI: 0 0
:END:
***** Get Me Down! : 0/0
:PROPERTIES:
:Name: Get Me Down!
:TOGETI: 0 0
:END:
***** Get Well Soon : 0/0
:PROPERTIES:
:Name: Get Well Soon
:TOGETI: 0 0
:END:
***** Uppers Delight : 0/0
:PROPERTIES:
:Name: Uppers Delight
:TOGETI: 0 0
:END:
***** Do NOT show Marie! : 0/0
:PROPERTIES:
:Name: Do NOT show Marie!
:TOGETI: 0 0
:END:
***** Hello, Partner! : 0/0
:PROPERTIES:
:Name: Hello, Partner!
:TOGETI: 0 0
:END:
***** Best Friends! : 0/0
:PROPERTIES:
:Name: Best Friends!
:TOGETI: 0 0
:END:
***** Packing Heat! : 0/0
:PROPERTIES:
:Name: Packing Heat!
:TOGETI: 0 0
:END:
***** Comfort and Justice! : 0/0
:PROPERTIES:
:Name: Comfort and Justice!
:TOGETI: 0 0
:END:""" :: String