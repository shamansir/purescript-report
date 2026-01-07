module Test.Main where

import Prelude


import Effect.Class (liftEffect)

import Yoga.JSON (readJSON) as JSON
import Foreign (ForeignError, renderForeignError) as F


import Data.Either (Either, either)
import Data.Newtype (unwrap)
import Data.Map (empty) as Map
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Text.Diff

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Effect (Effect)
import Effect.Aff (launchAff_, delay)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Utils (shouldEqual) as U

import GameLog.Dhall (FromDhall, dhallToAchievements) as GL
import GameLog.Types.Game (GameId(..), GameTag, gameName) as GL
import GameLog.Types.ManyGamesStats (GamesReport, fromArray, RawAchievements) as GL
import GameLog.Types.Achievement (Tag) as GL
import Report (toReport)

import Report.Convert.Generic (includeAll) as R
import Report.Convert.Dhall (toDhall) as D


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    describe "convert" do
      it "dhall" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportDhall = D.toDhall @GL.RawAchievements @GL.GameId @GL.GameTag @GL.Tag R.includeAll glReport
                reportDhall `U.shouldEqual` expectedDhall
            )
            eDhallGameCollection

        -- either
        --     (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
        --     (\achs -> "isAwesome" `U.shouldEqual` "isAwesome")
        --     reportDhall


expectedDhall = """let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "DHL:astral-chain"
        , name = "Astral Chain"
        , platform = GT.Platform.<TODO>
        , playtime = GT.Playtime.<TODO>
        }
        (Some { day = +12, mon = +8, year = +2025 }) (

    T.group "File" [ "00-file" ]
        [ T.kv_ "Time" (T.v_time { hrs = +7, min = +54, sec = +0 })
        , T.kv_ "Duration" (T.v_time { hrs = +36, min = +48, sec = +56 })
        , T.kv_ "Date" (T.v_date { day = +11, mon = +1, year = +2001 })
        , T.kv_ "Name" (T.v_t "File 11")
        , T.kv_ "Chapter" (T.v_t "Reckoning")
        , T.kv_ "Rank" (T.v_t "Riotsu")
        , T.kv_ "Grade" (T.v_t "Expert")
        , T.kv_ "Play Style" (T.v_t "Pt Standard")
        ]

    # T.group "Stats" [ "01-stats" ]
        [ T.kv_ "Order Completion" (T.v_pi { done = +67, total = +185 })
        ]

    # T.group "Hero" [ "01-stats", "00-hero" ]
        [ T.kv_ "Health" (T.v_rng { from = +1200, to = +1200 })
        , T.kv_ "Resonance" (T.v_pct 1.0)
        , T.kv_ "ATK" (T.v_i +50)
        , T.kv_ "Money" (T.v_mes +628 "G")
        , T.kv_ "Gene Codes" (T.v_i +514)
        , T.kv_ "Duty Evaluation" (T.v_i +41832)
        , T.kv_ "Order Completion" (T.v_pct 0.36)
        ]

    # T.group "Weapons" [ "01-stats", "01-weapons" ]
        [ T.kv_ "X-Baton Level" (T.v_i +6)
        , T.kv_ "Baton Atk" (T.v_i +130)
        , T.kv_ "Blaster Atk" (T.v_i +58)
        , T.kv_ "Gladius Atk" (T.v_i +282)
        , T.kv_ "Limiter Refill" (T.v_pct 1.1)
        , T.kv_ "Command Skills Unlocked" (T.v_i +5)
        , T.kv_ "Legatus Level" (T.v_i +4)
        , T.kv_ "AED Recovery" (T.v_pct 1.0)
        , T.kv_ "AED Speed" (T.v_mes +7 "sec")
        , T.kv_ "Limiter Max" (T.v_i +130)
        , T.kv_ "Additional AED Batteries" (T.v_i +1)
        ]

    # T.group "Basic" [ "01-stats", "02-basic" ]
        [ T.kv_ "All Pure Platinum" (T.v_none_)
        , T.kv_ "Red Cases Closed"
            ( T.v_lvli
                { reached = +90
                , levels =
                    [ { maximum = +1, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +25, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +50, name = "Lvl. 3" } // T.inj/no_date
                    , { maximum = +131, name = "Lvl. Max" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Blue Cases Closed"
            ( T.v_lvli
                { reached = +68
                , levels =
                    [ { maximum = +1, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +25, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +50, name = "Lvl. 3" } // T.inj/no_date
                    , { maximum = +87, name = "Lvl. 4" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Sword Scholar" (T.v_none_)
        , T.kv_ "Sword Master" (T.v_none_)
        , T.kv_ "Arrow Scholar" (T.v_done_)
        , T.kv_ "Arrow Master" (T.v_none_)
        , T.kv_ "Arm Scholar" (T.v_done_)
        , T.kv_ "Arm Master" (T.v_none_)
        , T.kv_ "Beast Scholar" (T.v_done_)
        , T.kv_ "Beast Master" (T.v_none_)
        , T.kv_ "Axe Scholar" (T.v_none_)
        , T.kv_ "Axe Master" (T.v_none_)
        , T.kv_ "Legionis Dominus" (T.v_vone_)
        , T.kv_ "X-Baton Master" (T.v_vone_)
        , T.kv_ "Legatus Master" (T.v_vone_)
        ]

    # T.group "Chapters" [ "01-stats", "02-basic", "00-chapters" ]
        [ T.kv_ "Startup" (T.v_done_)
        , T.kv_ "Awake" (T.v_done_)
        , T.kv_ "Link" (T.v_done_)
        , T.kv_ "Siedge" (T.v_done_)
        , T.kv_ "Accord" (T.v_done_)
        , T.kv_ "Complicit" (T.v_done_)
        , T.kv_ "Wild" (T.v_done_)
        , T.kv_ "Peace" (T.v_done_)
        , T.kv_ "Salvation" (T.v_done_)
        , T.kv_ "Madness" (T.v_done_)
        , T.kv_ "Reckoning" (T.v_done_)
        ]

    # T.group "S+" [ "01-stats", "02-basic", "01-s-plus" ]
        [ T.kv_ "Startup" (T.v_none_)
        , T.kv_ "Awake" (T.v_none_)
        , T.kv_ "Link" (T.v_none_)
        , T.kv_ "Siedge" (T.v_none_)
        , T.kv_ "Accord" (T.v_none_)
        , T.kv_ "Complicit" (T.v_none_)
        , T.kv_ "Wild" (T.v_none_)
        , T.kv_ "Peace" (T.v_none_)
        , T.kv_ "Salvation" (T.v_none_)
        , T.kv_ "Madness" (T.v_none_)
        , T.kv_ "Reckoning" (T.v_none_)
        ]

    # T.group "Rank" [ "01-stats", "02-basic", "03-rank" ]
        [ T.kv_ "Silver" (T.v_vone_)
        , T.kv_ "Gold" (T.v_vone_)
        , T.kv_ "Platinum" (T.v_vone_)
        , T.kv_ "Pure Platinum" (T.v_vone_)
        ]

    # T.group "Combat" [ "01-stats", "03-combat" ]
        [ T.kv_ "Chimeradicator"
            ( T.v_lvli
                { reached = +781
                , levels =
                    [ { maximum = +100, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +800, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +3000, name = "Lvl. 3" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Itemanical"
            ( T.v_lvli
                { reached = +300
                , levels =
                    [ { maximum = +30, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +300, name = "Lvl. 2" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Round Sword Master" (T.v_pi { done = +5, total = +30 })
        , T.kv_ "Triple Sword Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Slow Shot Master" (T.v_pi { done = +7, total = +30 })
        , T.kv_ "Air Shot Master" (T.v_pi { done = +30, total = +30 })
        , T.kv_ "Gravity Storm Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Round Bullet Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Howl Master" (T.v_pi { done = +7, total = +30 })
        , T.kv_ "Chain Drive Master" (T.v_pi { done = +7, total = +30 })
        , T.kv_ "Crash Bomb Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Blue Shield Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Hit Rush Master" (T.v_pi { done = +9, total = +30 })
        , T.kv_ "Auto-Bind Master" (T.v_pi { done = +4, total = +30 })
        , T.kv_ "Speed Start Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Sync Keep Master" (T.v_pi { done = +0, total = +30 })
        , T.kv_ "Power Charge Master" (T.v_pi { done = +30, total = +30 })
        , T.kv_ "Sync Attack Master" (T.v_pi { done = +30, total = +30 })
        , T.kv_ "Chain Jump Attack Master" (T.v_pi { done = +30, total = +30 })
        , T.kv_ "Chain Counter Attack Master" (T.v_pi { done = +18, total = +30 })
        , T.kv_ "Finishing Move Master" (T.v_pi { done = +30, total = +30 })
        , T.kv_ "Fusion Master" (T.v_pi { done = +2, total = +10 })
        , T.kv_ "Stealth Bind Master" (T.v_pi { done = +27, total = +30 })
        , T.kv_ "Simply Stunning" (T.v_pi { done = +30, total = +30 })
        , T.kv_ "Baton Maniac" (T.v_pi { done = +50000, total = +50000 })
        , T.kv_ "Blaster Maniac" (T.v_pi { done = +40354, total = +50000 })
        , T.kv_ "Gladius Maniac" (T.v_pi { done = +50000, total = +50000 })
        , T.kv_ "Sharpshooter" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Absolute K-9 Unit" (T.v_pi { done = +3, total = +10 })
        , T.kv_ "Armed & Dangerous" (T.v_pi { done = +5, total = +10 })
        , T.kv_ "Parry Professional" (T.v_pi { done = +0, total = +10 })
        , T.kv_ "Ted's Best Customer" (T.v_pi { done = +0, total = +5 })
        ]

    # T.group "Collection" [ "01-stats", "04-collection" ]
        [ T.kv_ "Supply Snatcher"
            ( T.v_lvli
                { reached = +201
                , levels =
                    [ { maximum = +1, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +120, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +327, name = "Max" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Finders Keepers"
            ( T.v_lvli
                { reached = +69
                , levels =
                    [ { maximum = +1, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +152, name = "Lvl. 2" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Red Matter Reducer" (T.v_pi { done = +1000, total = +1000 })
        , T.kv_ "???" T.v_unk
        , T.kv_ "???" T.v_unk
        , T.kv_ "Know Your Enemy"
            ( T.v_lvli
                { reached = +14
                , levels =
                    [ { maximum = +1, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +30, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +80, name = "Lvl. 3" } // T.inj/no_date
                    , { maximum = +167, name = "Lvl. 4" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "People Watcher" (T.v_pi { done = +1, total = +20 })
        , T.kv_ "It's Who You Know" (T.v_pi { done = +1, total = +40 })
        , T.kv_ "Ask Tourist" (T.v_pi { done = +2, total = +28 })
        , T.kv_ "Blueshifter"
            ( T.v_lvli
                { reached = +30
                , levels =
                    [ { maximum = +1, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +10, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +30, name = "Lvl. 3" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Retirement Fund"
            ( T.v_lvli
                { reached = +380128
                , levels =
                    [ { maximum = +10000, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +100000, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +500000, name = "Lvl. 3" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Gene Code Glutton"
            ( T.v_lvli
                { reached = +33814
                , levels =
                    [ { maximum = +1000, name = "Lvl. 1" } // T.inj/no_date
                    , { maximum = +10000, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = +100000, name = "Lvl. 3" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Hard Worker" (T.v_pi { done = +100000, total = +100000 })
        , T.kv_ "Model Officer" (T.v_pi { done = +465057, total = +500000 })
        , T.kv_ "Long Arm of the Law" (T.v_pi { done = +465057, total = +2000000 })
        , T.kv_ "Ark Beautification Society" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "???" T.v_unk
        , T.kv_ "???" T.v_unk
        , T.kv_ "Fashionista" (T.v_pi { done = +13, total = +39 })
        ]

    # T.group "Red Matter Remover" [ "01-stats", "04-collection", "00-red-matter" ]
        [ T.kv_ "File 01" (T.v_none_)
        , T.kv_ "File 02" (T.v_none_)
        , T.kv_ "File 03" (T.v_done_)
        , T.kv_ "File 04" (T.v_done_)
        , T.kv_ "File 05" (T.v_none_)
        , T.kv_ "File 06" (T.v_done_)
        , T.kv_ "File 07" (T.v_done_)
        , T.kv_ "File 08" (T.v_none_)
        , T.kv_ "File 09" (T.v_done_)
        , T.kv_ "File 10" (T.v_done_)
        , T.kv_ "File 11" (T.v_none_)
        ]

    # T.group "Slime Splatter" [ "01-stats", "04-collection", "01-slime" ]
        [ T.kv_ "File 01" (T.v_none_)
        , T.kv_ "File 02" (T.v_none_)
        , T.kv_ "File 03" (T.v_none_)
        , T.kv_ "File 04" (T.v_none_)
        , T.kv_ "File 05" (T.v_none_)
        , T.kv_ "File 06" (T.v_none_)
        , T.kv_ "File 07" (T.v_none_)
        , T.kv_ "File 08" (T.v_none_)
        , T.kv_ "File 09" (T.v_none_)
        , T.kv_ "File 10" (T.v_none_)
        , T.kv_ "File 11" (T.v_none_)
        ]

    # T.group "Feline Friend" [ "01-stats", "04-collection", "02-feline" ]
        [ T.kv_ "File 01" (T.v_none_)
        , T.kv_ "File 02" (T.v_none_)
        , T.kv_ "File 03" (T.v_none_)
        , T.kv_ "File 04" (T.v_done_)
        , T.kv_ "File 05" (T.v_none_)
        , T.kv_ "File 06" (T.v_done_)
        , T.kv_ "File 07" (T.v_done_)
        , T.kv_ "File 08" (T.v_done_)
        , T.kv_ "File 09" (T.v_none_)
        , T.kv_ "File 10" (T.v_none_)
        , T.kv_ "File 11" (T.v_none_)
        ]

    # T.group "Nature Calls" [ "01-stats", "04-collection", "03-nature" ]
        [ T.kv_ "HQ" (T.v_none)
        , T.kv_ "File 01" (T.v_none_)
        , T.kv_ "File 02" (T.v_none_)
        , T.kv_ "File 03" (T.v_done_)
        , T.kv_ "File 04" (T.v_done_)
        , T.kv_ "File 05" (T.v_done_)
        , T.kv_ "File 06" (T.v_none_)
        , T.kv_ "File 07" (T.v_done_)
        , T.kv_ "File 08" (T.v_none_)
        , T.kv_ "File 09" (T.v_done_)
        , T.kv_ "File 10" (T.v_done_)
        , T.kv_ "File 11" (T.v_done_)
        ]

    # T.group "Unique" [ "01-stats", "05-unique" ]
        [ T.kv_ "A Who's Who of Hermits" (T.v_vone_)
        , T.kv_ "Lappy's Helium Hullaballoo" (T.v_vone_)
        , T.kv_ "Sweet Release" (T.v_vone_)
        , T.kv_ "Summer Avalanche" (T.v_vone_)
        , T.kv_ "Serious Student" (T.v_done)
        , T.kv_ "Doggy Door" (T.v_vone_)
        , T.kv_ "Amateur Photographer" (T.v_done_)
        , T.kv_ "Precious Memories" (T.v_pi { done = +24, total = +36 })
        , T.kv_ "???" T.v_unk
        , T.kv_ "???" T.v_unk
        , T.kv_ "???" T.v_unk
        , T.kv_ "Astral Perfection" (T.v_vone_)
        ]

    # T.group "Photo" [ "01-stats", "05-unique", "06-photo" ]
        [ T.kv_ "I See You!" (T.v_vone_)
        , T.kv_ "Daddy Dearest" (T.v_vone_)
        , T.kv_ "Major Malfunction" (T.v_vone_)
        , T.kv_ "Get Me Down!" (T.v_vone_)
        , T.kv_ "Get Well Soon" (T.v_vone_)
        , T.kv_ "Uppers Delight" (T.v_vone_)
        , T.kv_ "Do NOT show Marie!" (T.v_vone_)
        , T.kv_ "Hello, Partner!" (T.v_vone_)
        , T.kv_ "Best Friends!" (T.v_vone_)
        , T.kv_ "Packing Heat!" (T.v_vone_)
        , T.kv_ "Comfort and Justice!" (T.v_vone_)
        ]

    )""" :: String