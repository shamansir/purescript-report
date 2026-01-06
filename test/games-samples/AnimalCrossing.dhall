let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "animal-crossing"
        , name = "Animal Crossing"
        , platform = GT.Platform.Other
        , playtime = GT.Playtime.MoreThan { hrs = +110, min = +0, sec = +0 }
        }
        { day = +29, mon = +8, year = +2025 } (

    T.groupStats "Statistics" [ "00-stats" ]
        [ T.kv_ "Miles Collected" (T.v_i +3270)
        , T.kv_ "Registered at" (T.v_date { day = +25, mon = +3, year = +2020 })
        , T.kv_ "Items Collected" (T.v_pi { done = +335, total = +400 })
        , T.kv_ "Recipes Learned" (T.v_pi { done = +195, total = +1828 })
        , T.kv_ "Music Albums Discovered" (T.v_pi { done = +13, total = +110 })
        , T.kv_ "Reactions Learned" (T.v_pi { done = +39, total = +88 })
        , T.kv_ "Reactions" -- 39/88
            (T.v_lvlp
                { levels =
                    [ { proc = T.p_done_, name = "Greetings" }  // T.inj/no_date
                    , { proc = T.p_todo_, name = "Agreement" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Disagreement" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Pleased" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Happiness" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Joy" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Glee" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Laughter" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Love" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Confident" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Showmanship" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Flourish" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Pride" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Encouraging" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Delight" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Apologetic" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Bewilderment" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Curiousity" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Surprise" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Amazed" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Inspiration" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Shocked" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Mistaken" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Bashfulness" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Shyness" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Sheepishness" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Smirking" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Mischief" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Resignation" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Daydreaming" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Thought" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Sleepy" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Dozing" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Worry" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Sighing" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Sadness" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Heartbreak" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Sorrow" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Intense" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Aggravation" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Cold Chill" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Distress" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Fearful" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Sneezing" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Haunt" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Scare" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Sit Down" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Wave Goodbye" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Take a Picture" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Sniff Sniff" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Work out" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Yoga" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Here You Go" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Excited" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Ta-da" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Confetti" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Viva" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Let's Go" } // T.inj/no_date
                    , { proc = T.p_done_, name = "Feelin' It" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Posture Warming" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Arm Circles" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Side bends" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Body Twists" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Wide Arm Stretch" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Upper Body Ci.." } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Jump" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Double Wave" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Stretch" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Jammin'" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Listening Ears" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Say Cheese" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Behold" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Eager" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Flex" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Work it" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Act Natural" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Hula" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Grooving Hop" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Groove Right" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Groove Left" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Soak It In" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Side-to-Side" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Island Stomp" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Airplane" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Twisty Dance" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Shimmy" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Turnip Patch" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "Arm-Swinging Da..." } // T.inj/no_date
                    ]
                }
            )
        ]

    # T.group "Museum" [ "00-stats", "00-museum" ]
        [ T.kv_ "Bugs Collected" (T.v_pi { done = +34, total = +80 })
        , T.kv_ "Fish" (T.v_pi { done = +23, total = +80 })
        , T.kv_ "Sea Critters" (T.v_pi { done = +6, total = +40 })
        , T.kv_ "Fossils" (T.v_pi { done = +51, total = +73 })
        , T.kv_ "Art" (T.v_pi { done = +1, total = +43 })
        ]

    # T.group "Nook Miles" [ "01-nook-miles" ]
        [ T.kv_ "Techless Miles!" (T.v_done)  // T.inj/date { day = +25, mon = +3, year = +2020 } -- Started on the Island
        , T.kv_ "Angling For Perfection!" -- 95/100 Fishing Mastering
            (T.v_lvlio
                { reached = +95
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 1" } // T.inj/date { day = +29, mon = +3, year = +2020 }
                    , { maximum = Some +100, name = "Lvl. 2" } // T.inj/no_date
                    , { maximum = None Integer, name = "Lvl. 3" } // T.inj/no_date
                    , { maximum = None Integer, name = "Lvl. 4" } // T.inj/no_date
                    , { maximum = None Integer, name = "Lvl. 5" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Fishing Mastering"
        , T.kv_ "Island Ichtyologist" -- 24/40 Fill Fish in Critterpedia
            (T.v_lvlio
                { reached = +24
                , levels =
                    [ { maximum = Some +10, name = "Lvl. 1" } // T.inj/date { day = +30, mon = +3, year = +2020 }
                    , { maximum = Some +20, name = "Lvl. 2" } // T.inj/date { day = +5, mon = +5, year = +2020 }
                    , { maximum = None Integer, name = "Lvl. 3" } // T.inj/no_date
                    , { maximum = None Integer, name = "Lvl. 4" } // T.inj/no_date
                    , { maximum = None Integer, name = "Lvl. 5" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Fill Fish in Critterpedia"
        , T.kv_ "Island Togetherness" -- 8/10 Chat with residents
            (T.v_lvlio
                { reached = +8
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +26, mon = +3, year = +2020 }
                    , { maximum = Some +10, name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Chat with residents"
        , T.kv_ "You've Got the Bug" -- 109/500 Catch bugs
            (T.v_lvlio
                { reached = +109
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +29, mon = +3, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 02" } // T.inj/date { day = +16, mon = +11, year = +2020 }
                    , { maximum = Some +500,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Catch bugs"
        , T.kv_ "Bugs Don't Bug Me" -- 34/40 Fill Bugs in Critterpedia
            (T.v_lvlio
                { reached = +34
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +1, mon = +4, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/date { day = +5, mon = +5, year = +2020 }
                    , { maximum = Some +40,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Fill Bugs in Critterpedia"
        , T.kv_ "Have a Nice DIY" -- 195/200 Collect DIY Recipes
            (T.v_lvlio
                { reached = +195
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/date { day = +4, mon = +4, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 03" } // T.inj/date { day = +12, mon = +4, year = +2020 }
                    , { maximum = Some +150,  name = "Lvl. 04" } // T.inj/date { day = +29, mon = +5, year = +2020 }
                    , { maximum = Some +200,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Collect DIY Recipes"
        , T.kv_ "Deep Dive" -- 8/50 Dive in the Sea
            (T.v_lvlio
                { reached = +8
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +4, mon = +10, year = +2020 }
                    , { maximum = Some +50, name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Dive in the Sea"
        , T.kv_ "Underwater Understudy" -- 6/10
            (T.v_lvlio
                { reached = +6
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +4, mon = +10, year = +2020 }
                    , { maximum = Some +50, name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Fill Deep Sea Creatures in Critterpedia"
        , T.kv_ "DIY Tools" -- 132/200 Make DIY Tools
            (T.v_lvlio
                { reached = +132
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +50, name = "Lvl. 02" } // T.inj/date { day = +12, mon = +4, year = +2020 }
                    , { maximum = Some +200, name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Make DIY Tools"
        , T.kv_ "DIY Furniture" -- 64/200 Craft DIY Furniture
            (T.v_lvlio
                { reached = +64
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +29, mon = +3, year = +2020 }
                    , { maximum = Some +50, name = "Lvl. 02" } // T.inj/date { day = +17, mon = +5, year = +2020 }
                    , { maximum = Some +200, name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Craft DIY Furniture"
        , T.kv_ "Furniture Freshener" -- 2/5 Refurbish DIY Furniture
            (T.v_lvlio
                { reached = +2
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Refurbish DIY Furniture"
        , T.kv_ "Rough-hewn" -- 1348/2000 Chop Wood from the Trees
            (T.v_lvlio
                { reached = +1348
                , levels =
                    [ { maximum = Some +20,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +100, name = "Lvl. 02" } // T.inj/date { day = +2, mon = +4, year = +2020 }
                    , { maximum = Some +500, name = "Lvl. 03" } // T.inj/date { day = +7, mon = +4, year = +2020 }
                    , { maximum = Some +2000, name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Chop Wood from the Trees"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Trashed Tools" -- 104/200 Tools Broken
            (T.v_lvlio
                { reached = +104
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/date { day = +3, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 03" } // T.inj/date { day = +1, mon = +5, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 04" } // T.inj/date { day = +29, mon = +3, year = +2021 }
                    , { maximum = Some +200,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Tools Broken"
        , T.kv_ "Rock-Splitting Champ" (T.v_done)  // T.inj/date { day = +5, mon = +5, year = +2020 } -- Whack a rock
        , T.kv_ "Bona Fide Bone Finder" (T.v_done)  // T.inj/date { day = +29, mon = +3, year = +2020 } -- Find the first fossil
        , T.kv_ "Fossil Assesment" -- 96/200 Assess Fossils
            (T.v_lvlio
                { reached = +96
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +30, mon = +3, year = +2020 }
                    , { maximum = Some +30,  name = "Lvl. 02" } // T.inj/date { day = +7, mon = +4, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Assess Fossils"
        , T.kv_ "Nice to Meet You, Gyroid!" (T.v_done)  // T.inj/date { day = +13, mon = +11, year = +2021 } -- Find the first gyroid
        , T.kv_ "Gyroid Getter" -- 3/5 Different Kinds of Gyroids
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Different Kinds of Gyroids"
        , T.kv_ "Greedy Weeder" -- 484/1000 Sell weeds
            (T.v_lvlio
                { reached = +484
                , levels =
                    [ { maximum = Some +50,  name = "Lvl. 01" } // T.inj/date { day = +3, mon = +4, year = +2020 }
                    , { maximum = Some +200,  name = "Lvl. 02" } // T.inj/date { day = +7, mon = +4, year = +2020 }
                    , { maximum = Some +1000,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Sell weeds"
        , T.kv_ "Flower Power" -- 5/10 Plant Flowers
            (T.v_lvlio
                { reached = +5
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Sell weeds"
        , T.kv_ "Flower Tender" -- 108/500 Shower Flowers
            (T.v_lvlio
                { reached = +108
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +1, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/date { day = +8, mon = +4, year = +2020 }
                    , { maximum = Some +100, name = "Lvl. 03" } // T.inj/date { day = +29, mon = +8, year = +2025 }
                    , { maximum = Some +500,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Shower Flowers"
        , T.kv_ "Tomorrow's Trees Today" -- 1/5 Plant Trees
            (T.v_lvlio
                { reached = +1
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Plant Trees"
        , T.kv_ "Pick of the Bunch" -- 3000/3000 Sell Fruits
            (T.v_lvlio
                { reached = +3000
                , levels =
                    [ { maximum = Some +20,  name = "Lvl. 01" } // T.inj/date { day = +3, mon = +4, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 02" } // T.inj/date { day = +6, mon = +4, year = +2020 }
                    , { maximum = Some +500,  name = "Lvl. 03" } // T.inj/date { day = +5, mon = +5, year = +2020 }
                    , { maximum = Some +1000,  name = "Lvl. 04" } // T.inj/date { day = +23, mon = +5, year = +2020 }
                    , { maximum = Some +3000,  name = "Lvl. 05" } // T.inj/date { day = +5, mon = +12, year = +2020 }
                    ]
                }
            ) // T.inj/det "Sell Fruits"
        , T.kv_ "Fruit Roots"-- 4/6 Grow different kinds of Fruits -- FIXME:
            (T.v_lvlp
                { levels =
                    [ { proc = T.p_done_, name = "A: Cherries" } // T.inj/date { day = +12, mon = +4, year = +2020 }
                    , { proc = T.p_done_, name = "B: Oranges" }  // T.inj/date { day = +12, mon = +4, year = +2020 }
                    , { proc = T.p_done_, name = "C: Pears" } // T.inj/date { day = +28, mon = +4, year = +2020 }
                    , { proc = T.p_todo_, name = "D: ???" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "E: ???" } // T.inj/no_date
                    , { proc = T.p_done_, name = "F: Coconuts" } // T.inj/date { day = +4, mon = +4, year = +2020 }
                    ]
                }
            ) // T.inj/det "Grow different kinds of Fruits"
        , T.kv_ "Shrubbery Hubbubbery" -- 3/5 Plant shrubs
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +23, mon = +4, year = +2020 }
                    , { maximum = None Integer,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Plant shrubs"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Executive Producer" -- 84/150 Eat what you grow
            (T.v_lvlio
                { reached = +84
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +11, mon = +11, year = +2021 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/date { day = +5, mon = +12, year = +2021 }
                    , { maximum = Some +150,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Eat what you grow"
        , T.kv_ "Go Ahead. Be Shellfish!" -- 298/500 Sell Shellfish
            (T.v_lvlio
                { reached = +298
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/date { day = +3, mon = +4, year = +2020 }
                    , { maximum = Some +200,  name = "Lvl. 03" } // T.inj/date { day = +14, mon = +4, year = +2020 }
                    , { maximum = Some +500,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Sell Shellfish"
        , T.kv_ "Clam and Collected" -- 28/50 Dig Up Clams
            (T.v_lvlio
                { reached = +28
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +29, mon = +3, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/date { day = +13, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Dig Up Clams"
        , T.kv_ "Trash Fishin'" -- 3/10 Fish the trash, clean the Island
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +3,  name = "Lvl. 01" } // T.inj/date { day = +3, mon = +6, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Fish the trash, clean the Island"
        , T.kv_ "Cast Master" -- 16/50 Catch Fish in a row
            (T.v_lvlio
                { reached = +16
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +18, mon = +5, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Catch Fish in a row"
        , T.kv_ "Dream House" -- 5/6 Expand your home
            (T.v_lvlio
                { reached = +5
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +2, mon = +4, year = +2020 }
                    , { maximum = Some +2,  name = "Lvl. 02" } // T.inj/date { day = +4, mon = +4, year = +2020 }
                    , { maximum = Some +5,  name = "Lvl. 03" } // T.inj/date { day = +6, mon = +10, year = +2020 }
                    , { maximum = Some +6,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Expand your home"
        , T.kv_ "Decorated Decorator!" (T.v_done)  // T.inj/date { day = +16, mon = +6, year = +2020 } -- Get S Ranking for the Decoration
        , T.kv_ "Hoard Reward" -- 95/100 Have furniture
            (T.v_lvlio
                { reached = +95
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +2, mon = +4, year = +2020 }
                    , { maximum = Some +15,  name = "Lvl. 02" } // T.inj/date { day = +5, mon = +4, year = +2020 }
                    , { maximum = Some +30,  name = "Lvl. 03" } // T.inj/date { day = +20, mon = +4, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Have furniture"
        , T.kv_ "Good Things in Store" -- 300/300 Items in the Storage
            (T.v_lvlio
                { reached = +300
                , levels =
                    [ { maximum = Some +20,  name = "Lvl. 01" } // T.inj/date { day = +3, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/date { day = +11, mon = +4, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 03" } // T.inj/date { day = +28, mon = +4, year = +2020 }
                    , { maximum = Some +200,  name = "Lvl. 04" } // T.inj/date { day = +10, mon = +6, year = +2020 }
                    , { maximum = Some +300,  name = "Lvl. 05" } // T.inj/date { day = +11, mon = +11, year = +2021 }
                    ]
                }
            ) // T.inj/det "Items in the Storage"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Smile Isle" -- 8/10 Fullfill residents' requests
            (T.v_lvlio
                { reached = +8
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +8, mon = +4, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Fullfill residents' requests"
        , T.kv_ "Reaction Ruler"-- 39/42 Learn Reactions
            (T.v_lvlio
                { reached = +39
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +5, mon = +4, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/date { day = +24, mon = +4, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 03" } // T.inj/date { day = +3, mon = +6, year = +2020 }
                    , { maximum = Some +30,  name = "Lvl. 04" } // T.inj/date { day = +29, mon = +3, year = +2021 }
                    , { maximum = Some +42,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Learn Reactions"
        , T.kv_ "Island Shutterbug!" (T.v_done)  // T.inj/date { day = +29, mon = +3, year = +2020 } -- First Island Photo
        , T.kv_ "Edit Credit" (T.v_done)  // T.inj/date { day = +25, mon = +3, year = +2020 } -- Edit Passport Details
        , T.kv_ "Nook Phone Life!" (T.v_done)  // T.inj/date { day = +25, mon = +3, year = +2020 } -- Use NookPhone
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Shop to it" -- 50/100 Shop with NookShopping
            (T.v_lvlio
                { reached = +50
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/date { day = +2, mon = +6, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 03" } // T.inj/date { day = +1, mon = +4, year = +2021 }
                    , { maximum = Some +100,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Shop with NookShopping"
        , T.kv_ "Growing Collection" -- 471/500 Expand NookShopping Catalogue
            (T.v_lvlio
                { reached = +471
                , levels =
                    [ { maximum = Some +100,  name = "Lvl. 01" } // T.inj/date { day = +7, mon = +4, year = +2020 }
                    , { maximum = Some +200,  name = "Lvl. 02" } // T.inj/date { day = +26, mon = +4, year = +2020 }
                    , { maximum = Some +300,  name = "Lvl. 03" } // T.inj/date { day = +17, mon = +5, year = +2020 }
                    , { maximum = Some +400,  name = "Lvl. 04" } // T.inj/date { day = +19, mon = +7, year = +2020 }
                    , { maximum = Some +500,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Expand NookShopping Catalogue"
        , T.kv_ "Nook Miles for Miles!" -- 156/200 Earn Miles by Getting Miles
            (T.v_lvlio
                { reached = +156
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +2, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 02" } // T.inj/date { day = +30, mon = +4, year = +2020 }
                    , { maximum = Some +200,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Earn Miles by Getting Miles"
        , T.kv_ "First Time Buyer!" (T.v_done)  // T.inj/date { day = +29, mon = +3, year = +2020 } -- Buy something first time
        , T.kv_ "Seller of Unwanted Stuff!" (T.v_done)  // T.inj/date { day = +27, mon = +3, year = +2020 } -- Fist sale
        , T.kv_ "Moving Fees Paid!" (T.v_done)  // T.inj/date { day = +29, mon = +3, year = +2020 } -- Move to the Island
        , T.kv_ "Bell Ringer" -- 681960/2000000 Spend Bells
            (T.v_lvlio
                { reached = +681960
                , levels =
                    [ { maximum = Some +5000,  name = "Lvl. 01" } // T.inj/date { day = +3, mon = +4, year = +2020 }
                    , { maximum = Some +50000,  name = "Lvl. 02" } // T.inj/date { day = +11, mon = +4, year = +2020 }
                    , { maximum = Some +500000,  name = "Lvl. 03" } // T.inj/date { day = +3, mon = +10, year = +2020 }
                    , { maximum = Some +2000000,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Spend Bells"
        , T.kv_ "Miles for Stalkholders" (T.v_done) // T.inj/date { day = +5, mon = +4, year = +2020 } // T.inj/det "Purchase Turnips"
        , T.kv_ "Cornering the Stalk Market" -- 16730/100000 Turnip Transactions
            (T.v_lvlio
                { reached = +16730
                , levels =
                    [ { maximum = Some +1000,  name = "Lvl. 01" } // T.inj/date { day = +21, mon = +4, year = +2020 }
                    , { maximum = Some +10000,  name = "Lvl. 02" } // T.inj/date { day = +20, mon = +5, year = +2020 }
                    , { maximum = Some +100000,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Turnip Transactions"
        , T.kv_ "No More Loan Playments" (T.v_done) // T.inj/date { day = +3, mon = +4, year = +2020 } // T.inj/det "(No more) Paying for Loan"
        , T.kv_ "Bulletin-Board Benefit" (T.v_done) // T.inj/date { day = +29, mon = +3, year = +2020 } // T.inj/det "Write Something on the Bulletin Board"
        , T.kv_ "Popular Pen Pal" -- 8/20 Write letters
            (T.v_lvlio
                { reached = +8
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +2, mon = +5, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Write letters"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Faint of Heart" (T.v_done) // T.inj/date { day = +29, mon = +3, year = +2020 } // T.inj/det "Meet Ghosts"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "It's Raining Treasure" -- 91/100 Shoot Balloons with Gifts
            (T.v_lvlio
                { reached = +91
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/date { day = +1, mon = +4, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/date { day = +4, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 03" }  // T.inj/date { day = +17, mon = +5, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Shoot Balloons with Gifts"
        , T.kv_ "Fun with Fences" (T.v_pi { done = +0, total = +20 }) // T.inj/det "Install Fences"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Wishes Come True" -- 38/200 Wish by Shooting Stars
            (T.v_lvlio
                { reached = +38
                , levels =
                    [ { maximum = Some +10,  name = "Lvl. 01" } // T.inj/date { day = +23, mon = +5, year = +2020 }
                    , { maximum = Some +30,  name = "Lvl. 02" } // T.inj/date { day = +29, mon = +3, year = +2021 }
                    , { maximum = Some +200,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Wish by Shooting Stars"
        , T.kv_ "Exterior Decorator" (T.v_pi { done = +10, total = +10 }) // T.inj/date { day = +29, mon = +3, year = +2020 } // T.inj/det "Put furniture outside"
        , T.kv_ "Techless Icons" -- 1/2 Select island flag and tune
            (T.v_lvlp
                { levels =
                    [ { proc = T.p_done_, name = "A: Select flag" } // T.inj/no_date
                    , { proc = T.p_todo_, name = "B: Select tune" } // T.inj/date { day = +11, mon = +11, year = +2021 }
                    ]
                }
            ) // T.inj/det "Select island flag and tune"
        , T.kv_ "Island Designer" -- 3/3 Build path, river, and cliff
            (T.v_lvlp
                { levels =
                    [ { proc = T.p_done_, name = "A: Path" }  // T.inj/date { day = +15, mon = +5, year = +2020 }
                    , { proc = T.p_done_, name = "B: River" } // T.inj/date { day = +29, mon = +5, year = +2020 }
                    , { proc = T.p_done_, name = "C: Cliff" } // T.inj/date { day = +18, mon = +5, year = +2020 }
                    ]
                }
            ) // T.inj/det "Build path, river, and cliff"
        , T.kv_ "Wispy Island Secrets" -- 3/10 Discover Island Secrets
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +1, mon = +4, year = +2020 }
                    , { maximum = Some +10, name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Discover Island Secrets"
        , T.kv_ "Guiliver's Travails" -- 4/10 Help in troubles at the Beach
            (T.v_lvlio
                { reached = +4
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +8, mon = +4, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Help in troubles at the Beach"
        , T.kv_ "K.K. Mania" -- 7/10 Attend K.K. Shows
            (T.v_lvlio
                { reached = +7
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +13, mon = +5, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Attend K.K. Shows"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Set Sail for Adventure" -- 3/10 Tour with the Sailor
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +5, mon = +12, year = +2021 }
                    , { maximum = Some +3,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Tour with the Sailor"
        , T.kv_ "Come Home to the Roost" -- 1/5 Sip a coffee at the Roost
            (T.v_lvlio
                { reached = +1
                , levels =
                    [ { maximum = Some +5,  name = "Lvl. 01" } // T.inj/no_date
                    , { maximum = Some +3,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Sip a coffee at the Roost"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "True Friends" -- 4/10 Establish true Friends
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +28, mon = +4, year = +2020 }
                    , { maximum = Some +2,  name = "Lvl. 02" } // T.inj/date { day = +8, mon = +5, year = +2020 }
                    , { maximum = Some +3,  name = "Lvl. 03" } // T.inj/date { day = +15, mon = +5, year = +2020 }
                    ]
                }
            ) // T.inj/det "Establish true Friends"
        , T.kv_ "Birthday Celebration" -- 1/10 Celebrate Birthdays of the Residents
            (T.v_lvlio
                { reached = +1
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +19, mon = +7, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Celebrate Birthdays of the Residents"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Making a Change" (T.v_done) // T.inj/date { day = +2, mon = +5, year = +2020 } // T.inj/det "Change your outfit"
        , T.kv_ "First Custom Design!" (T.v_none) // T.inj/no_date // T.inj/det "Use Custom Design App"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Paydirt!" (T.v_done) // T.inj/date { day = +29, mon = +3, year = +2020 } // T.inj/det "Treasure from buriying Bells"
        , T.kv_ "Shady Shakedown" -- 3/10 Shake Furniture from Trees
            (T.v_lvlio
                { reached = +3
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +17, mon = +5, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Shake Furniture from Trees"
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Island and Yourland" -- 2/5 Visit neighbour islands
            (T.v_lvlio
                { reached = +2
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +29, mon = +3, year = +2020 }
                    , { maximum = Some +5,  name = "Lvl. 02" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Visit neighbour islands"
        , T.kv_ "Host the Most" -- 5/10 Have guests from neighbour islands
            (T.v_lvlio
                { reached = +5
                , levels =
                    [ { maximum = Some +1,  name = "Lvl. 01" } // T.inj/date { day = +27, mon = +3, year = +2020 }
                    , { maximum = Some +5,  name = "Lvl. 02" } // T.inj/date { day = +22, mon = +4, year = +2020 }
                    , { maximum = Some +10,  name = "Lvl. 03" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det " Have guests from neighbour islands"
        , T.kv_ "Active Island Resident" -- 96/100 Number of active days on the island
            (T.v_lvlio
                { reached = +96
                , levels =
                    [ { maximum = Some +3,  name = "Lvl. 01" } // T.inj/date { day = +29, mon = +3, year = +2020 }
                    , { maximum = Some +20,  name = "Lvl. 02" } // T.inj/date { day = +16, mon = +4, year = +2020 }
                    , { maximum = Some +50,  name = "Lvl. 03" } // T.inj/date { day = +23, mon = +5, year = +2020 }
                    , { maximum = Some +100,  name = "Lvl. 04" } // T.inj/no_date
                    , { maximum = None Integer,  name = "Lvl. 05" } // T.inj/no_date
                    ]
                }
            ) // T.inj/det "Number of active days on the island"
        ]

    )

-- Reactions
    -- Greerings
    -- Disagreement
    -- Happiness
    -- Joy
    -- Laughter
    -- Encouraging
    -- Delight
    -- Apologetic
    -- Bewilderment
    -- Curiosity
    -- Surprise

    -- Amazed
    -- Shocked
    -- Mistaken
    -- Bashfullness
    -- Shyness
    -- Sheepishness
    -- Smirking
    -- Resignation
    -- Dozing
    -- Heartbreak
    -- Sorrow

    -- Intense
    -- Cold Chill
    -- Distress
    -- Sneezing
    -- Sit down
    -- Wave goodbye
    -- Take a picture
    -- Sniff Sniff
    -- Work Out
    -- Yoga
    -- Here You Go

    -- Excited
    -- Ta-da
    -- Confetti
    -- Viva
    -- Let's Go
    -- Feelin' it
    -- ???
    -- ???
    -- ???
    -- ???
    -- ???

-- Residents
    -- Gaston
    -- Cobb
    -- Drift
    -- Benjamin
    -- Freckles
    -- Nibbles
    -- Olivia
    -- Eugene
    -- Olive
    -- Amelia



-- Level (\d+) // T\.inj/date \{ day = \+(\d+), mon = \+(\d+), year = \+(\d+) \} -- (\d+)
-- , { maximum = +$5,  name = "Lvl. $1" } // T.inj/date { day = +$2, mon = +$3, year = +$4 }
