let T = ./Types.dhall

in
    T.collapseAt
        { id = "nonograms-katana"
        , name = "Nonograms Katana"
        , platform = T.Platform.GPlay
        , playtime = T.Playtime.Unknown
        }
        { day = +12, mon = +8, year = +2025 } (

    T.group "Statistics" [ "00-stats" ]
        [ T.kv_ "Black-and-white" (T.v_i +130)
        , T.kv_ "Colored" (T.v_i +144)
        , T.kv_ "Standard" (T.v_i +183)
        , T.kv_ "Sent by Users" (T.v_i +91)
        -- Size
        , T.kv_ "Small" (T.v_i +138)
        , T.kv_ "Medium" (T.v_i +120)
        , T.kv_ "Big" (T.v_i +9)
        , T.kv_ "Huge" (T.v_i +7)
        -- Way
        , T.kv_ "Coloring" (T.v_i +11)
        , T.kv_ "Mosaic" (T.v_i +10)
        , T.kv_ "Trial-and-error method" (T.v_i +14)
        , T.kv_ "True Nonogram" (T.v_i +11)
        -- Total (Bold)
        , T.kv_ "Total" (T.v_i +274)
        -- Started
        , T.kv_ "Started" (T.v_i +0)
        -- Other
        , T.kv_ "Categories"   (T.v_pi { done = +7, total = +31 })
        , T.kv_ "Achievements" (T.v_pi { done = +10, total = +84 })
        , T.kv_ "Time" (T.v_time { hrs = +0, min = +0, sec = +0 })
        , T.kv_ "Total Score" (T.v_i +83361)
        ]

    # T.group "Achievements" [ "01-achievements" ]
        [ T.kvd_ "I Know the Rules" "Complete the Tutorial" (T.v_done)
        , T.kvd_ "5x5 Expert" "Complete Category: 5x5" (T.v_pcti_done)
        , T.kvd_ "Smile in Color" "Complete Category: Color Beginner" (T.v_pcti_done)
        , T.kvd_ "Amateur" "Score 10000 Points" (T.v_pcti_done)
        , T.kvd_ "Professional" "Score 50000 Points" (T.v_pcti_done)
        , T.kvd_ "Master" "Score 100000 Points" (T.v_pcti +83)
        , T.kvd_ "50 Nonograms" "Solve 50 Nonograms" (T.v_pcti_done)
        , T.kvd_ "100 Nonograms" "Solve 100 Nonograms" (T.v_pcti_done)
        , T.kvd_ "500 Nonograms" "Solve 500 Nonograms" (T.v_pcti +36)
        , T.kvd_ "100 Colors" "Solve 100 Color Nonograms" (T.v_pcti +54)
        , T.kvd_ "Step by Step" "Complete 5 Categories" (T.v_pcti_done)
        , T.kvd_ "Terminator" "Complete 10 Categories" (T.v_pcti +70)
        , T.kvd_ "The Gamer" "Solve 30 Puzzles in Cartegory: Fantasy RPG" (T.v_pcti_done)
        , T.kvd_secret
        , T.kvd_secret
        , T.kvd_secret
        , T.kvd_ "Veteran of 1000 Nonograms" "Solve 1000 Nonograms" (T.v_pcti +27)
        , T.kvd_ "700 User Nonograms" "Solve 700 Nonograms sent by users" (T.v_pcti +13)
        , T.kvd_ "2000 User Nonograms" "Solve 2000 Nonograms sent by users" (T.v_pcti +4)
        , T.kvd_ "Anime Fan" "Solve 500 Nonograms tagged \"Anime, Manga\"" (T.v_pcti_none)
        , T.kvd_ "Cat Lover" "Solve 500 Nonograms tagged \"Cats\"" (T.v_pcti_none)
        , T.kvd_ "Dragon Hunter" "Solve 150 Nonograms tagged \"Dragons\"" (T.v_pcti_none)
        , T.kvd_ "Sunset Watcher" "Solve 150 Nonograms tagged \"Twilight, Sunsets\"" (T.v_pcti_none)
        , T.kvd_ "Major Tom" "Solve 150 Nonograms tagged \"Space\"" (T.v_pcti_none)
        , T.kvd_ "Admirer of Japanese Culture" "Solve 150 Nonograms tagged \"Japan\"" (T.v_pcti +8)
        , T.kvd_ "Pirate!" "Solve 100 Nonograms tagged \"Pirates!\"" (T.v_pcti_none)
        , T.kvd_ "Minimalist" "Solve 500 Nonograms of size 5x5" (T.v_pcti +5)
        , T.kvd_ "Maximalist" "Solve 500 Nonograms of size 80x80" (T.v_pcti +5)
        , T.kvd_ "Truth Seeker" "Solve 1500 True Nonograms" (T.v_pcti +36)
        , T.kvd_ "Adept" "Solve 5000 Nonograms" (T.v_pcti +5)
        , T.kvd_ "Bachelor" "Solve 10000 Nonograms" (T.v_pcti +2)
        , T.kvd_ "Magister" "Solve 15000 Nonograms" (T.v_pcti +1)
        , T.kvd_ "Professor" "Solve 20000 Nonograms" (T.v_pcti +1)
        , T.kvd_ "Academician" "Solve 25000 Nonograms" (T.v_pcti +1)
        , T.kvd_ "Laureate" "Solve 30000 Nonograms" (T.v_pcti_none)
        , T.kvd_ "Level 10: Scout" "Reach level 10 in the guild" (T.v_pcti +10)
        , T.kvd_ "Level 30: Explorer" "Reach level 30 in the guild" (T.v_pcti +3)
        , T.kvd_ "Level 50: Guild Master" "Reach level 50 in the guild" (T.v_pcti +2)
        , T.kvd_ "Level 80: Enlightened" "Reach level 80 in the guild" (T.v_pcti +1)
        , T.kvd_ "Knowledge is Power" "Learn any 7 skills" (T.v_pcti_none)
        , T.kvd_ "New Home" "Build any building" (T.v_none)
        , T.kvd_ "Construction is in Full Swing" "Build 10 buildings to level 1" (T.v_pcti_none)
        , T.kvd_ "The Last Brick" "Build all buildings to the maximum level" (T.v_pcti_none)
        , T.kvd_ "The First Mosaic" "Collect all fragments of mosaic #1" (T.v_none)
        , T.kvd_ "The Last Mosaic" "Collect all fragments of mosaic #10" (T.v_pcti_none)
        , T.kvd_ "The Templar Gold" "Loot: Coint: 1000" (T.v_pcti_none)
        , T.kvd_ "Montezuma's Treasure" "Loot: Ruby: 30" (T.v_pcti_none)
        , T.kvd_ "Pharaoh's Tomb" "Loot: Gold Ignot: 50" (T.v_pcti_none)
        , T.kvd_ "Blackbeards's Treasure" "Loot: Treasure: 50" (T.v_pcti_none)
        , T.kvd_ "A Month Later..." "Complete the quest: \"Puzzle of the day\" 30 times" (T.v_pcti_none)
        , T.kvd_ "Bounty Hunter" "Complete the quest: \"WANTED\" 40 times" (T.v_pcti_none)
        , T.kvd_ "Fan" "Craft: Fan" (T.v_none)
        , T.kvd_ "Arrows" "Craft: Arrows" (T.v_none)
        , T.kvd_ "Charcoal" "Craft: Charcoal" (T.v_none)
        , T.kvd_ "Wooden Beam" "Craft: Wooden Beam" (T.v_none)
        , T.kvd_ "Steel" "Craft: Steel" (T.v_none)
        , T.kvd_ "Gunpowder" "Craft: Gunpowder" (T.v_none)
        , T.kvd_ "Katana" "Craft: Katana" (T.v_none)
        , T.kvd_ "Bomb (Horoku)" "Craft: Bomb (Horoku)" (T.v_none)
        , T.kvd_ "Battering Ram (Kikkosha)" "Craft: Battering Ram (Kikkosha)" (T.v_none)
        , T.kvd_ "Great Traveler" "Complete 20 expeditions" (T.v_pcti_none)
        , T.kvd_ "Coffee Tycoon" "Loot: Coffee Beans: 500" (T.v_pcti_none)
        , T.kvd_ "Sea Wolf" "React level 10 in any type of sailing" (T.v_pcti_none)
        , T.kvd_ "Personal Museum" "Collect all artifacts" (T.v_pcti_none)
        , T.kvd_ "Best Farmer" "Maximum Level: Farming" (T.v_pcti +20)
        , T.kvd_ "The Hen That Laid the Golden Eggs" "Craft: Egg: 500" (T.v_pcti_none)
        , T.kvd_ "Chef" "Maximum Level: Cooking" (T.v_pcti +7)
        , T.kvd_ "Spicy Odyssey" "???" (T.v_none)
        , T.kvd_ "Silk Road" "???" (T.v_none)
        , T.kvd_secret
        , T.kvd_ "Steamobile" "Craft: Steamobile" (T.v_none)
        , T.kvd_ "Technological Progress" "Learn 20 technologies" (T.v_pcti_none)
        , T.kvd_ "Dungeons & Nonograms" "Complete level 5C of the dungeon" (T.v_none)
        , T.kvd_ "We Got a Champion" "Level up a hero to level 21" (T.v_none)
        , T.kvd_ "Valiant Knight" "Valor: 10000" (T.v_pcti_none)
        , T.kvd_ "Champions of the World" "Level up three heroes to level 21" (T.v_none)
        , T.kvd_ "Perfect Company" "Have a hero with 3 violet familiars." (T.v_none)
        , T.kvd_secret
        , T.kvd_secret
        , T.kvd_secret
        , T.kvd_ "Golden Hands" "Collect all craftifacts" (T.v_none)
        , T.kvd_secret
        , T.kvd_secret
        , T.kvd_secret
        ]

    )