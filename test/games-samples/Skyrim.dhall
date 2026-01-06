let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "elder-scrolls-5-skyrim"
        , name = "Elder Scrolls V: Skyrim"
        , platform = GT.Platform.Switch
        , playtime = GT.Playtime.MoreThan { hrs = +65, min = +0, sec = +0 }
        }
        { day = +12, mon = +9, year = +2025 } (

    T.groupStats "Hero" [ "00-hero" ]
        [ T.kv_ "Name" (T.v_t "Ulrich Wilfred")
        , T.kv_ "Race" (T.v_t "Dark Elf")
        , T.kv_ "Level" (T.v_i +21)
        , T.kv_ "Magicka" (T.v_pi { done = +120, total = +120 })
        , T.kv_ "Health" (T.v_pi { done = +290, total = +290 })
        , T.kv_ "Stamina" (T.v_pi { done = +115, total = +115 })
        , T.kv_ "Current Epoch" (T.v_t "Loredas, 11:29PM, 20th of Heartfire, 4E 201")
        ]

    # T.group "Hero: Skills" [ "00-hero", "00-skills" ]

        [ T.kv_ "Speech"      (T.v_pi { done = +29, total = +100 })
        , T.kv_ "Alchemy"     (T.v_pi { done = +34, total = +100 }) -- 2/5
        , T.kv_ "Illusion"    (T.v_pi { done = +21, total = +100 })
        , T.kv_ "Conjuration" (T.v_pi { done = +16, total = +100 })
        , T.kv_ "Destruction" (T.v_pi { done = +27, total = +100 })
        , T.kv_ "Restoration" (T.v_pi { done = +18, total = +100 })
        , T.kv_ "Alteration"  (T.v_pi { done = +22, total = +100 })
        , T.kv_ "Enchanting"  (T.v_pi { done = +16, total = +100 })
        , T.kv_ "Smithing"    (T.v_pi { done = +18, total = +100 })
        , T.kv_ "Heavy Armor" (T.v_pi { done = +51, total = +100 })
        , T.kv_ "Block"       (T.v_pi { done = +21, total = +100 })
        , T.kv_ "Two-Handed"  (T.v_pi { done = +62, total = +100 })
        , T.kv_ "One-Handed"  (T.v_pi { done = +26, total = +100 })
        , T.kv_ "Archery"     (T.v_pi { done = +49, total = +100 })
        , T.kv_ "Light Armor" (T.v_pi { done = +35, total = +100 })
        , T.kv_ "Sneak"       (T.v_pi { done = +26, total = +100 })
        , T.kv_ "Lockpicking" (T.v_pi { done = +49, total = +100 })
        , T.kv_ "Pickpocket"  (T.v_pi { done = +17, total = +100 })
        ]

    # T.group_ "Stats" [ "01-stats" ]

    # T.groupStats "Stats: General" [ "01-stats", "00-general" ]

        [ T.kv_ "Locations Discovered" (T.v_i +118)
        , T.kv_ "Dungeons Cleared" (T.v_i +31)
        , T.kv_ "Days Passed" (T.v_i +34)
        , T.kv_ "Hours Slept" (T.v_i +65)
        , T.kv_ "Hours Waiting" (T.v_i +30)
        , T.kv_ "Standing Stones Found" (T.v_i +5)
        , T.kv_ "Gold Found" (T.v_i +18325)
        , T.kv_ "Most Gold Carried" (T.v_i +6196)
        , T.kv_ "Chests Looted" (T.v_i +728)
        , T.kv_ "Skill Increases" (T.v_i +232)
        , T.kv_ "Skill Books Read" (T.v_i +25)
        , T.kv_ "Food Eaten" (T.v_i +1267)
        , T.kv_ "Training Sessions" (T.v_i +15)
        , T.kv_ "Books Read" (T.v_i +141)
        , T.kv_ "Horses Owned" (T.v_i +2)
        , T.kv_ "Houses Owned" (T.v_i +1)
        , T.kv_ "Stores Invested In" (T.v_i +0)
        , T.kv_ "Barters" (T.v_i +117)
        , T.kv_ "Persuations" (T.v_i +8)
        , T.kv_ "Bribes" (T.v_i +2)
        , T.kv_ "Intimidations" (T.v_i +2)
        , T.kv_ "Diseases Contracted" (T.v_i +5)
        ]

    # T.groupStats "Stats: Quests" [ "01-stats", "01-quests" ]

        [ T.kv_ "Quests Completed" (T.v_i +11)
        , T.kv_ "Misc Objectives Completed" (T.v_i +51)
        , T.kv_ "Main Quests Completed" (T.v_i +5)
        , T.kv_ "Side Quests Completed" (T.v_i +6)
        , T.kv_ "The Companions Quests Completed" (T.v_i +0)
        , T.kv_ "College of Winterhold Quests Completed" (T.v_i +0)
        , T.kv_ "Thieves' Guild Quests Completed" (T.v_i +1)
        , T.kv_ "The Dark Brotherhood Quests Completed" (T.v_i +1)
        , T.kv_ "Civil War Quests Completed" (T.v_i +0)
        , T.kv_ "Deadric Quests Completed" (T.v_i +0)
        , T.kv_ "Questlines Completed" (T.v_i +0)
        ]

    # T.groupStats "Stats: Combat" [ "01-stats", "02-combat" ]

        [ T.kv_ "People Killed" (T.v_i +265)
        , T.kv_ "Animals Killed" (T.v_i +211)
        , T.kv_ "Creatures Killed" (T.v_i +14)
        , T.kv_ "Undead Killed" (T.v_i +122)
        , T.kv_ "Daedra Killed" (T.v_i +10)
        , T.kv_ "Automatons Killed" (T.v_i +0)
        , T.kv_ "Favorite Weapon" (T.v_t "Steel Battleaxe of Sparks")
        , T.kv_ "Critical Strikes" (T.v_i +48)
        , T.kv_ "Sneak Attacks" (T.v_i +7)
        , T.kv_ "Backstabs" (T.v_i +0)
        , T.kv_ "Weapons Disarmed" (T.v_i +0)
        , T.kv_ "Brawls Won" (T.v_i +1)
        , T.kv_ "Bunnies Slaughtered" (T.v_i +0)
        ]

    # T.groupStats "Stats: Magic" [ "01-stats", "03-magic" ]

        [ T.kv_ "Spells Learned" (T.v_i +12)
        , T.kv_ "Favorite Spell" (T.v_t "Healing")
        , T.kv_ "Favorite School" (T.v_t "Restoration")
        , T.kv_ "Dragon Souls Collected" (T.v_i +5)
        , T.kv_ "Words of Power Collected" (T.v_i +4)
        , T.kv_ "Words of Power Unlocked" (T.v_i +4)
        , T.kv_ "Shouts Learned" (T.v_i +3)
        , T.kv_ "Shouts Unlocked" (T.v_i +3)
        , T.kv_ "Shouts Mastered" (T.v_i +0)
        , T.kv_ "Times Shouted" (T.v_i +33)
        , T.kv_ "Favorite Shout" (T.v_t "Unrelenting Force")
        ]

    # T.groupStats "Stats: Crafting" [ "01-stats", "04-crafting" ]

        [ T.kv_ "Soul Gems Used" (T.v_i +2)
        , T.kv_ "Souls Trapped" (T.v_i +0)
        , T.kv_ "Magic Items Made" (T.v_i +0)
        , T.kv_ "Weapons Improved" (T.v_i +4)
        , T.kv_ "Weapons Made" (T.v_i +0)
        , T.kv_ "Armor Improved" (T.v_i +7)
        , T.kv_ "Armor Made" (T.v_i +2)
        , T.kv_ "Potions Mixed" (T.v_i +409)
        , T.kv_ "Potions Used" (T.v_i +142)
        , T.kv_ "Poisons Mixed" (T.v_i +32)
        , T.kv_ "Poisons Used" (T.v_i +16)
        , T.kv_ "Ingridients Harvested" (T.v_i +553)
        , T.kv_ "Ingridients Eaten" (T.v_i +4)
        , T.kv_ "Nirnroots Found" (T.v_i +0)
        , T.kv_ "Wings Plucked" (T.v_i +6)
        ]

    # T.groupStats "Stats: Crime" [ "01-stats", "05-crime" ]

        [ T.kv_ "Total Lifetime Bounty" (T.v_i +15)
        , T.kv_ "Largest Bounty" (T.v_i +10)
        , T.kv_ "Locks Picked" (T.v_i +74)
        , T.kv_ "Pockets Picked" (T.v_i +1)
        , T.kv_ "Items Pickpocketed" (T.v_i +0)
        , T.kv_ "Times Jailed" (T.v_i +0)
        , T.kv_ "Days Jailed" (T.v_i +0)
        , T.kv_ "Fines Paid" (T.v_i +15)
        , T.kv_ "Jail Escapes" (T.v_i +0)
        , T.kv_ "Items Stolen" (T.v_i +5)
        , T.kv_ "Assaults" (T.v_i +14)
        , T.kv_ "Murders" (T.v_i +1)
        , T.kv_ "Horses Stolen" (T.v_i +0)
        , T.kv_ "Trespasses" (T.v_i +6)
        ]

    # T.group "Selected Quests" [ "02-quests" ]

        [ T.kv_ "Bring 10 fire salts to Balimund" (T.v_pi { done = +4, total = +10 })
        , T.kv_ "Find 2 flawless saphires for Madesi" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Find 3 Ice Wraith Teeth for Marise" (T.v_pi { done = +3, total = +5 })
        , T.kv_ "Find 20 Jazbay" (T.v_pi { done = +6, total = +20 })
        , T.kv_ "Gather 10 bear pelts for Temba Wide-Arms" (T.v_pi { done = +8, total = +10 })
        , T.kv_ "Gather 10 bear pelts for Temba Wide-Arms" (T.v_pi { done = +8, total = +10 })
        , T.kv_ "Join The Imperial Legion" (T.v_done)
        , T.kv_ "A Chance Arragement" (T.v_done)
        , T.kv_ "The Blessings of Nature" (T.v_done)
        , T.kv_ "Ancestral Worship" (T.v_done)
        , T.kv_ "The Way of the Voice" (T.v_done)
        , T.kv_ "Infiltration" (T.v_done)
        , T.kv_ "Delayed Burial" (T.v_done)
        , T.kv_ "Dragon Rising" (T.v_done)
        , T.kv_ "Bleak Falls Barrow" (T.v_done)
        , T.kv_ "The Golden Claw" (T.v_done)
        , T.kv_ "Before the Storm" (T.v_done)
        , T.kv_ "Unbound" (T.v_done)
        , T.kv_ "Rise in the East" (T.v_none)
        , T.kv_ "Innocence Lost" (T.v_none)
        , T.kv_ "Unfathomable Death" (T.v_none)
        , T.kv_ "Promised to Keep" (T.v_none)
        , T.kv_ "Taking Care of Business" (T.v_none)
        , T.kv_ "Dragonborn" (T.v_none)
        , T.kv_ "The Horn of Jurgen Windcaller" (T.v_none)
        , T.kv_ "The Break of Dawn" (T.v_none)
        , T.kv_ "Dawnguard" (T.v_none)
        , T.kv_ "In My Time of Need" (T.v_none)
        , T.kv_ "Forbidden Legend" (T.v_none)
        ]

    -- Installed Content: Dawnguard, Dragonborn, Fishing, Hearthfire, Rare Curios, Saints & Seducers, Survival Mode

    )
