let T = ./Types.dhall

in
    T.collapseAt
        { id = "doom-dark-ages-manual"
        , name = "Doom: Dark Ages"
        , platform = T.Platform.Playstation5
        , playtime = T.Playtime.MoreThan { hrs = +60, min = +0, sec = +0 }
        }
        { day = +18, mon = +12, year = +2025 }

    ( T.groupStats "Statistics" [ "00-stats" ]
        [ T.kv_ "Completion" (T.v_pcti +100)
        , T.kv_ "Chapters" (T.v_pi { done = +22, total = +22 })
        , T.kv_ "Trophies" (T.v_pi { done = +26, total = +29 }) // T.inj/self [ "00-stats", "00-trophies" ] -- 5 Silver, 10 Bronze
        , T.kv_ "Nickname" (T.v_t "elektrokilka")
        , T.kv_ "Difficulty" (T.v_t "Hurt me plenty")
        ]

    # T.group "Trophies" [ "00-stats", "00-trophies" ]
        [ T.kv_ "Bronze" (T.v_i +11)
        , T.kv_ "Silver" (T.v_i +12)
        , T.kv_ "Gold" (T.v_i +3)
        ]

    # T.group "Chapters" [ "01-chapters" ]
        [ T.kv_ "Chapter 01: Village of Khalim" (T.v_pcti +100) // T.inj/self [ "01-chapters", "00-village-of-khalim" ]
        , T.kv_ "Chapter 02: Hebeth" (T.v_pcti +97) // T.inj/self [ "01-chapters", "01-hebeth" ]
        , T.kv_ "Chapter 03: Barrier Core" (T.v_pcti +100) // T.inj/self [ "01-chapters", "02-barrier-core" ]
        , T.kv_ "Chapter 04: Sentinel Barracks" (T.v_pcti +100) // T.inj/self [ "01-chapters", "03-sentinel-barracks" ]
        , T.kv_ "Chapter 05: The Holy City of Aratum" (T.v_pcti +99) // T.inj/self [ "01-chapters", "04-holy-city-of-aratum" ]
        , T.kv_ "Chapter 06: The Siege - Part 1" (T.v_pcti +100) // T.inj/self [ "01-chapters", "05-siege-part-1" ]
        , T.kv_ "Chapter 07: The Siege - Part 2" (T.v_pcti +100) // T.inj/self [ "01-chapters", "06-siege-part-2" ]
        , T.kv_ "Chapter 08: Abyssal Forest" (T.v_pcti +100) // T.inj/self [ "01-chapters", "07-abyssal-forest" ]
        , T.kv_ "Chapter 09: Ancestral Forge" (T.v_pcti +100) // T.inj/self [ "01-chapters", "08-ancestral-forge" ]
        , T.kv_ "Chapter 10: The Forsaken Plains" (T.v_pcti +100) // T.inj/self [ "01-chapters", "09-forsaken-plains" ]
        , T.kv_ "Chapter 11: Hellbreaker" (T.v_pcti +100) // T.inj/self [ "01-chapters", "10-hellbreaker" ]
        , T.kv_ "Chapter 12: Sentinel Command Station" (T.v_pcti +100) // T.inj/self [ "01-chapters", "11-sentinel-command-station" ]
        , T.kv_ "Chapter 13: From Beyond" (T.v_pcti +72) // T.inj/self [ "01-chapters", "12-from-beyond" ]
        , T.kv_ "Chapter 14: Spire of Nerathul" (T.v_pcti +99) // T.inj/self [ "01-chapters", "13-spire-of-nerathul" ]
        , T.kv_ "Chapter 15: City of Ry'uul" (T.v_pcti +99) // T.inj/self [ "01-chapters", "14-city-of-ryuul" ]
        , T.kv_ "Chapter 16: The Kar'Thul Marshes" (T.v_pcti +99) // T.inj/self [ "01-chapters", "15-the-kar-thul-marshes" ]
        , T.kv_ "Chapter 17: Temple of Lomarith" (T.v_pcti +100) // T.inj/self [ "01-chapters", "16-temple-of-lomarith" ]
        , T.kv_ "Chapter 18: Belly of the Beast" (T.v_pcti +100) // T.inj/self [ "01-chapters", "17-belly-of-the-beast" ]
        , T.kv_ "Chapter 19: Harbor of Souls" (T.v_pcti +100) // T.inj/self [ "01-chapters", "18-harbour-of-souls" ]
        , T.kv_ "Chapter 20: Resurresction" (T.v_pcti +99) // T.inj/self [ "01-chapters", "19-resurresction" ]
        , T.kv_ "Chapter 21: Final Battle" (T.v_pcti +100) // T.inj/self [ "01-chapters", "20-final-battle" ]
        , T.kv_ "Chapter 22: Reconing" (T.v_pcti +99) // T.inj/self [ "01-chapters", "21-reconing" ]
        ]

    # T.group "Chapter 01: Village of Khalim" [ "01-chapters", "00-village-of-khalim" ]
        [ T.kv_ "Secrets" (T.v_pi { done = +6, total = +6 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Chapter 02: Hebeth" [ "01-chapters", "01-hebeth" ]
        [ T.kv_ "Gold" (T.v_pi { done = +202, total = +210 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +8, total = +9 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group_ "Chapter 03: Barrier Core" [ "01-chapters", "02-barrier-core" ]

    # T.group "Chapter 04: Sentinel Barracks" [ "01-chapters", "03-sentinel-barracks" ]
        [ T.kv_ "Gold" (T.v_pi { done = +212, total = +212 })
        , T.kv_ "Ruby" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +6, total = +6 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : RIP AND TEAR" (T.v_pi { done = +40, total = +40 })
        , T.kv_ "Challenge 2 : PLUNDERER" (T.v_pi { done = +150, total = +150 })
        , T.kv_ "Challenge 3 : PRIZE FIGHTER" (T.v_pi { done = +3, total = +3 })
        ]

    # T.group "Chapter 05: The Holy City of Aratum" [ "01-chapters", "04-holy-city-of-aratum" ]
        [ T.kv_ "Gold" (T.v_pi { done = +240, total = +240 })
        , T.kv_ "Ruby" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +9, total = +9 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : COUNTERCULT" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 2 : LAYOVER" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Challenge 3 : CAT AND MOUSE" (T.v_pi { done = +0, total = +3 })
        ]

    # T.group "Chapter 06: Siege - Part 1" [ "01-chapters", "05-siege-part-1" ]
        [ T.kv_ "Gold" (T.v_pi { done = +513, total = +513 })
        , T.kv_ "Ruby" (T.v_pi { done = +4, total = +4 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +11, total = +11 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : IMMOVABLE OBJECT" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Challenge 2 : UNSTOPPABLE FORCE" (T.v_pi { done = +25, total = +25 })
        , T.kv_ "Challenge 3 : SIEGE BREAKER" (T.v_pi { done = +4, total = +4 })
        ]

    # T.group "Chapter 07: Siege - Part 2" [ "01-chapters", "06-siege-part-2" ]
        [ T.kv_ "Gold" (T.v_pi { done = +183, total = +183 })
        , T.kv_ "Ruby" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +9, total = +9 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : ROUNDHOUSE" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Challenge 2 : EXTERMINATOR" (T.v_pi { done = +5, total = +5 }) -- // (T.inj/det "You earned Sleepwalker by completing 200 lessons after 10:00pm")
        ]

    # T.group "Chapter 08: Abyssal Forest" [ "01-chapters", "07-abyssal-forest" ]
        [ T.kv_ "Gold" (T.v_pi { done = +250, total = +250 })
        , T.kv_ "Ruby" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +11, total = +11 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : GHOST BUSTER" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 2 : KNOCKOUT" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Challenge 3 : BONE COLLECTOR" (T.v_pi { done = +100, total = +100 })
        ]

    # T.group "Chapter 09: Ancestral Forge" [ "01-chapters", "08-ancestral-forge" ]
        [ T.kv_ "Gold" (T.v_pi { done = +230, total = +230 })
        , T.kv_ "Ruby" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +9, total = +9 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : NOTHING BUT NADE" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Challenge 2 : SAWED OFF" (T.v_pi { done = +20, total = +20 })
        , T.kv_ "Challenge 3 : SEVEN SECRETS" (T.v_pi { done = +7, total = +7 })
        ]

    # T.group "Chapter 10: The Forsaken Plains" [ "01-chapters", "09-forsaken-plains" ]
        [ T.kv_ "Gold" (T.v_pi { done = +230, total = +230 })
        , T.kv_ "Ruby" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : ROCKET MAN" (T.v_pi { done = +2, total = +5 })
        , T.kv_ "Challenge 2 : EXTERMINATOR" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Challenge 3 : FAULT LINE" (T.v_pi { done = +25, total = +25 })
        ]

    # T.group "Chapter 11: Hellbreaker" [ "01-chapters", "10-hellbreaker" ]
        [ T.kv_ "Gold" (T.v_pi { done = +85, total = +85 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : ELUSIVE" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Challenge 2 : DISPERSION" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Challenge 3 : CRUSHINATOR" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Chapter 12: Sentinel Command Station" [ "01-chapters", "11-sentinel-command-station" ]
        [ T.kv_ "Gold" (T.v_pi { done = +177, total = +177 })
        , T.kv_ "Ruby" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +9, total = +9 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : PUNCHING BAG" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Challenge 2 : WAIT FOR IT" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Challenge 3 : SKELETON KEY" (T.v_pi { done = +2, total = +2 })
        ]

    # T.group "Chapter 13: From Beyond" [ "01-chapters", "12-from-beyond" ]
        [ T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : DEFT DRAGON" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Challenge 2 : VETERAN PURSUER" (T.v_pi { done = +1, total = +2 })
        ]

    # T.group "Chapter 14: Spire of Nerathul" [ "01-chapters", "13-spire-of-nerathul" ]
        [ T.kv_ "Gold" (T.v_pi { done = +359, total = +359 })
        , T.kv_ "Ruby" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +11, total = +11 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : HEART HARVESTER" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Challenge 2 : MASTER BLASTER" (T.v_pi { done = +0, total = +250 })
        , T.kv_ "Challenge 3 : EXPERT STALKER" (T.v_pi { done = +0, total = +2 })
        ]

    # T.group "Chapter 15: City of Ry'uul" [ "01-chapters", "14-city-of-ryuul" ]
        [ T.kv_ "Gold" (T.v_pi { done = +182, total = +182 })
        , T.kv_ "Ruby" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : HEADS UP" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 2 : FACE CRACKER" (T.v_pi { done = +25, total = +25 })
        , T.kv_ "Challenge 3 : TORRENT" (T.v_pi { done = +0, total = +3 })
        ]

    # T.group "Chapter 16: The Kar'Thul Marshes" [ "01-chapters", "15-the-kar-thul-marshes" ]
        [ T.kv_ "Gold" (T.v_pi { done = +188, total = +188 })
        , T.kv_ "Ruby" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : HUNTER" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Challenge 2 : ROUNDUP" (T.v_pi { done = +0, total = +20 })
        ]

    # T.group "Chapter 17: Temple of Lomarith" [ "01-chapters", "16-temple-of-lomarith" ]
        [ T.kv_ "Gold" (T.v_pi { done = +194, total = +194 })
        , T.kv_ "Ruby" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Secrets" (T.v_pi { done = +9, total = +9 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Skins" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : SWASHBUCKLER" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 2 : WATER LOGGED" (T.v_pi { done = +3, total = +3 })
        ]

    # T.group "Chapter 18: Belly of the Beast" [ "01-chapters", "17-belly-of-the-beast" ]
        [ T.kv_ "Gold" (T.v_pi { done = +153, total = +153 })
        , T.kv_ "Ruby" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +6, total = +6 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : SWALLOWED WHOLE" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 2 : TOE-TO-TOE" (T.v_pi { done = +10, total = +10 })
        ]

    # T.group "Chapter 19: Harbor of Souls" [ "01-chapters", "18-harbour-of-souls" ]
        [ T.kv_ "Gold" (T.v_pi { done = +294, total = +294 })
        , T.kv_ "Ruby" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +10, total = +10 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Challenge 1 : BERSERKER" (T.v_pi { done = +25, total = +25 })
        , T.kv_ "Challenge 2 : DRAGON HOARD" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 3 : EXECUTIONER" (T.v_pi { done = +21, total = +25 })
        ]

    # T.group "Chapter 20: Resurrection" [ "01-chapters", "19-resurrection" ]
        [ T.kv_ "Gold" (T.v_pi { done = +392, total = +392 })
        , T.kv_ "Ruby" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Health" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Armor" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +12, total = +12 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Collectible Toys" (T.v_pi { done =+2, total = +2 })
        , T.kv_ "Skins" (T.v_pi { done =+1, total = +1 })
        , T.kv_ "Challenge 1 : CANNON CRUSHER" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 2 : EXTREME PREJUDICE" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Challenge 2 : MASTER HUNTER" (T.v_pi { done = +0, total = +1 })
        ]

    # T.group "Chapter 21: Final Battle" [ "01-chapters", "20-final-battle" ]
        ( [] : T.Group/Empty )

    # T.group "Chapter 22: Reconing" [ "01-chapters", "21-reconing" ]
        [ T.kv_ "Gold" (T.v_pi { done = +335, total = +335 })
        , T.kv_ "Ruby" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Wraithstone" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Demonic Essence - Ammo" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Secrets" (T.v_pi { done = +11, total = +11 })
        , T.kv_ "Codex Entries" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Collectible Toys" (T.v_pi { done =+2, total = +2 })
        , T.kv_ "Challenge 1 : DEATH CLOCK" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Challenge 2 : THY FLESH CONSUMED" (T.v_pi { done = +25, total = +25 })
        ]

    # T.group "Campaign Milestones" [ "02-campaign-milestones" ]

        -- (T.v_pi { done = +1, total = +1 }) -> (T.v_pcti_done)
        -- (T.v_pi { done = +0, total = +1 }) -> (T.v_pcti_none)

        [ T.kv_ "A Dark Beginning [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Village of Khali" -- Finish Chapter 1
        , T.kv_ "Red Planet Achievement" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Hebeth" -- Finish Chapter 2.
        , T.kv_ "Supersized Brawl [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Barrier Core" -- Finish Chapter 3
        , T.kv_ "Onward and Upward" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Sentinel Barracks" -- Finish Chapter 4.
        , T.kv_ "Bringing the House Down [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: The Holy City of Aratum" -- Finish Chapter 5.
        , T.kv_ "Open War" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Siege - Part 1" -- Finish Chapter 6.
        , T.kv_ "Long Live the King" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Siege - Part 2" -- Finish Chapter 7.
        , T.kv_ "Overgrown" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Abyssal Forest" -- Finish Chapter 8.
        , T.kv_ "Heart Surgery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Ancestral Forge" -- Finish Chapter 9.
        , T.kv_ "I Have the Power" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: The Forsaken Plains" -- Finish Chapter 10.
        , T.kv_ "Knock Knock" (T.v_pi { done = +1, total = +1 })  // T.inj/det "Complete Chapter: Hellbreaker" -- Finish Chapter 11.
        , T.kv_ "Boarding Party" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Sentinel Command Station" -- Finish Chapter 12.
        , T.kv_ "Through The Rift" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: From Beyond" -- Finish Chapter 13.
        , T.kv_ "Jailbreak [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Spire of Nerathul" -- Finish Chapter 14.
        , T.kv_ "Into the Unknown" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: City of Ry'uul" -- Finish Chapter 15.
        , T.kv_ "Keymaker" (T.v_pi { done = +1, total = +1 })// T.inj/det "Complete Chapter: The Kar'Thul Marshes" -- Finish Chapter 16.
        , T.kv_ "Dark Waters" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Temple of Lomarith" -- Finish Chapter 17.
        , T.kv_ "Ritual Sacrifice" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Belly of the Beast" -- Finish Chapter 18.
        , T.kv_ "Too Angry to Die [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Harbor of Souls" -- Finish Chapter 19.
        , T.kv_ "Argent Return [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete Chapter: Resurrection" -- Finish Chapter 20.
        , T.kv_ "The Only Thing They Fear [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Defeat the Old One and Enhanced Ahzrak." -- Finish Chapter 21.
        , T.kv_ "Game Complete [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete the Campaign on any difficulty." -- Finish Chapter 22.
        , T.kv_ "Vagary Down! [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Kill the Vagary Champion." -- Finish Chapter 4.
        , T.kv_ "Agaddon Champion Down! [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Kill an Agaddon Hunter." -- Finish Chapter 8
        , T.kv_ "Komodo Champion Down! [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Kill a Komodo Demon." -- Finish Chapter 14.
        , T.kv_ "Challenge Accepted" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete all Mission Challenges in Holy City" -- Complete the Countercult, Layover, and Cat and Mouse challenges.
        , T.kv_ "Challenge Completed" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Complete all Mission Challenges in the campaign." -- -- Complete all 49 mission challenges. These will appear as optional objectives during a mission.
        , T.kv_ "Custom Trial" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Start a campaign with at least one Custom Difficulty setting modified and complete chapter: Village of Khalim." -- Tweak any setting on any difficulty and complete the first Chapter.
        , T.kv_ "Pandemonium Complete" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Beat the campaign on Pandemonium difficulty." -- Select Pandemonium as the difficulty at the start of your campaign.
        -- Rewards: Reverent Slayer Skin
        , T.kv_ "Ultra-Nightmare Complete" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Beat the campaign on Ultra-Nightmare difficulty." -- Select Ultra-Nightmare as the difficulty at the start of your campaign.
        -- Rewards: Conqueror Slayer Skin
        , T.kv_ "Cultist Purge" (T.v_pi { done = +9, total = +9 }) // T.inj/det "Kill all of the Cultists in the Village of Khalim" -- Kill all the cultists in the room where you get the Blue Key.
        , T.kv_ "Bloody Sandbox" (T.v_pi { done = +3, total = +3 }) // T.inj/det "Complete a Ripatorium encounter on each of the three arenas."
        ]

    # T.group "Progression Milestones" [ "03-progression-milestones" ]

        [ T.kv_ "Upgraded [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire your first weapon upgrade." -- Buy your first weapon upgrade in Chapter 2, we recommend the Combat Shotgun's “Incendiary” upgrade for 50 Gold.
        , T.kv_ "Spared No Expense" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Fully upgrade a weapon." -- Gather enough resources and upgrade weapons in a Sentinel Shrine.
        , T.kv_ "Combat Shotgun Mastery" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Master the Combat Shotgun." -- Fully upgrade your Combat Shotgun and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Super Shotgun Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Super Shotgun." -- Fully upgrade yourSuper Shotgun and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Shredder Mastery" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Master the Shredder." -- Fully upgrade your Shredder and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Impaler Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Impaler." -- Fully upgrade your Impaler and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Accelerator Mastery" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Master the Accelerator." -- Fully upgrade your Accelerator and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Cycler Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Cycler." -- Fully upgrade your Cycler and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Pulverizer Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Pulverizer." -- Fully upgrade your Pulverizer and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Ravager Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Ravager." -- Fully upgrade your Ravager and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Chainshot Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Chainshot." -- Fully upgrade your Chainshot and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Grenade Launcher Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Grenadeade Launcher." -- Fully upgrade your Grenade Launcher and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Rocket Launcher Mastery" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Master the Rocket Launcher." -- Fully upgrade your Rocket Launcher and complete its Mastery Challenge.
        -- Rewards: Conqueror Weapon Skin
        , T.kv_ "Fully Loaded [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Complete the Mastery Challenge for a single weapon." -- Purchase all upgrades for a single weapon then complete its corresponding challenge. We recommend the Combat Shotgun as the easiest one to accomplish.
        , T.kv_ "Gunpletionist [Trophy]" (T.v_pi { done = +0, total = +1 }) // T.inj/det "Complete the Mastery Challenge of all weapons." -- Purchase all available upgrades for all the weapons and complete their Mastery Challenges.
        , T.kv_ "Gimme That [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire the Ballistic Force Crossbow." -- Finish Chapter 14.
        , T.kv_ "Shield Adept [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire all shield upgrades under the base category." -- Purchase all the base shield upgrades in a Sentinel Shrine.
        , T.kv_ "Ancestral Blessing [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire your first Shield Rune." -- Finish Chapter 9.
        , T.kv_ "Powerful Investment [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire all upgrades for a single Shield Rune." -- Purchase all the upgrades for a single Shield Rune in a Sentinel Shrine.
        , T.kv_ "Melee Expert [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire all melee weapon upgrades." -- Purchase all the melee upgrades in a Sentinel Shrine.
        , T.kv_ "Berserker [Trophy]" (T.v_pi { done = +3, total = +3 }) // T.inj/det "Acquire all Shield Base, Shield Rune, and Melee Weapon upgrades." -- Purchase all the melee upgrades in a Sentinel Shrine.
        , T.kv_ "Essential Upgrade [Trophy]" (T.v_pi { done = +1, total = +1 }) // T.inj/det "Acquire your first Demonic Essence upgrade." -- Defeat the Leader miniboss at the end of Chapter 2.
        , T.kv_ "Essential Ammo [Trophy]" (T.v_pi { done = +12, total = +12 }) // T.inj/det "Acquire all Demonic Essence ammo upgrades." -- Find and defeat the 12 Leaders with the Demonic Ammo essence icon in Chapters 5, 6, 9, 10, 13, 14, 16, 17, 19, 20, and 22. There are two leaders in Chapter 17.
        , T.kv_ "Essential Armor [Trophy]" (T.v_pi { done = +10, total = +10 }) // T.inj/det "Acquire all Demonic Essence armor upgrades." -- Find and defeat the 10 Leaders with the Demonic Armor essence icon in Chapters 4, 6, 8, 10, 11, 13, 15, 16, 18, and 20.
        , T.kv_ "Essential Health [Trophy]" (T.v_pi { done = +10, total = +10 }) // T.inj/det "Acquire all Demonic Essence Health upgrades." -- Find and defeat the 10 Leaders with the Demonic Health essence icon in Chapters 2, 5, 7, 9, 10, 12, 14, 15, 19, 20.
        , T.kv_ "Essentially Unstoppable [Trophy]" (T.v_pi { done = +32, total = +32 }) // T.inj/det "Acquire all Demonic Essence upgrades." -- Defeat all 32 Leader demons found throughout the campaign.
        , T.kv_ "Toy Collector [Trophy]" (T.v_pi { done = +24, total = +24 }) // T.inj/det "Acquire all Collectible demon toys." -- Find all 24 Collectible toys.
        , T.kv_ "Lore Nerd [Trophy]" (T.v_pi { done = +26, total = +26 }) // T.inj/det "Acquire all Codex Lore pages." -- Find all 26 Codex Lore pages.
        ]

    # T.group "Weapon Mastery Challenges" [ "03-weapon-mastery" ]

        [ T.kv_ "Grenade Launcher Mastery Challenge: CLUSTERFUN" (T.v_pi { done = +100, total = +100 }) -- at Chapter 17
        , T.kv_ "Rocket Launcher Mastery Challenge: ENGORGE" (T.v_pi { done = +50, total = +50 }) -- at Chapter 18
        , T.kv_ "Cycler Mastery Challenge: THERMAL RUNAWAY" (T.v_pi { done = +500, total = +500 }) -- at Chapter 15
        , T.kv_ "Shredder Mastery Challenge: BURSTER" (T.v_pi { done = +0, total = +100 }) -- at Chapter 19
        , T.kv_ "Impaler Mastery Challenge: ACUPUNCTURE" (T.v_pi { done = +100, total = +100 }) -- at chapter 09
        , T.kv_ "Reaver Mastery Challenge: REVERBERATION" (T.v_pi { done = +100, total = +100 }) -- at Chapter 15
        , T.kv_ "Shotgun Mastery Challenge: BARREL STUFFER" (T.v_pi { done = +0, total = +150 }) -- at Chapter 22
        , T.kv_ "Super Shotgun Mastery Challenge: SUPER BARREL STUFFER" (T.v_pi { done = +50, total = +50 }) -- at Chapter 11
        , T.kv_ "Ravager Mastery Challenge: GATHERER" (T.v_pi { done = +100, total = +100 }) -- at chapter 09
        , T.kv_ "Pulverizer Mastery Challenge: HUNTER" (T.v_pi { done = +250, total = +250 }) -- at Chapter 14
        ]

    -- TODO: Weapon upgrades stats

    )