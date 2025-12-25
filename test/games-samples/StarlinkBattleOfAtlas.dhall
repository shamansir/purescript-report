let T = ./Types.dhall

in
    T.collapseAt
        { id = "starlink-battle-of-atlas"
        , name = "Starlink: Battle of Atlas"
        , platform = T.Platform.Switch
        , playtime = T.Playtime.MoreThan { hrs = +45, min = +0, sec = +0 }
        }
        { day = +12, mon = +8, year = +2025 } (

    T.group "Hero" [ "00-hero" ]
        [ T.kv_ "Level" (T.v_i +32)
        , T.kv_ "Name" (T.v_t "Levi McGray")
        , T.kv_ "Skill points unspent" (T.v_i +11)
        ]

    # T.group "Skills" [ "00-hero", "00-skills" ]
        [ T.kv_ "Nice Try (Ability Skill)"
            (T.v_lvls
                { reached = +1
                , levels =
                    [ { gives = "Projectile Deflection" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Totally Psyched (Ability Skill)"
            (T.v_lvls
                { reached = +2
                , levels =
                    [ { gives = "+50% Damage" } // T.inj/no_date
                    , { gives = "+100% Damage" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Bad Influence (Mentor Skill)"
            (T.v_lvls
                { reached = +2
                , levels =
                    [ { gives = "All Pilots: +10% durability at critical health" } // T.inj/no_date
                    , { gives = "All Pilots: +20% durability at critical health" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Reckless (Ability Skill)"
            (T.v_lvls
                { reached = +0
                , levels =
                    [ { gives = "-25% energy consumption" } // T.inj/no_date
                    , { gives = "-50% energy consumpiton" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Can't Touch This (Stat Skill)"
            (T.v_lvls
                { reached = +3
                , levels =
                    [ { gives = "+10% durability while at critical health" } // T.inj/no_date
                    , { gives = "+20% durability while at critical health" } // T.inj/no_date
                    , { gives = "+30% durability while at critical health" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Walk it Off (Stat Skill)"
            (T.v_lvls
                { reached = +3
                , levels =
                    [ { gives = "+10% shield recharge speed" } // T.inj/no_date
                    , { gives = "+20% shield recharge speed" } // T.inj/no_date
                    , { gives = "+30% shield recharge speed" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Thrill-Seeker (Ability Skill)"
            (T.v_lvls
                { reached = +3
                , levels =
                    [ { gives = "+0.5s per kill" } // T.inj/no_date
                    , { gives = "+1s per kill" } // T.inj/no_date
                    , { gives = "+1.5s per kill" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Star Power (Style Skill)"
            (T.v_lvls
                { reached = +0
                , levels =
                    [ { gives = "+50 charge per Trick" } // T.inj/no_date
                    , { gives = "+100 charge per Trick" } // T.inj/no_date
                    , { gives = "+150 charge per Trick" } // T.inj/no_date
                    ]
                }
            )
        , T.kv_ "Gotta Go Fast (Style Skill)"
            (T.v_lvls
                { reached = +0
                , levels =
                    [ { gives = "-10% Boost Gauge Cost" } // T.inj/no_date
                    , { gives = "-20% Boost Gauge Cost" } // T.inj/no_date
                    , { gives = "-30% Boost Gauge Cost" } // T.inj/no_date
                    ]
                }
            )
        ]

    # T.group "Resources" [ "01-resources" ]
        [ T.kv_ "Electrum" (T.v_i +2863717)
        , T.kv_ "Nova" (T.v_i +53)
        , T.kv_ "Cogs" (T.v_i +842)
        , T.kv_ "Organics" (T.v_pi { done = +0, total = +99 })
        , T.kv_ "Metallics" (T.v_pi { done = +5, total = +99 })
        , T.kv_ "Cores" (T.v_pi { done = +1, total = +99 })
        , T.kv_ "Aliens" (T.v_i +2)
        ]

    # T.group "Collection" [ "02-collection" ]
        [ T.kv_ "Hangar" (T.v_pi { done = +26, total = +56 })
        , T.kv_ "Tactical Data" (T.v_pi { done = +42, total = +43 })
        , T.kv_ "Enemy Codex" (T.v_pi { done = +25, total = +27 })
        , T.kv_ "Alien Factions" (T.v_pi { done = +28, total = +36 })
        , T.kv_ "Atlas Discoveries" (T.v_pi { done = +80, total = +114 })
        , T.kv_ "Database" (T.v_pi { done = +20, total = +32 })
        , T.kv_ "Activities" (T.v_pi { done = +6, total = +31 })
        ]

    # T.group "Equinox Upgrades" [ "03-equinox" ]
        [ T.kv_ "Core Systems" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Modding Bay" (T.v_pi { done = +4, total = +4 })
        , T.kv_ "Mod Foundry" (T.v_pi { done = +4, total = +4 })
        , T.kv_ "Freight & Cargo" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Warden Research" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Expedition Wing" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Definiton Ops" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Outlaw Barracks" (T.v_pi { done = +1, total = +5 })
        ]

    # T.group "Hangar" [ "04-collection", "00-hangar" ]
        [ T.kv_ "Star Fox" (T.v_pi { done = +4, total = +8 })
        , T.kv_ "Pilots" (T.v_pi { done = +6, total = +14 })
        , T.kv_ "Ships" (T.v_pi { done = +4, total = +8 })
        , T.kv_ "Weapons" (T.v_pi { done = +12, total = +26 })
        ]

    # T.group "Tactical Data" [ "04-collection", "01-tactical" ]
        [ T.kv_ "Technology" (T.v_pi { done = +19, total = +20 })
        , T.kv_ "Weapon Effects" (T.v_pi { done = +16, total = +16 })
        , T.kv_ "Warden Relics" (T.v_pi { done = +7, total = +7 })
        ]

    # T.group "Enemy Codex" [ "04-collection", "02-enemy-codex" ]
        [ T.kv_ "Forgotten Legion" (T.v_pi { done = +23, total = +24 })
        , T.kv_ "Outlaws" (T.v_pi { done = +2, total = +3 })
        ]

    # T.group "Enemy Factions" [ "04-collection", "03-enemy-factions" ]
        [ T.kv_ "Forgotten Legion" (T.v_pi { done = +9, total = +10 })
        , T.kv_ "Expedition" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Prospectors" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Outlaws" (T.v_pi { done = +8, total = +15 })
        ]

    # T.group "Atlas Discoveries" [ "04-collection", "04-altas" ]
        [ T.kv_ "Space" (T.v_pi { done = +5, total = +10 })
        , T.kv_ "Kirite" (T.v_pi { done = +9, total = +15 })
        , T.kv_ "Haven" (T.v_pi { done = +11, total = +14 })
        , T.kv_ "Necrom" (T.v_pi { done = +10, total = +14 })
        , T.kv_ "Tundria" (T.v_pi { done = +11, total = +14 })
        , T.kv_ "Sonatus" (T.v_pi { done = +8, total = +13 })
        , T.kv_ "Vylus" (T.v_pi { done = +12, total = +14 })
        , T.kv_ "Ashar" (T.v_pi { done = +9, total = +15 })
        , T.kv_ "Crimson Moon" (T.v_pi { done = +5, total = +5 })
        ]

    # T.group "Database" [ "04-collection", "05-database" ]
        [ T.kv_ "Chronicles" (T.v_pi { done = +16, total = +16 })
        , T.kv_ "Dossiers" (T.v_pi { done = +2, total = +7 })
        , T.kv_ "Faction Stories" (T.v_pi { done = +2, total = +9 })
        ]

    # T.group "Activities" [ "04-collection", "06-activities" ]
        [ T.kv_ "Races" (T.v_pi { done = +3, total = +5 })
        , T.kv_ "Crimson Coliseum" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Trophies" (T.v_pi { done = +2, total = +21 })
        ]

    # T.group_ "Planets" [ "05-planets" ]

    -- Ashar

    # T.group "Ashar" [ "05-planets", "00-ashar" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 0.25)
        , T.kv_ "Discovered" (T.v_pi { done = +7, total = +11 })
        ]

    # T.group "Ashar: Completed" [ "05-planets", "00-ashar", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +7, total = +11 })
        , T.kv_ "Wrecks" (T.v_pi { done = +0, total = +13 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Ashar: Discovered" [ "05-planets", "00-ashar", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +2, total = +3 })
        , T.kv_ "Sites" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +2, total = +5 })
        ]

    -- Necrom

    # T.group "Necrom" [ "05-planets", "01-necrom" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 0.29)
        , T.kv_ "Discovered" (T.v_pi { done = +8, total = +11 })
        ]

    # T.group "Necrom: Completed" [ "05-planets", "01-necrom", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +4, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +4, total = +16 })
        , T.kv_ "Wrecks" (T.v_pi { done = +1, total = +13 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Necrom: Discovered" [ "05-planets", "01-necrom", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +2, total = +3 })
        , T.kv_ "Sites" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +3, total = +5 })
        ]

    -- Sonatus

    # T.group "Sonatus" [ "05-planets", "02-sonatus" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 0.64)
        , T.kv_ "Discovered" (T.v_pi { done = +6, total = +10 })
        ]

    # T.group "Sonatus: Completed" [ "05-planets", "02-sonatus", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +4, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +10, total = +14 })
        , T.kv_ "Wrecks" (T.v_pi { done = +10, total = +19 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Sonatus: Discovered" [ "05-planets", "02-sonatus", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Sites" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +2, total = +4 })
        ]

    -- Kirite

    # T.group "Kirite" [ "05-planets", "03-kirite" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 0.50)
        , T.kv_ "Discovered" (T.v_pi { done = +7, total = +12 })
        ]

    # T.group "Kirite: Completed" [ "05-planets", "03-kirite", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +4, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +12, total = +17 })
        , T.kv_ "Wrecks" (T.v_pi { done = +3, total = +17 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Kirite: Discovered" [ "05-planets", "03-kirite", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +3, total = +4 })
        , T.kv_ "Sites" (T.v_pi { done = +2, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +2, total = +5 })
        ]

    -- Haven

    # T.group "Haven" [ "05-planets", "04-haven" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 1.00)
        , T.kv_ "Discovered" (T.v_pi { done = +9, total = +11 })
        ]

    # T.group "Haven: Completed" [ "05-planets", "04-haven", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +15, total = +15 })
        , T.kv_ "Wrecks" (T.v_pi { done = +18, total = +18 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Haven: Discovered" [ "05-planets", "04-haven", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Sites" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +3, total = +5 })
        ]

    -- Vylus

    # T.group "Vylus" [ "05-planets", "05-vylus" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 0.86)
        , T.kv_ "Discovered" (T.v_pi { done = +10, total = +11 })
        ]

    # T.group "Vylus: Completed" [ "05-planets", "05-vylus", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +4, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +13, total = +15 })
        , T.kv_ "Wrecks" (T.v_pi { done = +14, total = +16 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Vylus: Discovered" [ "05-planets", "05-vylus", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Sites" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +4, total = +5 })
        ]

    -- Tundria

    # T.group "Tundria" [ "05-planets", "06-tundria" ]
        [ T.kv_ "Alliance Benefit" (T.v_pct 1.00)
        , T.kv_ "Completed" (T.v_pct 0.84)
        , T.kv_ "Discovered" (T.v_pi { done = +9, total = +11 })
        ]

    # T.group "Tundria: Completed" [ "05-planets", "06-tundria", "00-completed" ]
        [ T.kv_ "Spires" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Ruins" (T.v_pi { done = +14, total = +16 })
        , T.kv_ "Wrecks" (T.v_pi { done = +11, total = +15 })
        , T.kv_ "Wonders" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "Tundria: Discovered" [ "05-planets", "06-tundria", "01-discovered" ]
        [ T.kv_ "Species" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Sites" (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Samples" (T.v_pi { done = +3, total = +5 })
        ]

    -- Ships

    # T.group_ "Ship" [ "06-ship" ]

    # T.group "Zenith" [ "06-ship", "00-zenith" ]
        [ T.kv_ "Name" (T.v_t "Zenith")
        , T.kv_ "Level" (T.v_pi { done = +2, total = +5 })
        , T.kv_ "Energy" (T.v_pi { done = +49302, total = +82000 })
        ]

    # T.group "Body" [ "06-ship", "00-zenith", "00-body" ]
        [ T.kv_ "Speed" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Handling" (T.v_pi { done = +150, total = +300 })
        , T.kv_ "Defence" (T.v_pi { done = +255, total = +300 })
        , T.kv_ "Energy" (T.v_pi { done = +235, total = +300 })
        , T.kv_ "Weight" (T.v_t "Medium")
        ]

    # T.group "Right: Levitator" [ "06-ship", "00-zenith", "01-right" ]
        [ T.kv_ "Damage" (T.v_pi { done = +28, total = +300 })
        , T.kv_ "Range" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Fire Rate" (T.v_pi { done = +26, total = +300 })
        , T.kv_ "Energy Cost" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Status Effect" (T.v_t "Stasis Overload")
        ]

    # T.group "Left: Frost Barrage" [ "06-ship", "00-zenith", "02-left" ]
        [ T.kv_ "Damage" (T.v_pi { done = +26, total = +300 })
        , T.kv_ "Range" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Fire Rate" (T.v_pi { done = +36, total = +300 })
        , T.kv_ "Energy Cost" (T.v_pi { done = +46, total = +300 })
        , T.kv_ "Status Effect" (T.v_t "Frost")
        ]

    # T.group "Neptune" [ "06-ship", "01-neptune" ]
        [ T.kv_ "Name" (T.v_t "Neptune")
        , T.kv_ "Level" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Energy" (T.v_pi { done = +20537, total = +26000 })
        , T.kv_ "Down" (T.v_done)
        ]

    # T.group "Body" [ "06-ship", "01-neptune", "00-body" ]
        [ T.kv_ "Speed" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Handling" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Defence" (T.v_pi { done = +230, total = +300 })
        , T.kv_ "Energy" (T.v_pi { done = +125, total = +300 })
        , T.kv_ "Weight" (T.v_t "Ultra-Heavy")
        ]

    # T.group "Pulse" [ "06-ship", "02-pulse" ]
        [ T.kv_ "Name" (T.v_t "Pulse")
        , T.kv_ "Level" (T.v_pi { done = +5, total = +5 })
        , T.kv_ "Mastered" (T.v_done)
        ]

    # T.group "Body" [ "06-ship", "02-pulse", "00-body" ]
        [ T.kv_ "Speed" (T.v_pi { done = +175, total = +300 })
        , T.kv_ "Handling" (T.v_pi { done = +190, total = +300 })
        , T.kv_ "Defence" (T.v_pi { done = +180, total = +300 })
        , T.kv_ "Energy" (T.v_pi { done = +100, total = +300 })
        , T.kv_ "Weight" (T.v_t "Light")
        ]

    )