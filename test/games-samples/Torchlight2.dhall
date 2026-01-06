let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "torchlight-2"
        , name = "Torchlight II"
        , platform = GT.Platform.Switch
        , playtime = GT.Playtime.MoreThan { hrs = +10, min = +0, sec = +0 }
        }
        { day = +5, mon = +8, year = +2025 } (

    T.group "Hero" [ "00-hero" ]
        [ T.kv_ "nickname" (T.v_t "shamansir")
        , T.kv_ "level" (T.v_i +26)
        , T.kv_ "gold" (T.v_i +25114)
        , T.kv_ "fame" (T.v_pi { done = +16620, total = +18000 })
        , T.kv_ "title" (T.v_t "Well-known Berserker")
        ]
    # T.group "Overview" [ "01-overview" ]
        [ T.kv_ "Difficulcy" (T.v_t "Normal")
        , T.kv_ "Time Played" (T.v_time { hrs = +14, min = +8, sec = +24 })
        , T.kv_ "Average Time Per Level" (T.v_time { hrs = +0, min = +32, sec = +12 })
        , T.kv_ "Potions Used" (T.v_i +276)
        , T.kv_ "Deaths" (T.v_i +6)
        , T.kv_ "Gold Gathered" (T.v_i +68920)
        , T.kv_ "Steps Taken" (T.v_i +42786)
        , T.kv_ "Quests Completed" (T.v_i +38)
        , T.kv_ "Monsters Killed" (T.v_i +4752)
        , T.kv_ "Monsters Exploded" (T.v_i +783)
        , T.kv_ "Champions Killed" (T.v_i +79)
        , T.kv_ "Skills Used" (T.v_i +3223)
        , T.kv_ "Lootables Looted" (T.v_i +242)
        , T.kv_ "Traps Sprung" (T.v_i +58)
        , T.kv_ "Breakables Broken" (T.v_i +723)
        , T.kv_ "Portals Used" (T.v_i +40)
        , T.kv_ "Fish Caught" (T.v_i +27)
        , T.kv_ "Times Gambled" (T.v_i +0)
        , T.kv_ "Items Enchanted" (T.v_i +12)
        , T.kv_ "Items Transmuted" (T.v_i +0)
        , T.kv_ "Highest Damage Taken" (T.v_i +577)
        , T.kv_ "Highest Damage Dealt" (T.v_i +1460)
        ]
    # T.group_ "Total Stats" [ "02-total" ]
    # T.group "Strength" [ "02-total", "00-strength" ]
        [ T.kv_ "Base Strength" (T.v_i +55)
        , T.kv_ "Percent Strength Bonus" (T.v_pct_mes +0)
        , T.kv_ "Strength Bonus" (T.v_i +19)
        , T.kv_ "Total Strength" (T.v_i +74)
        ]
    # T.group "Dexterity" [ "02-total", "01-dexterity" ]
        [ T.kv_ "Base Dexterity" (T.v_i +54)
        , T.kv_ "Percent Dexterity Bonus" (T.v_pct_mes +0)
        , T.kv_ "Dexterity Bonus" (T.v_i +16)
        , T.kv_ "Total Dexterity" (T.v_i +70)
        ]
    # T.group "Focus" [ "02-total", "02-focus" ]
        [ T.kv_ "Base Focus" (T.v_i +18)
        , T.kv_ "Percent Focus Bonus" (T.v_pct_mes +0)
        , T.kv_ "Focus Bonus" (T.v_i +27)
        , T.kv_ "Total Focus" (T.v_i +45)
        ]
    # T.group "Vitality" [ "02-total", "03-vitality" ]
        [ T.kv_ "Base Vitality" (T.v_i +39)
        , T.kv_ "Percent Vitality Bonus" (T.v_pct_mes +0)
        , T.kv_ "Vitality Bonus" (T.v_i +0)
        , T.kv_ "Total Vitality" (T.v_i +38)
        ]
    # T.group "Attack" [ "03-attack" ]
        [ T.kv_ "Attack Speed Bonus" (T.v_pct_mesx +1 32.0)
        , T.kv_ "Cast Speed Bonus" (T.v_pct_mesx +1 60.0)
        , T.kv_ "Critical Chance" (T.v_pct_mes +16)
        , T.kv_ "Critical Damage Bonus" (T.v_pct_mesx +1 16.0)
        , T.kv_ "Fumble Chance" (T.v_pct_mes +21)
        , T.kv_ "Fumble Damage Penalty" (T.v_pctx -1 0.55)
        , T.kv_ "Dual Wielding Bonus" (T.v_pct_mesx +1 0.0)
        , T.kv_ "Execute Chance" (T.v_pct_mes +33)
        , T.kv_ "Physical Damage Bonus" (T.v_pct_mes +0)
        ]
    # T.group "Elemental Damage Bonuses" [ "03-attack", "00-elemental" ]
        [ T.kv_ "Fire" (T.v_pct_mes +0)
        , T.kv_ "Ice" (T.v_pct_mes +0)
        , T.kv_ "Electric" (T.v_pct_mes +6)
        , T.kv_ "Poison" (T.v_pct_mes +12)
        ]
    # T.group "Defence" [ "04-defence" ]
        [ T.kv_ "Physical Armor" (T.v_i +101)
        , T.kv_ "Physical Damage Reduction" (T.v_pct_mes +25)
        , T.kv_ "Block Chance" (T.v_pct_mes +0)
        , T.kv_ "Dodge Chance" (T.v_pct_mes +14)
        , T.kv_ "Missile Reflect Chance" (T.v_pct_mes +3)
        , T.kv_ "Missile Reflect DPS" (T.v_pct_mes +50)
        , T.kv_ "Resistance to Slowing Effects" (T.v_pct_mes +0)
        , T.kv_ "Resistance to Immbolozation" (T.v_pct_mes +0)
        ]
    # T.group "Elemental Armors" [ "04-defence", "00-armors" ]
        [ T.kv_ "Fire" (T.v_i +97)
        , T.kv_ "Ice" (T.v_i +28)
        , T.kv_ "Electric" (T.v_i +40)
        , T.kv_ "Poison" (T.v_i +28)
        ]
    # T.group "Elemental Damage Reductions" [ "04-defence", "01-damagered" ]
        [ T.kv_ "Fire" (T.v_pct_mes +25)
        , T.kv_ "Ice" (T.v_pct_mes +25)
        , T.kv_ "Electric" (T.v_pct_mes +25)
        , T.kv_ "Poison" (T.v_pct_mes +31)
        ]
    # T.group "Right Hand:" [ "05-right" ]
        [ T.kv_ "Damage Per Second" (T.v_i +295)
        , T.kv_ "Attack Speed" (T.v_mesd 0.36 "seconds")
        , T.kv_ "Physical Damage" (T.v_rng { from = +41, to = +59 })
        , T.kv_ "Range" (T.v_mesd 1.80 "meters")
        , T.kv_ "Damage Arc" (T.v_mes +0 "degrees")
        , T.kv_ "Splash Damage" (T.v_pct_mes +0)
        ]
    # T.group "Elemental Damages" [ "05-right", "00-elemental" ]
        [ T.kv_ "Fire" (T.v_i +0)
        , T.kv_ "Ice" (T.v_i +67)
        , T.kv_ "Electric" (T.v_i +0)
        , T.kv_ "Poison" (T.v_i +0)
        , T.kv_ "Critical Damage" (T.v_i +276)
        ]
    # T.group "Left Hand:" [ "06-left" ]
        [ T.kv_ "Damage Per Second" (T.v_i +295)
        , T.kv_ "Attack Speed" (T.v_mesd 0.36 "seconds")
        , T.kv_ "Physical Damage" (T.v_rng { from = +41, to = +59 })
        , T.kv_ "Range" (T.v_mesd 1.80 "meters")
        , T.kv_ "Damage Arc" (T.v_mes +0 "degrees")
        , T.kv_ "Splash Damage" (T.v_pct_mes +0)
        ]
    # T.group "Elemental Damages" [ "06-left", "00-elemental" ]
        [ T.kv_ "Fire" (T.v_i +0)
        , T.kv_ "Ice" (T.v_i +67)
        , T.kv_ "Electric" (T.v_i +0)
        , T.kv_ "Poison" (T.v_i +0)
        , T.kv_ "Critical Damage" (T.v_i +276)
        ]
    # T.group "Misc" [ "07-misc" ]
        [ T.kv_ "Health Regeneration" (T.v_per +0 "sec")
        , T.kv_ "Mana Regeneration" (T.v_perd 1.00 "sec")
        , T.kv_ "Base Mana Regeneration" (T.v_mesd 4.00 "%/sec")
        , T.kv_ "Run Speed" (T.v_f 10.50)
        , T.kv_ "Luck Bonus" (T.v_pct_mesx +1 4.0)
        , T.kv_ "Fishing Luck Bonus" (T.v_pct_mesx +1 0.0)
        , T.kv_ "Gold Drop Bonus" (T.v_pct_mesx +1 0.0)
        , T.kv_ "Avg. Equipment Level" (T.v_f 17.08)
        ]
    # T.group_ "Skills" [ "08-skills" ]
    # T.group "Hunter" [ "08-skills", "00-hunter" ]
        [ T.kv_ "Eviscreate" (T.v_pd { done = 1.4, total = 3.0 })
        , T.kv_ "Howl" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Raze" (T.v_pd { done = 1.2, total = 3.0 })
        , T.kv_ "Wolfstrike" (T.v_pd { done = 0.2, total = 3.0 })
        , T.kv_ "Battle Rage" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Rupture" (T.v_pd { done = 0.0, total = 0.0 })
        , T.kv_ "Ravage" (T.v_pd { done = 0.0, total = 0.0 })
        , T.kv_ "Blood Hunter" (T.v_empty)
        , T.kv_ "Executioner" (T.v_empty)
        ]
    # T.group "Tundra" [ "08-skills", "01-tundra" ]
        [ T.kv_ "Frost Breath" (T.v_pd { done = 0.0, total = 0.0 })
        , T.kv_ "Stormclaw" (T.v_pd { done = 0.5, total = 3.0 })
        , T.kv_ "Storm Hatchet" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Nothern Rage" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Iceshield" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Permafrost" (T.v_pd { done = 0.0, total = 0.0 })
        , T.kv_ "Glacial Shatter" (T.v_pd { done = 0.0, total = 0.0 })
        , T.kv_ "Shatter Storm" (T.v_empty)
        , T.kv_ "Rage Retaliation" (T.v_empty)
        ]
    # T.group "Shadow" [ "08-skills", "02-shadow" ]
        [ T.kv_ "Shadow Burst" (T.v_pd { done = 1.45, total = 3.0 })
        , T.kv_ "Wolf Shader" (T.v_pd { done = 1.45, total = 3.0 })
        , T.kv_ "ShadowBind" (T.v_pd { done = 0.45, total = 3.0 })
        , T.kv_ "Savage Rush" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Chain Snare" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Battle Standard" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Wolfpack" (T.v_pd { done = 0.0, total = 3.0 })
        , T.kv_ "Frenzy Mastery" (T.v_empty)
        , T.kv_ "Shred Armor" (T.v_empty)
        , T.kv_ "Red Wolf" (T.v_empty)
        ]
    # T.group "Stats" [ "09-stats" ]
        [ T.kv_ "Strength" (T.v_t "55+19->74")  -- Base+Items->Total
        , T.kv_ "Dexterity" (T.v_t "54+16->70") -- Base+Items->Total
        , T.kv_ "Focus" (T.v_t "18+27->45")     -- Base+Items->Total
        , T.kv_ "Vitality" (T.v_t "38+0->38")   -- Base+Items->Total
        ]
    # T.group_ "Effect" [ "09-stats", "00-effect" ]
    # T.group_ "Strength" [ "09-stats", "00-effect", "00-strength" ]
    # T.group "Weapon Damage" [ "09-stats", "00-effect", "00-strength", "00-weapon" ]
        [ T.kv_ "Right Hand" (T.v_rng { from = +88, to = +126 })
        , T.kv_ "Left Hand" (T.v_rng { from = +88, to = +126 })
        , T.kv_ "Effect" (T.v_pct_mesx +1 37.0)
        , T.kv_ "Critical Damage" (T.v_pct_mesx +1 37.0)
        ]
    # T.group "Dexterity" [ "09-stats", "00-effect", "01-dexterity" ]
        [ T.kv_ "Critical" (T.v_pct_mes +16)
        , T.kv_ "Dodge" (T.v_pct_mes +14)
        , T.kv_ "Critical Chance" (T.v_pct_mesx +1 13.0)
        , T.kv_ "Dodge Chance" (T.v_pct_mesx +1 13.0)
        , T.kv_ "Weapon Damage" (T.v_pct_mesx +1 446.0)
        ]
    # T.group "Focus" [ "09-stats", "00-effect", "02-focus" ]
        [ T.kv_ "Magic Damage" (T.v_rng { from = +47, to = +67 })
        , T.kv_ "Maximum Mana (MP)" (T.v_i +109)
        , T.kv_ "Magic Damage Effect" (T.v_pct_mesx +1 225.0)
        , T.kv_ "Execute Chance" (T.v_pct_mesx +1 33.0)
        , T.kv_ "Weapon Damage" (T.v_pct_mesx +1 446.0)
        ]
    # T.group "Vitality" [ "09-stats", "00-effect", "03-vitality" ]
        [ T.kv_ "Armor" (T.v_rng { from = +51, to = +101 })
        , T.kv_ "Maximum Health (MP)" (T.v_i +1890)
        , T.kv_ "Armor Bonus" (T.v_pct_mesx +1 9.5)
        , T.kv_ "Block Chance" (T.v_pct_mesx +1 7.3)
        ]

    )

{-

-\s([\w\s]+):\s+(\d+)$
-\s([\w\s]+):\s+"(\d+)"$
, T.kv_ "$1" (T.v_i +$2)


-\s([\w\s]+):\s+"([\d\.]+)%"$
, T.kv_ "$1" (T.v_pct $2)

-\s([\w\s]+):\s+"([+-])([\d\.]+)%"$
, T.kv_ "$1" (T.v_pctx $21 $3)


-\s([\w\s]+):\s+"([\d\.]+)/([\d\.]+)"$
, T.kv_ "$1" (T.v_pd { done = $2, total = $3 })

-\s([\w\s]+):\s+"(\d+)-(\d+)"$
, T.kv_ "$1" (T.v_pi { done = +$2, total = +$3 })

-}