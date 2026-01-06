let T = ./Types.dhall
let GT = ./Game.Types.dhall

in GT.collapseAt
        { id = "no-rest-for-the-wicked"
        , name = "No Rest For The Wicked"
        , platform = GT.Platform.Steam
        , playtime = GT.Playtime.MoreThan { hrs = +22, min = +42, sec = +0 }
        }
        { day = +27, mon = +12, year = +2025 } (

    T.group "File" [ "00-file" ]
        [ T.kv_ "Level" (T.v_i +13)
        , T.kv_ "XP" (T.v_pi { done = +843, total = +1100 })
        , T.kv_ "HP" (T.v_pi { done = +154, total = +154 })
        , T.kv_ "Attribute Points" (T.v_i +0)
        ]

    # T.group "Money" [ "01-money" ]
        [ T.kv_ "Silver" (T.v_i +11)
        , T.kv_ "Bronze" (T.v_i +77)
        ]

    # T.group "Stats" [ "02-stats" ]
        [ T.kv_ "Health" (T.v_i +16)
        , T.kv_ "Stamina" (T.v_i +12)
        , T.kv_ "Strength" (T.v_i +17)
        , T.kv_ "Dexterity" (T.v_i +22)
        , T.kv_ "Intelligence" (T.v_i +14)
        , T.kv_ "Faith" (T.v_i +10)
        , T.kv_ "Focus" (T.v_i +15)
        , T.kv_ "Equip Load" (T.v_i +10)
        ]

    # T.group "General" [ "02-stats", "00-general" ]
        [ T.kv_ "Hit Points" (T.v_i +154)
        , T.kv_ "Stamina Points" (T.v_i +58)
        , T.kv_ "Stamina Regen" (T.v_i +30)
        , T.kv_ "Focus Points" (T.v_i +185)
        , T.kv_ "Focus Gain" (T.v_pcti +110)
        ]

    # T.group "Defence" [ "02-stats", "01-defence" ]
        [ T.kv_ "Armor" (T.v_i +246) -- (T.v_pcti +33)
        , T.kv_ "Heat Resistance" (T.v_i +250) -- (T.v_pcti +33)
        , T.kv_ "Cold Resistance" (T.v_i +258) -- (T.v_pcti +34)
        , T.kv_ "Electric Resistance" (T.v_i +299) -- (T.v_pcti +38)
        , T.kv_ "Plague Resistance" (T.v_i +311) -- (T.v_pcti +39)
        , T.kv_ "Poise" (T.v_i +17)
        ]

    # T.group "Defence, %" [ "02-stats", "02-defence-pct" ]
        [ T.kv_ "Armor" (T.v_pcti +33)
        , T.kv_ "Heat Resistance" (T.v_pcti +33)
        , T.kv_ "Cold Resistance" (T.v_pcti +34)
        , T.kv_ "Electric Resistance" (T.v_pcti +38)
        , T.kv_ "Plague Resistance" (T.v_pcti +39)
        ]

    # T.group "Weight" [ "02-stats", "03-weight" ]
        [ T.kv_ "Equip Load" (T.v_i +170)
        , T.kv_ "Current Weight" (T.v_i +155)
        , T.kv_ "Weight Class" (T.v_t "Heavy")
        , T.kv_ "Weight Class (%)" (T.v_pcti +91)
        ]

    # T.group_ "Weapons" [ "03-weapons" ]

    # T.group "Blood Rusted Sword" [ "03-weapons", "00-blood-rusted-sword" ]
        [ T.kv_ "Base" (T.v_i +20)
        , T.kv_ "Attribute Bonus (Kind)" (T.v_t "Strength")
        , T.kv_ "Attribute Bonus" (T.v_i +9)
        , T.kv_ "Other" (T.v_i +0)
        , T.kv_ "Total" (T.v_i +29)
        ]

    # T.group "Short Bow" [ "03-weapons", "01-short-bow" ]
        [ T.kv_ "Base" (T.v_i +18)
        , T.kv_ "Attribute Bonus (Kind)" (T.v_t "Dexterity")
        , T.kv_ "Attribute Bonus" (T.v_i +13)
        , T.kv_ "Other" (T.v_i +0)
        , T.kv_ "Total" (T.v_i +31)
        ]

    # T.group_ "Quests" [ "04-quests" ]

    # T.group "Active" [ "04-quests", "00-active" ]
        [ T.kv_ "Servant of God, part 3" (T.p_todo) -- TODO level reached/?
        , T.kv_ "Of Rats and Raiders, part 1" (T.p_todo) -- TODO level reached/?
        , T.kv_ "Spilled Blood, part 1" (T.p_todo) -- TODO level reached/?
        , T.kv_ "Spoken and Unspoken, part 5" (T.p_todo) -- TODO level reached/?
        ]

    # T.group "Completed" [ "04-quests", "01-completed" ]
        [ T.kv_ "A Small Mercy, 4/4" (T.v_pi { done = +4, total = +4 })
        , T.kv_ "Sacrament 5/5" (T.v_pi { done = +5, total = +5 })
        ]

    # T.group "Been to" [ "05-been-to" ]
        [ T.kv_ "The Shallows" (T.p_done)
        , T.kv_ "Mariner's Keep" (T.p_done)
        , T.kv_ "Orbran Glades" (T.p_done)
        , T.kv_ "Sacrament (Home)" (T.p_done)
        , T.kv_ "Nameless Pass" (T.p_done)
        ]

    # T.group_ "Blueprints & Recipes" [ "06-blueprints-recipes" ]

    # T.group "Weapons" [ "06-blueprints-recipes", "00-weapons" ]
        [ T.kv_ "Claymore" (T.p_done)
        , T.kv_ "Petaled Spear" (T.p_done)
        ]

    # T.group "Armor" [ "06-blueprints-recipes", "01-armor" ]
        [ T.kv_ "Reckless Talbear Scarb" (T.p_done)
        , T.kv_ "Reckless Talbear Gloves" (T.p_done)
        , T.kv_ "Reckless Talbear Helmet" (T.p_done)
        , T.kv_ "Reckless Talbear Pants" (T.p_done)
        ]

    # T.group "Food" [ "06-blueprints-recipes", "02-food" ]
        [ T.kv_ "Fish Skewer" (T.p_done)
        , T.kv_ "Mushroom & Meat Curry" (T.p_done)
        , T.kv_ "Crab Chowder" (T.p_done)
        , T.kv_ "Crab Stuffed Mushroom" (T.p_done)
        , T.kv_ "Dracaena Dumplings" (T.p_done)
        , T.kv_ "Mushroom Soup" (T.p_done)
        ]

    # T.group "Decorations" [ "06-blueprints-recipes", "03-decorations" ]
        [ T.kv_ "Wayfarer's Rest Bed" (T.p_done)
        , T.kv_ "Small Chest" (T.p_done)
        , T.kv_ "Medium Chest" (T.p_done)
        ]

    )