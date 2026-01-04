let T = ./Types.dhall

in
    T.collapseAt
        { id = "balatro-ios"
        , name = "Balatro+"
        , platform = T.Platform.IOS
        , playtime = T.Playtime.Unknown
        }
        { day = +4, mon = +1, year = +2026 }

    ( T.group "Statistics" [ "00-stats" ]
        [ T.kv_ "Best Hand" (T.v_mes +3289 "Chips")
        , T.kv_ "Highest Round" (T.v_i +10)
        , T.kv_ "Highest Ante" (T.v_i +4)
        , T.kv_ "Most Played Hand" (T.v_t "Two Pair") -- FIXME: merge two values
        , T.kv_ "Most Played Hand (Count)" (T.v_i +13)
        , T.kv_ "Most Money" (T.v_mes +19 "$")
        , T.kv_ "Best Win Streak" (T.v_i +0) -- FIXME: merge two values
        , T.kv_ "Best Win Streak (Count)" (T.v_i +0)
        , T.kv_ "Progress" (T.v_pcti +1)
        , T.kv_ "Collection" (T.v_pcti +7)  -- FIXME: merge two values
        , T.kv_ "Collection (Items)" (T.v_pi { done = +24, total = +340 })
        , T.kv_ "Challenges" (T.v_pcti +0)  -- FIXME: merge two values
        , T.kv_ "Challenges (Count)" (T.v_pi { done = +0, total = +20 })
        , T.kv_ "Jocker Stickers" (T.v_pcti +0)  -- FIXME: merge two values
        , T.kv_ "Jocker Stickers (Count)" (T.v_pi { done = +0, total = +1200 })
        , T.kv_ "Deck Stake Wins" (T.v_pcti +0)  -- FIXME: merge two values
        , T.kv_ "Deck Stake Wins (Count)" (T.v_pi { done = +0, total = +120 })
        ]

    # T.group_ "Cards Stats" [ "01-cards-stats" ]

    # T.group "Cards Stats: Jockers" [ "01-cards-stats", "00-jockers" ] -- Total completed rounds with this card
        [ T.kv_ "Jocker" (T.v_mes +8 "Rounds") // T.inj/det "Common: +4 Mult"
        , T.kv_ "Ride the Bus" (T.v_mes +5 "Rounds") // T.inj/det "Common: Gains +1 Mult consecutive hand w/o scoring face card"
        , T.kv_ "Turtle Bean" (T.v_mes +5 "Rounds") // T.inj/det "Uncommon: +5 Hand size, reduced by 1 every round"
        , T.kv_ "Blue Jocker" (T.v_mes +4 "Rounds") // T.inj/det "Common: +2 Chips for each remaining card in deck"
        , T.kv_ "Midas Mask" (T.v_mes +3 "Rounds") // T.inj/det "Uncommon: All played face card become Gold cards when scored"
        , T.kv_ "Even Steven" (T.v_mes +2 "Rounds") // T.inj/det "Common: even rank give +4 Mult when scored"
        , T.kv_ "Banner" (T.v_mes +2 "Rounds") // T.inj/det "Common: +30 Chips for each remaining discard"
        , T.kv_ "DNA" (T.v_mes +1 "Rounds") // T.inj/det "Rare: if first hand of round has only 1 card, add a permamnent copy to deck and draw it"
        ]

    # T.group "Cards Stats: Consumables" [ "01-cards-stats", "01-consumables" ] -- Number of times this card has been used
        [ T.kv_ "Saturn" (T.v_mes +1 "Time") -- Planet
        , T.kv_ "The Sun" (T.v_mes +1 "Time") -- Tarot
        , T.kv_ "The Empress" (T.v_mes +1 "Time") -- Tarot
        , T.kv_ "Uranus" (T.v_mes +1 "Time") -- Planet
        , T.kv_ "Venus" (T.v_mes +1 "Time") -- Planet
        ]

    # T.group "Cards Stats: Consumables" [ "01-cards-stats", "01-consumables" ] -- Number of times this card has been used
        [ T.kv_ "Saturn" (T.v_mes +1 "Time") -- Planet
        , T.kv_ "The Sun" (T.v_mes +1 "Time") -- Tarot
        , T.kv_ "The Empress" (T.v_mes +1 "Time") -- Tarot
        , T.kv_ "Uranus" (T.v_mes +1 "Time") -- Planet
        , T.kv_ "Venus" (T.v_mes +1 "Time") -- Planet
        ]

    # T.group "Cards Stats: Tarots" [ "01-cards-stats", "02-tarots" ] -- Number of times this card has been used
        [ T.kv_ "The Sun" (T.v_mes +1 "Time") -- Tarot
        , T.kv_ "The Empress" (T.v_mes +1 "Time") -- Tarot
        ]

    # T.group "Cards Stats: Planets" [ "01-cards-stats", "03-planets" ] -- Number of times this card has been used
        [ T.kv_ "Saturn" (T.v_mes +1 "Time") -- Planet
        , T.kv_ "Uranus" (T.v_mes +1 "Time") -- Planet
        , T.kv_ "Venus" (T.v_mes +1 "Time") -- Planet
        ]

    # T.group_ "Cards Stats: Spectrals" [ "01-cards-stats", "04-spectrals" ] -- Number of times this card has been used

    # T.group "Cards Stats: Vouchers" [ "01-cards-stats", "05-vouchers" ] -- Number of times this Voucher has been used
        [ T.kv_ "Paint Brush" (T.v_mes +1 "Time") // T.inj/det "+1 Hand Size"
        ]

    # T.groupStats "Collection" [ "02-collection" ]

        [ T.kv_ "Jockers" (T.v_pi { done = +8, total = +150 }) // T.inj/self [ "02-collection", "00-jockers" ]
        , T.kv_ "Decks" (T.v_pi { done = +2, total = +15 }) // T.inj/self [ "02-collection", "01-decks" ]
        , T.kv_ "Vouchers" (T.v_pi { done = +1, total = +32 }) // T.inj/self [ "02-collection", "02-vouchers" ]
        , T.kv_ "Consumables: Tarot Cards" (T.v_pi { done = +3, total = +22 }) // T.inj/self [ "02-collection", "03-consumables", "00-tarots" ]
        , T.kv_ "Consumables: Planet Cards" (T.v_pi { done = +3, total = +12 }) // T.inj/self [ "02-collection", "03-consumables", "01-planets" ]
        , T.kv_ "Consumables: Spectral Cards" (T.v_pi { done = +0, total = +18 }) // T.inj/self [ "02-collection", "03-consumables", "02-spectrals" ]
        , T.kv_ "Enhanced Cards" (T.v_empty) // T.inj/self [ "02-collection", "04-enhanced" ]
        , T.kv_ "Seals" (T.v_empty) // T.inj/self [ "02-collection", "05-seals" ]
        , T.kv_ "Editions" (T.v_pi { done = +1, total = +5 }) // T.inj/self [ "02-collection", "06-editions" ]
        , T.kv_ "Booster Packs" (T.v_pi { done = +1, total = +32 }) // T.inj/self [ "02-collection", "07-booster-packs" ]
        , T.kv_ "Tags" (T.v_pi { done = +0, total = +24 }) // T.inj/self [ "02-collection", "08-tags" ]
        , T.kv_ "Blinds" (T.v_pi { done = +0, total = +30 }) // T.inj/self [ "02-collection", "09-blinds" ]
        ]

    # T.groupStats "Jockers" [ "02-collection", "00-jockers" ]

        -- Page 1

        -- Page 1: Row 1

        [ T.kv_ "Jocker" (T.v_done)
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 1: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 1: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 2

        -- Page 2: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 2: Row 2

        , T.kv_unk
        , T.kv_ "Banner" (T.v_done)
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 2: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 3

        -- Page 3: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 3: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_ "Even Steven" (T.v_done)
        , T.kv_unk

        -- Page 3: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_ "Ride the Bus" (T.v_done)
        , T.kv_unk

        -- Page 4

        -- Page 4: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 4: Row 2

        , T.kv_ "DNA" (T.v_done)
        , T.kv_unk
        , T.kv_ "Blue Jocker" (T.v_done)
        , T.kv_unk
        , T.kv_unk

        -- Page 4: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 5

        -- Page 5: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 5: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 5: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 6

        -- Page 6: Row 1

        , T.kv_ "Midas Mask" (T.v_done)
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_ "Turtle Bean" (T.v_done)

        -- Page 6: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 6: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 7

        -- Page 7: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 7: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 7: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 8

        -- Page 8: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 8: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 8: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 9

        -- Page 9: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 9: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page 9: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page X

        -- Page X: Row 1

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page X: Row 2

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk

        -- Page X: Row 3

        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk
        , T.kv_unk


        ]

    # T.groupStats "Decks" [ "02-collection", "01-decks" ]

        [ T.kv_ "Red Deck" (T.v_done)
        , T.kv_ "Blue Deck" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        ]

    # T.groupStats "Vouchers" [ "02-collection", "02-vouchers" ]

        -- Page 1/4

        [ T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 2/4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 3/4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 4/4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Paint Brush" (T.v_done)
        , T.kv_ "???" (T.v_none)

        ]

    # T.group_ "Consumables" [ "02-collection", "03-consumables" ]

    # T.groupStats "Tarot Cards" [ "02-collection", "03-consumables", "00-tarots" ]

        -- Page 1/2

        [ T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "The Empress" (T.v_done)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 2/2

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "The Devil" (T.v_done)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "The Sun" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        ]

    # T.groupStats "Planet Cards" [ "02-collection", "03-consumables", "01-planets" ]

        [ T.kv_ "???" (T.v_none)
        , T.kv_ "Venus" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Saturn" (T.v_done)

        , T.kv_ "Uranus" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        ]

    # T.groupStats "Spectral Cards" [ "02-collection", "03-consumables", "03-spectral" ]

        -- Page 1/2

        [ T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 2/2

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        ]

    # T.groupStats "Enhanced Cards" [ "02-collection", "04-enhanced" ]

        [ T.kv_ "Bonus Card" (T.v_empty)
        , T.kv_ "Mult Card" (T.v_empty)
        , T.kv_ "Wild Card" (T.v_empty)
        , T.kv_ "Glass Card" (T.v_empty)
        , T.kv_ "Steel Card" (T.v_empty)
        , T.kv_ "Stone Card" (T.v_empty)
        , T.kv_ "Gold Card" (T.v_empty)
        , T.kv_ "Lucky Card" (T.v_empty)
        ]

    # T.groupStats "Seals" [ "02-collection", "05-seals" ]

        [ T.kv_ "Gold Seal" (T.v_empty)
        , T.kv_ "Red Seal" (T.v_empty)
        , T.kv_ "Blue Seal" (T.v_empty)
        , T.kv_ "Purple Seal" (T.v_empty)
        ]

    # T.groupStats "Editions" [ "02-collection", "06-editions" ]

        [ T.kv_ "Base" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        ]

    # T.groupStats "Booster Packs" [ "02-collection", "07-booster-packs" ]

        -- Page 1/4

        [ T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 2/4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Page 3/4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "Mega Standard Pack" (T.v_done)

        -- Page 4/4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        ]

    # T.groupStats "Tags" [ "02-collection", "08-tags" ]

        -- Row 1

        [ T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 2

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 3

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        ]

    # T.groupStats "Blinds" [ "02-collection", "09-blinds" ]

        -- Row 1

        [ T.kv_ "Small Blind" (T.v_done)
        , T.kv_ "Big Blind" (T.v_done)
        , T.kv_ "The Hook" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 2

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 3

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "The Window" (T.v_done)
        , T.kv_ "???" (T.v_none)

        -- Row 4

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 5

        , T.kv_ "???" (T.v_none)
        , T.kv_ "The Head" (T.v_done)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        -- Row 6

        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)
        , T.kv_ "???" (T.v_none)

        ]

    # T.groupStats "Antes" [ "02-collection", "10-antes" ]

        [ T.kv_ "Ante / Base"
            (T.v_lvli
                { reached = +5000
                , levels =
                    [ { maximum = +300, name = "Ante 1" } // T.inj/no_date
                    , { maximum = +800, name = "Ante 2" } // T.inj/no_date
                    , { maximum = +2000, name = "Ante 3" } // T.inj/no_date
                    , { maximum = +5000, name = "Ante 4" } // T.inj/no_date
                    , { maximum = +11000, name = "Ante 5" } // T.inj/no_date
                    , { maximum = +20000, name = "Ante 6" } // T.inj/no_date
                    , { maximum = +35000, name = "Ante 7" } // T.inj/no_date
                    , { maximum = +50000, name = "Ante 8" } // T.inj/no_date
                    , { maximum = +110000, name = "Ante 9" } // T.inj/no_date
                    , { maximum = +560000, name = "Ante 10" } // T.inj/no_date
                    , { maximum = +7200000, name = "Ante 11" } // T.inj/no_date
                    , { maximum = +300000000, name = "Ante 12" } // T.inj/no_date
                    , { maximum = +47000000000, name = "Ante 13" } // T.inj/no_date
                    , { maximum = +29000000000000000, name = "Ante 14" } // T.inj/no_date
                    , { maximum = +77000000000000000000, name = "Ante 15" } // T.inj/no_date
                    , { maximum = +860000000000000000000000, name = "Ante 16" } // T.inj/no_date
                    ]
                }
            )

        ]


    )