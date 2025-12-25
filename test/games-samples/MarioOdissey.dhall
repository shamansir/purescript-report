let T = ./Types.dhall

let triple_v = T.v_pi { done = +3, total = +3 }

in
    T.collapseAt
        { id = "super-mario-odissey"
        , name = "Super Mario: Odissey"
        , platform = T.Platform.Switch
        , playtime = T.Playtime.MoreThan { hrs = +50, min = +0, sec = +0 }
        }
        { day = +14, mon = +8, year = +2025 }

    ( T.group "Power Moons" [ "00-pmoons" ]

        [ T.kv_ "Cascade Kingdom: Purple Coins" (T.v_pi { done = +50, total = +50 })
        , T.kv_ "Cascade Kingdom: Moons" (T.v_pi { done = +16, total = +25 }) // T.inj/self [ "00-pmoons", "00-cascade-kingdom" ]
        , T.kv_ "Sand Kingdom: Purple Coins" (T.v_pi { done = +93, total = +100 })
        , T.kv_ "Sand Kingdom: Moons" (T.v_pi { done = +47, total = +89 }) // T.inj/self [ "00-pmoons", "01-sand-kingdom" ]
        , T.kv_ "Wooded Kingdom: Purple Coins" (T.v_pi { done = +100, total = +100 })
        , T.kv_ "Wooded Kingdom: Moons" (T.v_pi { done = +36, total = +54 }) // T.inj/self [ "00-pmoons", "02-wooded-kingdom" ]
        , T.kv_ "Lake Kingdom: Purple Coins" (T.v_pi { done = +50, total = +50 })
        , T.kv_ "Lake Kingdom: Moons" (T.v_pi { done = +14, total = +33 }) // T.inj/self [ "00-pmoons", "03-lake-kingdom" ]
        , T.kv_ "Cloud Kingdom: Moons" (T.v_pi { done = +0, total = +2 }) // T.inj/self [ "00-pmoons", "04-cloud-kingdom" ]
        , T.kv_ "Lost Kingdom: Purple Coins" (T.v_pi { done = +48, total = +50 })
        , T.kv_ "Lost Kingdom: Moons" (T.v_pi { done = +15, total = +25 }) // T.inj/self [ "00-pmoons", "05-lost-kingdom" ]
        , T.kv_ "Metro Kingdom: Purple Coins" (T.v_pi { done = +81, total = +100 })
        , T.kv_ "Metro Kingdom: Moons" (T.v_pi { done = +26, total = +66 }) // T.inj/self [ "00-pmoons", "06-metro-kingdom" ]
        , T.kv_ "Seaside Kingdom: Purple Coins" (T.v_pi { done = +100, total = +100 })
        , T.kv_ "Seaside Kingdom: Moons" (T.v_pi { done = +23, total = +52 }) // T.inj/self [ "00-pmoons", "07-seaside-kingdom" ]
        , T.kv_ "Snow Kingdom: Purple Snowflakes" (T.v_pi { done = +41, total = +50 })
        , T.kv_ "Snow Kingdom: Moons" (T.v_pi { done = +19, total = +37 }) // T.inj/self [ "00-pmoons", "08-snow-kingdom" ]
        , T.kv_ "Luncheon Kingdom: Purple Tomatoes" (T.v_pi { done = +87, total = +100 })
        , T.kv_ "Luncheon Kingdom: Moons" (T.v_pi { done = +20, total = +56 }) // T.inj/self [ "00-pmoons", "09-luncheon-kingdom" ]
        , T.kv_ "Ruined Kingdom: Moons" (T.v_pi { done = +2, total = +5 }) // T.inj/self [ "00-pmoons", "10-ruined-kingdom" ]
        , T.kv_ "Bowser's Kingdom: Purple Tomatoes" (T.v_pi { done = +94, total = +100 })
        , T.kv_ "Bowser's Kingdom: Red Moons" (T.v_pi { done = +22, total = +45 }) // T.inj/self [ "00-pmoons", "10-bowsers-kingdom" ]
        , T.kv_ "Moon Kingdom: Purple Crystals" (T.v_pi { done = +44, total = +50 })
        , T.kv_ "Moon Kingdom: Moons" (T.v_pi { done = +5, total = +27 }) // T.inj/self [ "00-pmoons", "11-moon-kingdom" ]
        , T.kv_ "Dark Side: Moons" (T.v_pi { done = +0, total = +24 }) // T.inj/self [ "00-pmoons", "12-dark-side" ]
        , T.kv_ "Mushroom Kingdom: Purple Coins" (T.v_pi { done = +100, total = +100 })
        , T.kv_ "Mushroom Kingdom: Moons" (T.v_pi { done = +31, total = +104 }) // T.inj/self [ "00-pmoons", "13-mushroom-kingdom" ]
        , T.kv_ "Cap Kingdom: Purple Caps" (T.v_pi { done = +47, total = +50 })
        , T.kv_ "Cap Kingdom: Moons" (T.v_pi { done = +20, total = +31 }) // T.inj/self [ "00-pmoons", "14-cap-kingdom" ]
        ]

    # T.group "Cascade Kingdom" [ "00-pmoons", "00-cascade-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "Our First Power Moon" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "02" (triple_v) // T.inj/det "Multi Moon Atop the Falls" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Chomp Through the Rocks" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "Behind the Waterfall" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "On Top of the Rubble" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Treasure of the Waterfall Basin" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "07" (T.v_done) // T.inj/det "Above a High Cliff" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "08" (T.v_done) // T.inj/det "Across the Floating Isles" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "09" (T.v_done) // T.inj/det "Cascade Kingdom Timer Challenge 1" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "10" (T.v_done) // T.inj/det "Cascade Kingdom Timer Challenge 2" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "11" (T.v_done) // T.inj/det "Good Morning, Captain Toad!" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "12" (T.v_done) // T.inj/det "Dinosaur Nest: Big Cleanup!" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "13" (T.v_done) // T.inj/det "Dinosaur Nest: Running Wild" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "Past the Chasm Lifts" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "17" (T.v_done) // T.inj/det "Hidden Chasm Passage" // T.inj/date { mon = +11, day = +6, year = +2017 }
        , T.kv_ "18" (T.v_done) // T.inj/det "Secret Path to Fossil Falls!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        ]

    # T.group "Sand Kingdom" [ "00-pmoons", "01-sand-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "Atop the Highest Tower" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "Moon Shards in the Sand" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "03" (triple_v) // T.inj/det "Showdown on the Inverted Pyramid" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "04" (triple_v) // T.inj/det "The Hole in the Desert" // T.inj/date { mon = +11, day = +12, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "Overlooking the Desert Town" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "06" (T.v_none)
        , T.kv_ "07" (T.v_done) // T.inj/det "On the Leaning Pillar" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "08" (T.v_none)
        , T.kv_ "09" (T.v_done) // T.inj/det "Secret of the Mural" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "10" (T.v_done) // T.inj/det "Secret of the Inverted Mural" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "11" (T.v_done) // T.inj/det "On Top of the Stone Archway" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "12" (T.v_done) // T.inj/det "From a Crate in the Ruins" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "13" (T.v_done) // T.inj/det "On the Lone Pillar" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "14" (T.v_done) // T.inj/det "On the Statue’s Tail" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "Where the Birds Gather" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "17" (T.v_done) // T.inj/det "Top of a Dune" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "18" (T.v_done) // T.inj/det "Lost in the Luggage" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_done) // T.inj/det "Inside a Block Is a Hard Place" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_done) // T.inj/det "Bird Traveling the Wastes" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "23" (T.v_done) // T.inj/det "The Lurker Under the Stone" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "24" (T.v_done) // T.inj/det "The Treasure of Jaxi Ruins" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "25" (T.v_done) // T.inj/det "Desert Gardening: Plaza Seed" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "26" (T.v_done) // T.inj/det "Desert Gardening: Ruins Seed" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "27" (T.v_done) // T.inj/det "Desert Gardening: Seed on the Cliff" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_done) // T.inj/det "Sand Kingdom Timer Challenge 2" // T.inj/date { mon = +11, day = +13, year = +2017 }
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_done) // T.inj/det "Found in the Sand! Good Dog!" // T.inj/date { mon = +11, day = +13, year = +2017 }
        , T.kv_ "32" (T.v_done) // T.inj/det "Taking Notes: Jump on the Palm" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "33" (T.v_done) // T.inj/det "Herding Sheep in the Dunes" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "34" (T.v_done) // T.inj/det "Fishing in the Oasis" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "35" (T.v_none)
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        , T.kv_ "38" (T.v_none)
        , T.kv_ "38" (T.v_done) // T.inj/det "Jaxi Reunion!" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "39" (T.v_done) // T.inj/det "Welcome Back, Jaxi!" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "40" (T.v_done) // T.inj/det "Wandering Cactus" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "41" (T.v_none)
        , T.kv_ "42" (T.v_none)
        , T.kv_ "43" (T.v_done) // T.inj/det "Employees Only" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "44" (T.v_none) // T.inj/det "Sand Kingdom Slots" // T.inj/tags [ "parrot" ]
        , T.kv_ "45" (T.v_done) // T.inj/det "Walking the Desert" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "46" (T.v_none)
        , T.kv_ "47" (T.v_none)
        , T.kv_ "48" (T.v_done) // T.inj/det "Goomba Tower Assembly" // T.inj/date { mon = +11, day = +12, year = +2017 }
        , T.kv_ "49" (T.v_none)
        , T.kv_ "50" (T.v_none)
        , T.kv_ "51" (T.v_done) // T.inj/det "Sphynx’s Treasure Vault" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "52" (T.v_done) // T.inj/det "A Rumble from the Sandy Floor" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "53" (T.v_done) // T.inj/det "Dancing with New Friends" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "54" (T.v_done) // T.inj/det "The Invisible Maze" // T.inj/date { mon = +11, day = +8, year = +2017 }
        , T.kv_ "55" (T.v_none)
        , T.kv_ "56" (T.v_none)
        , T.kv_ "57" (T.v_none)
        , T.kv_ "58" (T.v_done) // T.inj/det "Jaxi Driver" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "59" (T.v_done) // T.inj/det "Jaxi Stunt Driving" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "60" (T.v_done) // T.inj/det "Strange Neighborhood" // T.inj/date { mon = +11, day = +13, year = +2017 }
        , T.kv_ "61" (T.v_done) // T.inj/det "Above a Strange Neighborhood" // T.inj/date { mon = +11, day = +13, year = +2017 }
        , T.kv_ "62" (T.v_done) // T.inj/det "Secret Path to Tostarena!" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "63" (T.v_none)
        , T.kv_ "64" (T.v_done) // T.inj/det "Jammin’ in the Sand Kingdom" // T.inj/date { mon = +12, day = +25, year = +2017 }
        , T.kv_ "65" (T.v_none)
        , T.kv_ "66" (T.v_none)
        , T.kv_ "67" (T.v_none)
        , T.kv_ "68" (T.v_none)
        , T.kv_ "69" (T.v_done) // T.inj/det "Peach in the Sand Kingdom" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "70" (T.v_done) // T.inj/det "Mighty Leap from the Palm Tree!" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "71" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "72" (T.v_done) // T.inj/det "Into the Flowing Sands" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +25, year = +2017 }
        , T.kv_ "73" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "74" (T.v_done) // T.inj/det "Island in the Poison Swamp" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "75" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "76" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "77" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "78" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "79" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "80" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "81" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "82" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "83" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "84" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "85" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "86" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "87" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "88" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "89" (T.v_none) // T.inj/tags [ "box" ]
        ]

    # T.group "Wooded Kingdom" [ "00-pmoons", "02-wooded-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "Road to Sky Garden" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "02" (triple_v) // T.inj/det "Flower Thieves of Sky Garden" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Path to the Secret Flower Field" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "04" (triple_v) // T.inj/det "Defend the Secret Flower Field" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "05" (T.v_none)
        , T.kv_ "06" (T.v_done) // T.inj/det "Back Way Up the Mountain" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "07" (T.v_done) // T.inj/det "Rolling Rock in the Woods" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "08" (T.v_done) // T.inj/det "Caught Hopping in the Forest!" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "09" (T.v_done) // T.inj/det "Thanks for the Charge!" // T.inj/date { mon = +11, day = +17, year = +2017 }
        , T.kv_ "10" (T.v_done) // T.inj/det "Atop the Tall Tree" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "11" (T.v_done) // T.inj/det "Tucked Away Inside the Tunnel" // T.inj/date { mon = +11, day = +17, year = +2017 }
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_done) // T.inj/det "The Nut ‘Round the Corner" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "14" (T.v_done) // T.inj/det "Climb the Cliff to Get the Nut" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "15" (T.v_done) // T.inj/det "The Nut in the Red Maze" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "16" (T.v_done) // T.inj/det "The Nut at the Dead End" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "17" (T.v_done) // T.inj/det "Cracked Nut on a Crumbling Tower" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "18" (T.v_done) // T.inj/det "The Nut that Grew on the Tall Fence" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "19" (T.v_done) // T.inj/det "Fire in the Cave" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "20" (T.v_done) // T.inj/det "Hey Out There, Captain Toad!" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "21" (T.v_done) // T.inj/det "Love in the Forest Ruins" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_done) // T.inj/det "Nut Planted in the Tower" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "25" (T.v_done) // T.inj/det "Stretching Your Legs" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_done) // T.inj/det "Make the Secret Flower Field Bloom" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_done) // T.inj/det "Past the Peculiar Pipes" // T.inj/date { mon = +11, day = +16, year = +2017 }
        , T.kv_ "31" (T.v_done) // T.inj/det "By the Babbling Brook in Deep Woods" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "32" (T.v_done) // T.inj/det "The Hard Rock in Deep Woods" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "33" (T.v_none) // T.inj/det "A Treasure Made from Coins" // T.inj/tags [ "parrot" ]
        , T.kv_ "34" (T.v_done) // T.inj/det "Beneath the Roots of the Moving Tree" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "35" (T.v_done) // T.inj/det "Deep Woods Treasure Trap" // T.inj/date { mon = +11, day = +15, year = +2017 }
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_done) // T.inj/det "Wooded Kingdom Timer Challenge 1" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "38" (T.v_done) // T.inj/det "Wooded Kingdom Timer Challenge 2" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "39" (T.v_done) // T.inj/det "Flooding Pipeway" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "40" (T.v_none)
        , T.kv_ "41" (T.v_done) // T.inj/det "Wandering in the Fog" // T.inj/date { mon = +11, day = +17, year = +2017 }
        , T.kv_ "42" (T.v_none)
        , T.kv_ "43" (T.v_done) // T.inj/det "Flower Road Reach" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "44" (T.v_done) // T.inj/det "Flower Road Run" // T.inj/date { mon = +11, day = +17, year = +2017 }
        , T.kv_ "45" (T.v_done) // T.inj/det "Elevator Escalation" // T.inj/date { mon = +11, day = +18, year = +2017 }
        , T.kv_ "46" (T.v_none)
        , T.kv_ "47" (T.v_done) // T.inj/det "Walking on Clouds" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "48" (T.v_done) // T.inj/det "Above the Clouds" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "49" (T.v_none)
        , T.kv_ "50" (T.v_none)
        , T.kv_ "51" (T.v_none)
        , T.kv_ "52" (T.v_none)
        , T.kv_ "53" (T.v_none)
        , T.kv_ "54" (T.v_none)
        ]

    # T.group "Lake Kingdom" [ "00-pmoons", "03-lake-kingdom" ]

        [ T.kv_ "01" (triple_v) // T.inj/det "Broodals Over the Lake" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "Dorrie-Back Rider" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Cheep Cheep Crossing" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "04" (T.v_none)
        , T.kv_ "05" (T.v_done) // T.inj/det "What’s in the Box?" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "On the Lakeshore" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "07" (T.v_none)
        , T.kv_ "08" (T.v_done) // T.inj/det "Treasure in the Spiky Waterway" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_none)
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_done) // T.inj/det "Taking Notes: Dive and Swim" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "14" (T.v_done) // T.inj/det "Taking Notes: In the Cliffside" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "I Met a Lake Cheep Cheep!" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "17" (T.v_done) // T.inj/det "Our Secret Little Room" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "18" (T.v_done) // T.inj/det "Let’s Go Swimming, Captain Toad!" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_done) // T.inj/det "A Successful Repair Job" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_done) // T.inj/det "Unzip the Chasm" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_done) // T.inj/det "Jump, Grab, Cling, and Climb" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_none)
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_none)
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        ]

    # T.group "Cloud Kingdom" [ "00-pmoons", "04-cloud-kingdom" ]

        [ T.kv_ "01" (T.v_none)
        , T.kv_ "02" (T.v_none)
        ]

    # T.group "Lost Kingdom" [ "00-pmoons", "05-lost-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "Atop a Propeller Pillar" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "Below the Cliff’s Edge" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Inside the Stone Cage" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "On a Tree in the Swamp" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "Over the Fuzzies, Above the Swamp" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Avoiding Fuzzies Inside the Wall" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "07" (T.v_none)
        , T.kv_ "08" (T.v_done) // T.inj/det "Enjoying the View of Forgotten Isle" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "09" (T.v_done) // T.inj/det "On the Mountain Road" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "10" (T.v_done) // T.inj/det "A Propeller Pillar’s Secret" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "11" (T.v_done) // T.inj/det "Wrecked Rock Block" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "12" (T.v_done) // T.inj/det "A Butterfly’s Treasure" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_done) // T.inj/det "Cave Gardening" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "15" (T.v_done) // T.inj/det "Moon Shards in the Jungle" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "16" (T.v_none)
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_done) // T.inj/det "Soaring Over Forgotten Isle!" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_done) // T.inj/det "Get Some Rest, Captain Toad!" // T.inj/date { mon = +11, day = +22, year = +2017 }
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        ]

    # T.group "Metro Kingdom" [ "00-pmoons", "06-metro-kingdom" ]

        [ T.kv_ "01" (triple_v) // T.inj/det "New Donk City’s Pest Problem" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "Drummer on Board!" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Guitarist on Board!" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "Bassist on Board!" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "Trumpeter on Board!" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Powering Up the Station" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "07" (triple_v) // T.inj/det "A Traditional Festival!" // T.inj/date { mon = +12, day = +1, year = +2017 }
        , T.kv_ "08" (T.v_done) // T.inj/det "Inside an Iron Girder" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "09" (T.v_done) // T.inj/det "Swaying in the Breeze" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "10" (T.v_done) // T.inj/det "Girder Sandwich" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "11" (T.v_none)
        , T.kv_ "12" (T.v_done) // T.inj/det "Dizzying Heights" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "13" (T.v_done) // T.inj/det "Secret Girder Tunnel!" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "14" (T.v_done) // T.inj/det "Who Piled Garbage on This?" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "15" (T.v_done) // T.inj/det "Hidden in the Scrap" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "16" (T.v_none)
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_none)
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_done) // T.inj/det "How You Doin’, Captain Toad?" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_none)
        , T.kv_ "28" (T.v_done) // T.inj/det "Metro Kingdom Slots" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_done) // T.inj/det "Remotely Captured Car" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_done) // T.inj/det "City Hall Lost & Found" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "35" (T.v_done) // T.inj/det "Sewer Treasure" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        , T.kv_ "38" (T.v_none)
        , T.kv_ "39" (T.v_none)
        , T.kv_ "40" (T.v_none) // T.inj/det "Off the Beaten Wire" // T.inj/tags [ "parrot" ]
        , T.kv_ "41" (T.v_done) // T.inj/det "Moon Shards Under Siege" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "42" (T.v_done) // T.inj/det "Sharpshooting Under Siege" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "43" (T.v_done) // T.inj/det "Inside the Rotating Maze" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "44" (T.v_done) // T.inj/det "Outside the Rotating Maze" // T.inj/date { mon = +11, day = +24, year = +2017 }
        , T.kv_ "45" (T.v_none)
        , T.kv_ "46" (T.v_none)
        , T.kv_ "47" (T.v_done) // T.inj/det "Bullet Building" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "48" (T.v_done) // T.inj/det "One Man’s Trash…" // T.inj/date { mon = +11, day = +25, year = +2017 }
        , T.kv_ "49" (T.v_none)
        , T.kv_ "50" (T.v_none)
        , T.kv_ "51" (T.v_done) // T.inj/det "Secret Path to New Donk City!" // T.inj/date { mon = +11, day = +14, year = +2017 }
        , T.kv_ "52" (T.v_none)
        , T.kv_ "53" (T.v_none)
        , T.kv_ "54" (T.v_none)
        , T.kv_ "55" (T.v_none)
        , T.kv_ "56" (T.v_none)
        , T.kv_ "57" (T.v_none)
        , T.kv_ "59" (T.v_none)
        , T.kv_ "60" (T.v_none)
        , T.kv_ "61" (T.v_none)
        , T.kv_ "62" (T.v_none)
        , T.kv_ "63" (T.v_none)
        , T.kv_ "64" (T.v_none)
        , T.kv_ "65" (T.v_none)
        , T.kv_ "66" (T.v_none)
        ]

    # T.group "Seaside Kingdom" [ "00-pmoons", "07-seaside-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "The Stone Pillar Seal" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "The Lighthouse Seal" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "The Hot Spring Seal" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "The Seal Above the Canyon" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "05" (triple_v) // T.inj/det "The Glass Is Half Full" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "On the Cliff Overlooking the Beach" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "07" (T.v_done) // T.inj/det "Ride the Jetstream" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "08" (T.v_done) // T.inj/det "Ocean-Bottom Maze: Treasure" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_done) // T.inj/det "Underwater Highway Tunnel" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "11" (T.v_done) // T.inj/det "Shh! It’s a Shortcut!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_done) // T.inj/det "Merci, Dorrie!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "Under a Dangerous Ceiling" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_done) // T.inj/det "The Back Canyon: Excavation!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_done) // T.inj/det "Treasure Trap Hidden in the Inlet" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_none)
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_done) // T.inj/det "Moon Shards in the Sea" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "31" (T.v_done) // T.inj/det "Taking Notes: Ocean Surface Dash" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_none)
        , T.kv_ "35" (T.v_none) // T.inj/det "Ocean Quiz: Good!" // T.inj/tags [ "parrot" ]
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        , T.kv_ "38" (T.v_none)
        , T.kv_ "39" (T.v_done) // T.inj/det "Looking Back in the Dark Waterway" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "40" (T.v_done) // T.inj/det "The Sphynx’s Underwater Vault" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "41" (T.v_done) // T.inj/det "A Rumble on the Seaside Floor" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "42" (T.v_done) // T.inj/det "A Relaxing Dance" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "43" (T.v_done) // T.inj/det "Wading in the Cloud Sea" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "44" (T.v_done) // T.inj/det "Sunken Treasure in the Cloud Sea" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "45" (T.v_none)
        , T.kv_ "46" (T.v_none)
        , T.kv_ "47" (T.v_none)
        , T.kv_ "48" (T.v_none)
        , T.kv_ "49" (T.v_done) // T.inj/det "Secret Path to Bubblaine!" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "50" (T.v_none)
        , T.kv_ "51" (T.v_none)
        , T.kv_ "52" (T.v_none)
        ]

    # T.group "Snow Kingdom" [ "00-pmoons", "08-snow-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "The Icicle Barrier" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "The Ice Wall Barrier" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "The Gusty Barrier" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "The Snowy Mountain Barrier" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "05" (triple_v) // T.inj/det "The Bound Bowl Grand Prix" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Entrance to Shiveria" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "07" (T.v_done) // T.inj/det "Behind Snowy Mountain" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "08" (T.v_done) // T.inj/det "Shining in the Snow in Town" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_done) // T.inj/det "The Shiverian Treasure Chest" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "12" (T.v_done) // T.inj/det "Treasure in the Ice Wall" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_done) // T.inj/det "Moon Shards in the Snow" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "16" (T.v_done) // T.inj/det "Taking Notes: Snow Path Dash" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "17" (T.v_done) // T.inj/det "Fishing in the Glacier!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "18" (T.v_done) // T.inj/det "Ice-Dodging Goomba Stack" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "19" (T.v_done) // T.inj/det "Captain Toad is Chilly!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_done) // T.inj/det "Jump ‘n’ Swim in the Freezing Water" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "27" (T.v_done) // T.inj/det "Freezing Water Near the Ceiling" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_done) // T.inj/det "Spinning Above the Clouds" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "32" (T.v_done) // T.inj/det "High-Altitude Spinning" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_none)
        , T.kv_ "35" (T.v_none)
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        ]

    # T.group "Luncheon Kingdom" [ "00-pmoons", "09-luncheon-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "The Broodals Are After Some Cookin’" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "Under the Cheese Rocks" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "03" (triple_v) // T.inj/det "Big Pot on the Volcano: Dive In!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "Climb Up the Cascading Magma" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "05" (triple_v) // T.inj/det "Cookatiel Showdown!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Piled on the Salt" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "07" (T.v_done) // T.inj/det "Lurking in the Pillar’s Shadow" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "08" (T.v_none)
        , T.kv_ "09" (T.v_done) // T.inj/det "Is This an Ingredient Too?!" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_none)
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_done) // T.inj/det "Light the Lantern on the Small Island" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_none)
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_none)
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_done) // T.inj/det "Beneath the Rolling Vegetables" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "22" (T.v_done) // T.inj/det "All the Cracks Are Fixed" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "23" (T.v_done) // T.inj/det "Taking Notes: Swimming in Magma" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_none)
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_done) // T.inj/det "Alcove Behind the Pillars of Magma" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_done) // T.inj/det "Light the Two Flames" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_none)
        , T.kv_ "35" (T.v_none)
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_done) // T.inj/det "Magma Swamp: Floating and Sinking" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "38" (T.v_none)
        , T.kv_ "39" (T.v_none)
        , T.kv_ "40" (T.v_none)
        , T.kv_ "41" (T.v_done) // T.inj/det "Fork Flickin’ to the Summit" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "42" (T.v_done) // T.inj/det "Fork Flickin’ Detour" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "43" (T.v_done) // T.inj/det "Excavate ‘n’ Search the Cheese Rocks" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "44" (T.v_done) // T.inj/det "Climb the Cheese Rocks" // T.inj/date { mon = +12, day = +2, year = +2017 }
        , T.kv_ "45" (T.v_none)
        , T.kv_ "46" (T.v_none)
        , T.kv_ "47" (T.v_done) // T.inj/det "Secret Path to Mount Volbono!" // T.inj/date { mon = +11, day = +19, year = +2017 }
        , T.kv_ "48" (T.v_none)
        , T.kv_ "49" (T.v_none)
        , T.kv_ "50" (T.v_none)
        , T.kv_ "51" (T.v_none)
        , T.kv_ "52" (T.v_none)
        , T.kv_ "53" (T.v_none)
        , T.kv_ "54" (T.v_none)
        , T.kv_ "55" (T.v_none)
        , T.kv_ "56" (T.v_none)
        ]

    # T.group "Ruined Kingdom" [ "00-pmoons", "10-ruined-kingdom" ]

        [ T.kv_ "01" (triple_v) // T.inj/det "Battle with the Lord of Lightning!" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "In the ancient Treasure Chest" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "03" (T.v_none)
        , T.kv_ "04" (T.v_none)
        , T.kv_ "05" (T.v_none)
        ]

    # T.group "Bowser's Kingdom" [ "00-pmoons", "10-bowsers-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "Infiltrate Bowser’s Castle!" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "02" (T.v_done) // T.inj/det "Smart Bombing" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Big Broodal Battle" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "04" (triple_v) // T.inj/det "Showdown at Bowser’s Castle" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "Behind the Big Wall" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Treasure Inside the Turret" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "07" (T.v_done) // T.inj/det "From the Side Above the Castle Gate" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "08" (T.v_done) // T.inj/det "Sunken Treasure in the Moat" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_none)
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_done) // T.inj/det "Inside a Block in the Castle" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "Exterminate the Ogres!" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_done) // T.inj/det "Taking Notes: Between Spines" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "19" (T.v_done) // T.inj/det "Stack Up Above the Wall" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "20" (T.v_done) // T.inj/det "Hidden Corridor Under the Floor" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "21" (T.v_done) // T.inj/det "Poking Your Nose in the Plaster Wall" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "22" (T.v_done) // T.inj/det "Poking the Turret Wall" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "23" (T.v_done) // T.inj/det "Poking Your Nose by the Great Gate" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "24" (T.v_done) // T.inj/det "Jizo All in a Row" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "25" (T.v_done) // T.inj/det "Underground Jizo" // T.inj/date { mon = +12, day = +3, year = +2017 }
        , T.kv_ "26" (T.v_done) // T.inj/det "Found Behind Bars!" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "27" (T.v_none)
        , T.kv_ "28" (T.v_done) // T.inj/det "Good to See You, Captain Toad!" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_done) // T.inj/det "Bowser’s Castle Treasure Vault" // T.inj/date { mon = +12, day = +4, year = +2017 }
        , T.kv_ "31" (T.v_done) // T.inj/det "Scene of Crossing the Poison Swamp" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_none)
        , T.kv_ "35" (T.v_none)
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        , T.kv_ "38" (T.v_none)
        , T.kv_ "39" (T.v_none)
        , T.kv_ "40" (T.v_none)
        , T.kv_ "41" (T.v_none)
        , T.kv_ "42" (T.v_none)
        , T.kv_ "43" (T.v_none)
        , T.kv_ "44" (T.v_none)
        , T.kv_ "45" (T.v_none)
        ]

    # T.group "Moon Kingdom" [ "00-pmoons", "11-moon-kingdom" ]

        [ T.kv_ "01" (T.v_none)
        , T.kv_ "02" (T.v_none)
        , T.kv_ "03" (T.v_none)
        , T.kv_ "04" (T.v_done) // T.inj/det "Rolling Rock on the Moon" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "Caught Hopping on the Moon!" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "06" (T.v_done) // T.inj/det "Cliffside Treasure Chest" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "07" (T.v_none)
        , T.kv_ "08" (T.v_none)
        , T.kv_ "09" (T.v_done) // T.inj/det "Under the Bowser Statue" // T.inj/date { mon = +12, day = +5, year = +2017 }
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_done) // T.inj/det "Around the Barrier Wall" // T.inj/date { mon = +12, day = +11, year = +2017 }
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_none)
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_none)
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        ]

    # T.group "Dark Side" [ "00-pmoons", "12-dark-side" ]

        [ T.kv_ "01" (T.v_none)
        , T.kv_ "02" (T.v_none)
        , T.kv_ "03" (T.v_none)
        , T.kv_ "04" (T.v_none)
        , T.kv_ "05" (T.v_none)
        , T.kv_ "06" (T.v_none)
        , T.kv_ "07" (T.v_none)
        , T.kv_ "08" (T.v_none)
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_none)
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_none)
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_none)
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        ]

    # T.group "Mushroom Kingdom" [ "00-pmoons", "13-mushroom-kingdom" ]

        [ T.kv_ "01" (T.v_none)
        , T.kv_ "02" (T.v_done) // T.inj/det "Pops Out of the Tail" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "03" (T.v_done) // T.inj/det "Caught Hopping at Peach’s Castle" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "Gardening for Toad: Garden Seed" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "05" (T.v_done) // T.inj/det "Gardening for Toad: Field Seed" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "06" (T.v_none)
        , T.kv_ "07" (T.v_done) // T.inj/det "Gardening for Toad: Lake Seed" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "08" (T.v_none)
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_done) // T.inj/det "Taking Notes: Around the Well" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "12" (T.v_done) // T.inj/det "Herding Sheep at Peach’s Castle" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "13" (T.v_done) // T.inj/det "Gobbling Fruit with Yoshi" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "Love at Peach’s Castle" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_none)
        , T.kv_ "19" (T.v_done) // T.inj/det "Jammin’ in the Mushroom Kingdom" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_none)
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_none)
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_none)
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_none)
        , T.kv_ "35" (T.v_none)
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        , T.kv_ "38" (T.v_none)
        , T.kv_ "39" (T.v_none)
        , T.kv_ "40" (T.v_none)
        , T.kv_ "41" (T.v_none)
        , T.kv_ "42" (T.v_none)
        , T.kv_ "43" (T.v_none)
        , T.kv_ "44" (T.v_done) // T.inj/det "Rescue Princess Peach :: Rescue Princess Peach from Bowser" // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "45" (T.v_pi { done = +14, total = +14 }) // T.inj/det "Achieve World Peace :: Bring Peace to all kingdoms in the world." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "46" (T.v_pi { done = +324, total = +100 }) // T.inj/det "Power Moon Knight :: Collect Power Moons" // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "47" (T.v_pi { done = +324, total = +300 }) // T.inj/det "Power Moon Wizard :: Collect Power Moons" // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "48" (T.v_pi { done = +324, total = +600 }) // T.inj/det "Power Moon Ruler :: Collect Power Moons" // T.inj/tags [ "mushroom" ]
        , T.kv_ "49" (T.v_pi { done = +5, total = +13 }) // T.inj/det "Regional Coin Shopper :: Buy using regional coins." // T.inj/tags [ "mushroom" ]
        , T.kv_ "50" (T.v_pi { done = +10, total = +10 }) // T.inj/det "Flat Moon Finder :: Collect 8-Bit Power Moons" // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "51" (T.v_pi { done = +10, total = +20 }) // T.inj/det "Flat Moon Fanatic :: Collect 8-Bit Power Moons" // T.inj/tags [ "mushroom" ]
        , T.kv_ "52" (T.v_pi { done = +15, total = +15 }) // T.inj/det "Treasure Chest Hunter :: Collect Power Moons from treasure chests." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "53" (T.v_pi { done = +15, total = +25 }) // T.inj/det "Super Treasure Chest Hunter :: Collect Power Moons from treasure chests." // T.inj/tags [ "mushroom" ]
        , T.kv_ "54" (T.v_pi { done = +9, total = +5 }) // T.inj/det "Note-Collecting World Tour :: Collect Power Moons from taking notes." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "55" (T.v_pi { done = +0, total = +20 }) // T.inj/det "Note-Collecting Space Tour :: Collect Power Moons from taking notes." // T.inj/tags [ "mushroom" ]
        , T.kv_ "56" (T.v_pi { done = +6, total = +10 }) // T.inj/det "Timer Challenge: Amateur :: Collect Power Moons from Timer Challenges." // T.inj/tags [ "mushroom" ]
        , T.kv_ "57" (T.v_pi { done = +6, total = +25 }) // T.inj/det "Timer Challenge: Professional :: Collect Power Moons from Timer Challenges." // T.inj/tags [ "mushroom" ]
        , T.kv_ "58" (T.v_pi { done = +8, total = +5 }) // T.inj/det "Captain Toad Meeter :: Meet up with Captain Toad" // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "59" (T.v_pi { done = +8, total = +10 }) // T.inj/det "Captain Toad Greeter :: Meet up with Captain Toad" // T.inj/tags [ "mushroom" ]
        , T.kv_ "60" (T.v_pi { done = +2, total = +5 }) // T.inj/det "Touring with Princess Peach :: Meet up with Princess Peach as she travels the world." // T.inj/tags [ "mushroom" ]
        , T.kv_ "61" (T.v_pi { done = +2, total = +10 }) // T.inj/det "Globe-Trotting with Princess Peach :: Meet up with Princess Peach as she travels the world." // T.inj/tags [ "mushroom" ]
        , T.kv_ "62" (T.v_pi { done = +2, total = +4 }) // T.inj/det "Master Sheep Herder :: Herd sheep to collect a Power Moon" // T.inj/tags [ "mushroom" ]
        , T.kv_ "63" (T.v_pi { done = +2, total = +5 }) // T.inj/det "Gaga for Goombette :: Collect Power Moons from Gombette." // T.inj/tags [ "mushroom" ]
        , T.kv_ "64" (T.v_pi { done = +2, total = +3 }) // T.inj/det "Lakitu Fishing Trip :: Fish Power Moons as Lakitu" // T.inj/tags [ "mushroom" ]
        , T.kv_ "65" (T.v_pi { done = +6, total = +5 }) // T.inj/det "Flower-Growing Guru :: Grow seeds to collect Power Moons" // T.inj/tags [ "mushroom" ] -- marked as not-done
        , T.kv_ "66" (T.v_pi { done = +6, total = +10 }) // T.inj/det "Flower-Growing Sage :: Grow seeds to collect Power Moons" // T.inj/tags [ "mushroom" ]
        , T.kv_ "67" (T.v_pi { done = +4, total = +5 }) // T.inj/det "Running with Rabbits :: Catch rabbits to collect Power Moons." // T.inj/tags [ "mushroom" ]
        , T.kv_ "68" (T.v_pi { done = +4, total = +10 }) // T.inj/det "Racing with Rabbits :: Catch rabbits to collect Power Moons." // T.inj/tags [ "mushroom" ]
        , T.kv_ "69" (T.v_pi { done = +23, total = +15 }) // T.inj/det "Ground Pound Instructor :: Dig up Power Moons by Ground Pounding." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "70" (T.v_pi { done = +23, total = +45 }) // T.inj/det "Ground Pound Professor :: Dig up Power Moons by Ground Pounding." // T.inj/tags [ "mushroom" ]
        , T.kv_ "71" (T.v_pi { done = +3, total = +3 }) // T.inj/det "Rad Hatter :: Collect Power Moons by throwing your hat on things."  // T.inj/tags [ "mushroom" ] -- marked as not-done
        , T.kv_ "72" (T.v_pi { done = +3, total = +10 }) // T.inj/det "Super Rad Hatter :: Collect Power Moons by throwing your hat on things." // T.inj/tags [ "mushroom" ]
        , T.kv_ "73" (T.v_pi { done = +3, total = +5 }) // T.inj/det "Traveling Bird Herder :: Collect Power Moons from migrating birds." // T.inj/tags [ "mushroom" ]
        , T.kv_ "74" (T.v_pi { done = +3, total = +3 }) // T.inj/det "Wearing It Well! :: Collect Power Moons by wearing certain outfits." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "75" (T.v_pi { done = +3, total = +8 }) // T.inj/det "Wearing It Great! :: Collect Power Moons by wearing certain outfits." // T.inj/tags [ "mushroom" ]
        , T.kv_ "76" (T.v_pi { done = +3, total = +15 }) // T.inj/det "Wearing It Perfect! :: Collect Power Moons by wearing certain outfits." // T.inj/tags [ "mushroom" ]
        , T.kv_ "77" (T.v_pi { done = +0, total = +5 }) // T.inj/det "Hat-Seeking Missile :: Find Bonetters hidden on people's heads." // T.inj/tags [ "mushroom" ]
        , T.kv_ "78" (T.v_pi { done = +2, total = +5 }) // T.inj/det "Music Maestro :: Play the music that Toad wants to hear." // T.inj/tags [ "mushroom" ]
        , T.kv_ "79" (T.v_pi { done = +0, total = +5 }) // T.inj/det "Art Enthusiast :: Collect Power Moons using Hint Art." // T.inj/tags [ "mushroom" ]
        , T.kv_ "80" (T.v_pi { done = +0, total = +15 }) // T.inj/det "Art Investigator :: Collect Power Moons using Hint Art." // T.inj/tags [ "mushroom" ]
        , T.kv_ "81" (T.v_pi { done = +1, total = +3 }) // T.inj/det "Slots Machine :: Collect Power Moons playing Slots." // T.inj/tags [ "mushroom" ]
        , T.kv_ "82" (T.v_pi { done = +0, total = +10 }) // T.inj/det "Koopa Freerunning MVP :: Win Koopa Freerunning races." // T.inj/tags [ "mushroom" ]
        , T.kv_ "83" (T.v_pi { done = +0, total = +22 }) // T.inj/det "Koopa Freerunning Hall of Famer :: Win Koopa Freerunning races." // T.inj/tags [ "mushroom" ]
        , T.kv_ "84" (T.v_pi { done = +1, total = +5 }) // T.inj/det "Supernaturally Sure-Footed :: Receive passing scores in Koopa Trace-Walking." // T.inj/tags [ "mushroom" ]
        , T.kv_ "85" (T.v_pi { done = +0, total = +3 }) // T.inj/det "Quizmaster :: Answer all of the Sphynx's questions correctly." // T.inj/tags [ "mushroom" ]
        , T.kv_ "86" (T.v_pi { done = +3, total = +10 }) // T.inj/det "Souvenir Sampler :: Collect souvenirs and stickers." // T.inj/tags [ "mushroom" ]
        , T.kv_ "87" (T.v_pi { done = +0, total = +20 }) // T.inj/det "Souvenir Sleuth :: Collect souvenirs and stickers." // T.inj/tags [ "mushroom" ]
        , T.kv_ "88" (T.v_pi { done = +0, total = +40 }) // T.inj/det "Souvenir Savant :: Collect souvenirs and stickers." // T.inj/tags [ "mushroom" ]
        , T.kv_ "89" (T.v_pi { done = +48, total = +20 }) // T.inj/det "Capturing Novice :: Capture targets." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "90" (T.v_pi { done = +48, total = +35 }) // T.inj/det "Capturing Apprentice :: Capture targets." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "91" (T.v_pi { done = +48, total = +45 }) // T.inj/det "Capturing Master :: Capture targets." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "92" (T.v_pi { done = +16, total = +15 }) // T.inj/det "Hat Maven :: Collect hats." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "93" (T.v_pi { done = +16, total = +35 }) // T.inj/det "Hat Icon :: Collect hats." // T.inj/tags [ "mushroom" ]
        , T.kv_ "94" (T.v_pi { done = +17, total = +15 }) // T.inj/det "Fashion Maven :: Collect outfits." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "95" (T.v_pi { done = +17, total = +35 }) // T.inj/det "Fashion Icon :: Collect outfits." // T.inj/tags [ "mushroom" ]
        , T.kv_ "96" (T.v_pi { done = +2, total = +14 }) // T.inj/det "Moon Rock Liberator :: Open Moon Rocks." // T.inj/tags [ "mushroom" ]
        , T.kv_ "97" (T.v_pi { done = +6, total = +10 }) // T.inj/det "World Warper :: Travel through warp holes." // T.inj/tags [ "mushroom" ]
        , T.kv_ "98" (T.v_pi { done = +77, total = +40 }) // T.inj/det "Checkpoint Flagger :: Activate Checkpoint Flags." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "99" (T.v_pi { done = +77, total = +80 }) // T.inj/det "Checkpoint Flag Enthusiast :: Activate Checkpoint Flags." // T.inj/tags [ "mushroom" ]
        , T.kv_ "100" (T.v_pi { done = +19721, total = +1000 }) // T.inj/det "Loaded with Coins :: Collect coins." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "101" (T.v_pi { done = +19721, total = +5000 }) // T.inj/det "Rolling in Coins :: Collect coins." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "102" (T.v_pi { done = +19721, total = +10000 }) // T.inj/det "Swimming in Coins :: Collect coins." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "103" (T.v_pi { done = +16530, total = +10000 }) // T.inj/det "Jump! Jump! Jump! :: Jump." // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "104" (T.v_pi { done = +11434, total = +5000 }) // T.inj/det "Fly, Cappy, Fly! :: Throw Cappy" // T.inj/tags [ "mushroom" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        ]

    # T.group "Cap Kingdom" [ "00-pmoons", "14-cap-kingdom" ]

        [ T.kv_ "01" (T.v_done) // T.inj/det "Frog-Jumping Above the Fog" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "02" (T.v_none)
        , T.kv_ "03" (T.v_done) // T.inj/det "Cap Kingdom Timer Challenge 1" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "04" (T.v_done) // T.inj/det "Good Evening, Captain Toad!" // T.inj/date { mon = +11, day = +11, year = +2017 }
        , T.kv_ "05" (T.v_none) // T.inj/det "Shopping in Bonneton" // T.inj/tags [ "parrot" ]
        , T.kv_ "06" (T.v_done) // T.inj/det "Skimming the Poison Tide" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "07" (T.v_none)
        , T.kv_ "08" (T.v_done) // T.inj/det "Push-Block Peril" // T.inj/date { mon = +12, day = +25, year = +2017 }
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_done) // T.inj/det "Searching the Frog Pond" // T.inj/date { mon = +12, day = +11, year = +2017 }
        , T.kv_ "11" (T.v_done) // T.inj/det "Secrets of the Frog Pond" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "12" (T.v_done) // T.inj/det "The Forgotten Treasure" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_done) // T.inj/det "Peach in the Cap Kingdom" // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_done) // T.inj/det "Next to Glasses Bridge" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "19" (T.v_done) // T.inj/det "Danger Sign" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "20" (T.v_done) // T.inj/det "Under the Big One’s Brim" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "21" (T.v_done) // T.inj/det "Fly to the Edge of the Fog" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "22" (T.v_done) // T.inj/det "Spin the Hat, Get a Prize" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "23" (T.v_done) // T.inj/det "Hidden in a Sunken Hat" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "24" (T.v_done) // T.inj/det "Fog-Shrouded Platform" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "25" (T.v_done) // T.inj/det "Bird Traveling in the Fog" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "26" (T.v_done) // T.inj/det "Caught Hopping Near the Ship!" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +25, year = +2017 }
        , T.kv_ "27" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "28" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "29" (T.v_none) // T.inj/tags [ "box" ]
        , T.kv_ "30" (T.v_done) // T.inj/det "Roll On and On" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        , T.kv_ "31" (T.v_done) // T.inj/det "Precision Rolling" // T.inj/tags [ "box" ] // T.inj/date { mon = +12, day = +12, year = +2017 }
        ]

    # T.group "Others" [ "01-others" ]

        [ T.kv_ "Capture List" (T.v_pi { done = +48, total = +52 }) // T.inj/self [ "01-others", "00-captures" ]
        , T.kv_ "Souvenir List" (T.v_pi { done = +3, total = +43 }) // T.inj/self [ "01-others", "01-souvenirs" ]
        , T.kv_ "Music List" (T.v_pi { done = +74, total = +82 }) // T.inj/self [ "01-others", "02-music" ]
        ]

    # T.group "Capture List" [ "01-others", "00-captures" ]

        [ T.kv_ "1: Frog" (T.v_done) // T.inj/det "Jump extremely high to reach elevated areas."
        , T.kv_ "2: Spark Pylon" (T.v_done) // T.inj/det "Zip along power lines to traverse across levels."
        , T.kv_ "3: Paragoomba" (T.v_done) // T.inj/det "Fly over gaps and hazards."
        , T.kv_ "4: Chain Chomp" (T.v_done) // T.inj/det "Swing and break obstacles with momentum."
        , T.kv_ "5: Big Chain Chomp" (T.v_done) // T.inj/det "Same as Chain Chomp, with longer reach."
        , T.kv_ "6: Chain Chompikins" (T.v_done) // T.inj/det "Used in the fight with Madame Broode."
        , T.kv_ "7: T-Rex" (T.v_done) // T.inj/det "Smash through rocks and enemies (temporary)."
        , T.kv_ "8: Binoculars" (T.v_done) // T.inj/det "Look around and survey the surrounding area."
        , T.kv_ "9: Bullet Bill" (T.v_done) // T.inj/det "Fly quickly to destroy blocks (temporary)."
        , T.kv_ "10: Moe-Eye" (T.v_done) // T.inj/det "See invisible platforms and objects with its shades."
        , T.kv_ "11: Cactus" (T.v_done) // T.inj/det "Move blocked areas to reveal hidden Power Moons."
        , T.kv_ "12: Goomba" (T.v_done) // T.inj/det "Stack into towers; unlock affection-based Moon."
        , T.kv_ "13: Knucklotec’s Fist" (T.v_done) // T.inj/det "Punch enemies during boss fight with Knucklotec."
        , T.kv_ "14: Mini Rocket" (T.v_done) // T.inj/det "Fly to sky-platform areas to find moons."
        , T.kv_ "15: Glydon" (T.v_done) // T.inj/det "Glide across long distances."
        , T.kv_ "16: Lakitu" (T.v_done) // T.inj/det "Fish for Cheep Cheeps and capture Big Cheep Cheeps."
        , T.kv_ "17: Zipper" (T.v_done) // T.inj/det "Open hidden paths by unzipping sections of levels."
        , T.kv_ "18: Cheep Cheep" (T.v_done) // T.inj/det "Swim faster; breathe underwater."
        , T.kv_ "19: Puzzle Part (Lake Kingdom)" (T.v_done) // T.inj/det "Solve a Lochlady puzzle to earn a Power Moon."
        , T.kv_ "20: Poison Piranha Plant" (T.v_done) // T.inj/det "Spit poison to clear enemies (post rock-throw)."
        , T.kv_ "21: Uproot" (T.v_done) // T.inj/det "Stretch and break high Rock Blocks."
        , T.kv_ "22: Fire Bro" (T.v_done) // T.inj/det "Throw fire balls to defeat obstacles and enemies."
        , T.kv_ "23: Sherm" (T.v_done) // T.inj/det "Drive and destroy barriers as a tank."
        , T.kv_ "24: Coin Coffer" (T.v_done) // T.inj/det "Shoot coins to attack and solve puzzles."
        , T.kv_ "25: Tree" (T.v_done) // T.inj/det "Move it to reveal secret ground spots."
        , T.kv_ "26: Boulder" (T.v_done) // T.inj/det "Shift rocks to clear paths or access areas."
        , T.kv_ "27: Picture Match Part (Goomba)" (T.v_none) // T.inj/det "Collect Goomba puzzle piece in Cloud Kingdom."
        , T.kv_ "28: Tropical Wiggler" (T.v_done) // T.inj/det "Stretch onto small platforms and alcoves."
        , T.kv_ "29: Pole" (T.v_done) // T.inj/det "Propel Mario long distances in Metro Kingdom."
        , T.kv_ "30: Manhole Extension" (T.v_done) // T.inj/det "Lift a manhole to reveal hidden Power Moons."
        , T.kv_ "31: Taxi" (T.v_done) // T.inj/det "Fly to secret night zone to gather Moon Shards."
        , T.kv_ "32: R.C. Car" (T.v_done) // T.inj/det "Drive to break into a cage and collect moons."
        , T.kv_ "33: Ty-Foo" (T.v_done) // T.inj/det "Blow away obstacles with wind."
        , T.kv_ "34: Shiverian Racer" (T.v_done) // T.inj/det "Race in the Bound Bowl Grand Prix."
        , T.kv_ "35: Snow Cheep Cheep" (T.v_done) // T.inj/det "Swim through freezing waters without damage."
        , T.kv_ "36: Gushen" (T.v_done) // T.inj/det "Spray water to reach higher platforms."
        , T.kv_ "37: Podoboo" (T.v_done) // T.inj/det "Swim through lava unharmed."
        , T.kv_ "38: Volbonan" (T.v_done) // T.inj/det "Propel Mario to reach distant heights."
        , T.kv_ "39: Hammer Bro / Chef Bro" (T.v_done) // T.inj/det "Throw hammers/pans to clear obstacles."
        , T.kv_ "40: Meat" (T.v_done) // T.inj/det "Lure Cookatiel to release a Multi Moon."
        , T.kv_ "41: Venus Fire Trap" (T.v_none) // T.inj/det "Spit fire to activate mechanisms."
        , T.kv_ "42: Pokio" (T.v_done) // T.inj/det "Launch upwards by poking into walls."
        , T.kv_ "43: Jizo" (T.v_done) // T.inj/det "Move or align for coins, blocks, or P-Switches."
        , T.kv_ "44: Bowser Statue" (T.v_done) // T.inj/det "Shift it to uncover hidden Power Moons."
        , T.kv_ "45: Parabones" (T.v_done) // T.inj/det "Fly across lava with bone wings."
        , T.kv_ "46: Banzai Bill" (T.v_done) // T.inj/det "Crash through lava areas invincibly."
        , T.kv_ "47: Chargin' Chuck" (T.v_done) // T.inj/det "Charge and break heavy obstacles."
        , T.kv_ "48: Bowser" (T.v_done) // T.inj/det "Smash huge blocks during Moon Kingdom story event."
        , T.kv_ "49: Letter (M-A-R-I-O)" (T.v_none) // T.inj/det "Spell \"Mario\" to earn a Power Moon."
        , T.kv_ "50: Puzzle Part (Metro Kingdom)" (T.v_none) // T.inj/det "Activate a socket to unlock a Moon."
        , T.kv_ "51: Picture Match Part (Mario)" (T.v_done) // T.inj/det "Collect puzzle piece in Mushroom Kingdom."
        , T.kv_ "52: Yoshi" (T.v_done) // T.inj/det "Eat berries; 10 eaten grant a Power Moon."
        ]

    # T.group "Souvenir List" [ "01-others", "01-souvenirs" ]

        [ T.kv_ "01: Plush Frog" (T.v_done) // T.inj/det "These tiny froggies wear tiny top hats for self-defense, which looks so darn cute, they made a toy version!"
        , T.kv_ "02" (T.v_none)
        , T.kv_ "03" (T.v_none)
        , T.kv_ "04" (T.v_none)
        , T.kv_ "05" (T.v_none)
        , T.kv_ "06" (T.v_none)
        , T.kv_ "07" (T.v_none)
        , T.kv_ "08" (T.v_none)
        , T.kv_ "09" (T.v_none)
        , T.kv_ "10" (T.v_none)
        , T.kv_ "11" (T.v_none)
        , T.kv_ "12" (T.v_none)
        , T.kv_ "13" (T.v_none)
        , T.kv_ "14" (T.v_none)
        , T.kv_ "15" (T.v_none)
        , T.kv_ "16" (T.v_none)
        , T.kv_ "17" (T.v_none)
        , T.kv_ "18" (T.v_none)
        , T.kv_ "19" (T.v_none)
        , T.kv_ "20" (T.v_none)
        , T.kv_ "21" (T.v_none)
        , T.kv_ "22" (T.v_none)
        , T.kv_ "23" (T.v_none)
        , T.kv_ "24" (T.v_none)
        , T.kv_ "25" (T.v_none)
        , T.kv_ "26" (T.v_none)
        , T.kv_ "27" (T.v_done) // T.inj/det "A sticker inspired by Top-Hat Tower."
        , T.kv_ "28" (T.v_none)
        , T.kv_ "29" (T.v_done) // T.inj/det "A sticker inspired by the Inverted Pyramid."
        , T.kv_ "30" (T.v_none)
        , T.kv_ "31" (T.v_none)
        , T.kv_ "32" (T.v_none)
        , T.kv_ "33" (T.v_none)
        , T.kv_ "34" (T.v_none)
        , T.kv_ "35" (T.v_none)
        , T.kv_ "36" (T.v_none)
        , T.kv_ "37" (T.v_none)
        , T.kv_ "38" (T.v_none)
        , T.kv_ "39" (T.v_none)
        , T.kv_ "40" (T.v_none)
        , T.kv_ "41" (T.v_none)
        , T.kv_ "42" (T.v_none)
        , T.kv_ "43" (T.v_none)
        ]

    # T.group "Music List" [ "01-others", "02-music" ]

        [ T.kv_ "1: Bonneton" (T.v_done)
        , T.kv_ "2: Fossil Falls" (T.v_done)
        , T.kv_ "3: Fossil Falls (8-Bit)" (T.v_done)
        , T.kv_ "4: Fossil Falls: Dinosaur" (T.v_done)
        , T.kv_ "5: Tostarena: Ruins" (T.v_done)
        , T.kv_ "6: Tostarena: Ruins (8-Bit)" (T.v_done)
        , T.kv_ "7: Tostarena: Night" (T.v_done)
        , T.kv_ "8: Tostarena: Night (8-Bit)" (T.v_done)
        , T.kv_ "9: Tostarena: Town" (T.v_done)
        , T.kv_ "10: Tostarena: Jaxi" (T.v_done)
        , T.kv_ "11: Steam Gardens" (T.v_done)
        , T.kv_ "12: Steam Gardens (8-Bit)" (T.v_done)
        , T.kv_ "13: Steam Gardens: Sherm" (T.v_done)
        , T.kv_ "14: Lake Lamode 1" (T.v_done)
        , T.kv_ "15: Lake Lamode 1 (8-Bit)" (T.v_done)
        , T.kv_ "16: Lake Lamode 2" (T.v_done)
        , T.kv_ "17: Lake Lamode: Underwater Passage" (T.v_done)
        , T.kv_ "18: Forgotten Isle 1" (T.v_done)
        , T.kv_ "19: Forgotten Isle 2" (T.v_done)
        , T.kv_ "20: Forgotten Isle 2 (8-Bit)" (T.v_done)
        , T.kv_ "21: New Donk City: Night 1" (T.v_done)
        , T.kv_ "22: New Donk City: Night 2" (T.v_done)
        , T.kv_ "23: New Donk City: Daytime" (T.v_done)
        , T.kv_ "24: New Donk City: Cafe" (T.v_none)
        , T.kv_ "25: New Donk City (Band Performance)" (T.v_done)
        , T.kv_ "26: NDC Festival" (T.v_done)
        , T.kv_ "27: NDC Festival (Japanese)" (T.v_done)
        , T.kv_ "28: NDC Festival (8-Bit)" (T.v_none)
        , T.kv_ "29: Bubblaine" (T.v_done)
        , T.kv_ "30: Bubblaine: Underwater" (T.v_done)
        , T.kv_ "31: Bubblaine (8-Bit)" (T.v_done)
        , T.kv_ "32: Shiveria: Town" (T.v_done)
        , T.kv_ "33: Shiveria: Race-Course Entrance" (T.v_done)
        , T.kv_ "34: Mount Volbono" (T.v_done)
        , T.kv_ "35: Mount Volbono (8-Bit)" (T.v_done)
        , T.kv_ "36: Mount Volbono: Town" (T.v_done)
        , T.kv_ "37: Bowser’s Castle 1" (T.v_done)
        , T.kv_ "38: Bowser’s Castle 1 (8-Bit)" (T.v_done)
        , T.kv_ "39: Bowser’s Castle 2" (T.v_done)
        , T.kv_ "40: Honeylune Ridge" (T.v_done)
        , T.kv_ "41: Honeylune Ridge (8-Bit)" (T.v_none)
        , T.kv_ "42: Honeylune Ridge: Caves" (T.v_done)
        , T.kv_ "43: Honeylune Ridge: Wedding Hall" (T.v_done)
        , T.kv_ "44: Honeylune Ridge: Collapse" (T.v_done)
        , T.kv_ "45: Honeylune Ridge: Collapse (8-Bit)" (T.v_done)
        , T.kv_ "46: Honeylune Ridge: Escape (\"Break Free (Lead the Way)\")" (T.v_done)
        , T.kv_ "47: Honeylune Ridge: Escape (Japanese)" (T.v_done)
        , T.kv_ "48: Honeylune Ridge: Escape (8-Bit)" (T.v_none)
        , T.kv_ "49: Peach’s Castle" (T.v_done)
        , T.kv_ "50: Broodals Battle" (T.v_done)
        , T.kv_ "51: Madame Broode Battle" (T.v_done)
        , T.kv_ "52: Knucklotec Battle" (T.v_done)
        , T.kv_ "53: Torkdrift Battle" (T.v_done)
        , T.kv_ "54: Mechawiggler Battle" (T.v_done)
        , T.kv_ "55: Mollusque-Lanceur Battle" (T.v_done)
        , T.kv_ "56: Mollusque-Lanceur Battle (8-Bit)" (T.v_done)
        , T.kv_ "57: Cookatiel Battle" (T.v_done)
        , T.kv_ "58: Ruined Dragon Battle" (T.v_done)
        , T.kv_ "59: RoboBrood Battle" (T.v_done)
        , T.kv_ "60: Bowser Battle 1" (T.v_done)
        , T.kv_ "61: Bowser Battle 2" (T.v_done)
        , T.kv_ "62: Run, Jump, Throw! 1" (T.v_done)
        , T.kv_ "63: Run, Jump, Throw! 2" (T.v_done)
        , T.kv_ "64: Run, Jump, Throw! 2 (8-Bit)" (T.v_done)
        , T.kv_ "65: Subterranean 1" (T.v_done)
        , T.kv_ "66: Subterranean 1 (8-Bit)" (T.v_none)
        , T.kv_ "67: Subterranean 2" (T.v_done)
        , T.kv_ "68: Caves" (T.v_done)
        , T.kv_ "69: Ice" (T.v_done)
        , T.kv_ "70: Another World" (T.v_done)
        , T.kv_ "71: Ruins" (T.v_done)
        , T.kv_ "72: Ruins (8-Bit)" (T.v_done)
        , T.kv_ "73: Projection Room: Above Ground" (T.v_none)
        , T.kv_ "74: Projection Room: Underground" (T.v_none)
        , T.kv_ "75: Above the Clouds" (T.v_done)
        , T.kv_ "76: Toad Brigade Member" (T.v_done)
        , T.kv_ "77: To the Next Kingdom" (T.v_done)
        , T.kv_ "78: Shop" (T.v_done)
        , T.kv_ "79: Race" (T.v_done)
        , T.kv_ "80: RC Car" (T.v_none)
        , T.kv_ "81: Spinning Slots" (T.v_done)
        , T.kv_ "82: Climactic Duel!" (T.v_done)
        ]

    )

-- \| (\d+) \| ([\w\s'’,:!]+)\|
-- , T.kv_ "$1" (T.v_done) // T.inj/det "$2"


-- \| (\d+)\s+\| ([\w\s…&'’‘,:?!-]+)\|
-- \| (\d+)\s+\| ([\w\s…&'’‘,()./:?!-]+)\|

-- \| (\d+) \| —\s+\|
-- , T.kv_ "$1" (T.v_none)

-- \s+"$
--"
