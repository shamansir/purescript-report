let T = ./Types.dhall

in
    T.collapseAt
        { id = "forza-horizon-5"
        , name = "Forza Horizon 5"
        , platform = T.Platform.Steam
        , playtime = T.Playtime.Exact { hrs = +69, min = +30, sec = +0 }
        }
        { day = +14, mon = +8, year = +2025 }

    ( T.groupStats "Statistics" [ "00-stats" ]
        [ T.kv_ "Level" (T.v_i +131)
        , T.kv_ "Nickname" (T.v_t "Elektrokiłka")
        , T.kv_ "Current Car" (T.v_t "2020 Lamborghini Huracán STO (S1 883)")
        ]
    # T.groupStats "General" [ "00-stats", "00-general" ]
        [ T.kv_ "Time Driven" (T.v_time { hrs = +30, min = +52, sec = +39 })
        , T.kv_ "Number of Podiums" (T.v_i +71)
        , T.kv_ "Number of Victories" (T.v_i +61)
        , T.kv_ "Time Spent in the First Place" (T.v_time { hrs = +2, min = +59, sec = +10 })
        , T.kv_ "Total Winnings" (T.v_mes +7378074 "CR")
        , T.kv_ "Clean Laps" (T.v_i +0)
        , T.kv_ "Number of Collisions in a Race" (T.v_i +125)
        , T.kv_ "Average Number of Collisions in a Race" (T.v_i +26)
        , T.kv_ "Number of clean overtakes" (T.v_i +365)
        , T.kv_ "Number of Cars in Garage" (T.v_i +124)
        , T.kv_ "Favorite Car" (T.v_t "Hoonigan Cossie")
        , T.kv_ "Most Successful Car" (T.v_t "Hoonigan Cossie")
        , T.kv_ "Most Valuable Car" (T.v_t "Bugatti Divo")
        , T.kv_ "Car Manufactures Owned" (T.v_i +50)
        , T.kv_ "Garage Value" (T.v_mes +41561000 "CR")
        , T.kv_ "Garage Parts Value" (T.v_mes +0 "CR")
        , T.kv_ "Total #Forzathron Points Earned" (T.v_i +2430)
        , T.kv_ "#Forzathron Daily Challenges Completed" (T.v_i +6)
        , T.kv_ "#Forzathron Weekly Challenges Completed" (T.v_i +0)
        , T.kv_ "Season Events Entered" (T.v_i +1)
        , T.kv_ "The Trial Championships Entered" (T.v_i +0)
        , T.kv_ "Favorite Radio Station" (T.v_t "Horizon Wave")
        , T.kv_ "Times Using Anna" (T.v_i +5)
        ]
    # T.groupStats "Campaign" [ "00-stats", "01-campaign" ]
        [ T.kv_ "Accolade Points Earned" (T.v_i +90300)
        , T.kv_ "Expeditions Completed" (T.v_i +4)
        , T.kv_ "Festival Outposts Unlocked" (T.v_i +2)
        , T.kv_ "Player House Owned" (T.v_pi { done = +7, total = +7 })
        , T.kv_ "Showcases Hosted" (T.v_i +1)
        , T.kv_ "Showcases Won" (T.v_pi { done = +1, total = +4 })
        , T.kv_ "Time in Showcases" (T.v_time { hrs = +0, min = +5, sec = +8 })
        , T.kv_ "Horizon Chapters Completed" (T.v_pi { done = +9, total = +65 })
        , T.kv_ "Horizon Stories Stars Earned" (T.v_pi { done = +18, total = +195 })
        , T.kv_ "Number of Races" (T.v_i +80)
        , T.kv_ "Horizon Exibitions Completed" (T.v_pi { done = +13, total = +84 })
        , T.kv_ "Head-to-Head Races Entered" (T.v_i +0)
        , T.kv_ "Head-to-Head Races Won" (T.v_i +0)
        , T.kv_ "Time in Head-to-Head Races" (T.v_time { hrs = +0, min = +0, sec = +0 })
        , T.kv_ "Street Scene Events Entered" (T.v_i +0)
        , T.kv_ "Street Scene Events Won" (T.v_pi { done = +0, total = +21 })
        , T.kv_ "Time in Street Scene Events" (T.v_time { hrs = +0, min = +0, sec = +0 })
        , T.kv_ "Midnight Battels Won" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Speed Trap Stars Earned" (T.v_pi { done = +4, total = +93 })
        , T.kv_ "Speed Zone Stars Earned" (T.v_pi { done = +3, total = +78 })
        , T.kv_ "Danger Sign Stars Earned" (T.v_pi { done = +3, total = +60 })
        , T.kv_ "Drift Zone Stars Earned" (T.v_pi { done = +0, total = +60 })
        , T.kv_ "Trailblazer Gate Stars Earned" (T.v_pi { done = +3, total = +30 })
        , T.kv_ "Speed Traps Triggered" (T.v_pi { done = +2, total = +31 })
        , T.kv_ "Speed Traps Completed" (T.v_pi { done = +2, total = +31 })
        , T.kv_ "Speed Zones Triggered" (T.v_pi { done = +1, total = +26 })
        , T.kv_ "Speed Zones Completed" (T.v_pi { done = +1, total = +26 })
        , T.kv_ "Danger Signs Triggered" (T.v_pi { done = +1, total = +20 })
        , T.kv_ "Danger Signs Completed" (T.v_pi { done = +1, total = +20 })
        , T.kv_ "Drift Zones Triggered" (T.v_pi { done = +0, total = +20 })
        , T.kv_ "Drift Zones Completed" (T.v_pi { done = +0, total = +20 })
        , T.kv_ "Trailblazer Gates Triggered" (T.v_pi { done = +1, total = +10 })
        , T.kv_ "Trailblazer Gates Completed" (T.v_pi { done = +1, total = +10 })
        ]
    # T.groupStats "Online" [ "00-stats", "02-online" ]
        [ T.kv_ "Kudos Received" (T.v_i +8)
        , T.kv_ "Kudos Given" (T.v_i +3)
        , T.kv_ "Horizon Arcade Events Entered" (T.v_i +1)
        , T.kv_ "Horizon Arcade Events Completed" (T.v_i +0)
        , T.kv_ "Flag Rush Games Entered" (T.v_i +0)
        , T.kv_ "Flag Rush Games Won" (T.v_i +0)
        , T.kv_ "Number of Flags Captured" (T.v_i +0)
        , T.kv_ "Number of Flags Stolen From You" (T.v_i +0)
        , T.kv_ "Team Flag Rush Games Entered" (T.v_i +0)
        , T.kv_ "Team Flag Rush Games Won" (T.v_i +0)
        , T.kv_ "Infected Games Entered" (T.v_i +0)
        , T.kv_ "Infected Games Won" (T.v_i +0)
        , T.kv_ "Times Infected" (T.v_i +0)
        , T.kv_ "Times You Infected Others" (T.v_i +0)
        , T.kv_ "Survival Games Entered" (T.v_i +0)
        , T.kv_ "Survival Games Won" (T.v_i +0)
        , T.kv_ "King Games Entered" (T.v_i +0)
        , T.kv_ "King Games Won" (T.v_i +0)
        , T.kv_ "Team King Games Entered" (T.v_i +0)
        , T.kv_ "Team King Games Won" (T.v_i +0)
        , T.kv_ "Rivals Beaten" (T.v_i +0)
        , T.kv_ "Credits Earned from Rivals" (T.v_mes +0 "CR")
        , T.kv_ "Laps Completed in Rivals" (T.v_i +0)
        , T.kv_ "Best Finish Position in the Eliminator" (T.v_i +0)
        , T.kv_ "Games of the Eliminator won" (T.v_i +0)
        , T.kv_ "Final Races Completed in the Eliminator" (T.v_i +0)
        , T.kv_ "Eliminations" (T.v_i +0)
        , T.kv_ "Car Drops Found in the Eliminator" (T.v_i +0)
        , T.kv_ "Legendary Cars Driven in the Eliminator" (T.v_i +0)
        , T.kv_ "Games of the Eliminator Played" (T.v_i +0)
        , T.kv_ "Time Spent in the Eliminator" (T.v_time { hrs = +0, min = +0, sec = +0 })
        , T.kv_ "Distance Driven in the Eliminator" (T.v_mes +0 "mi")
        ]
    # T.groupStats "Skills" [ "00-stats", "03-skills" ]
        [ T.kv_ "Car Perks Earned" (T.v_i +141)
        , T.kv_ "Highest Skill Score" (T.v_i +687600)
        , T.kv_ "Highest Skill Score Car" (T.v_t "Hoonigan Cossie")
        , T.kv_ "Total Skill Points Earned" (T.v_i +750)
        , T.kv_ "Total Ultimate Skills" (T.v_i +1309)
        , T.kv_ "Ultimate Drift Skills" (T.v_i +252)
        , T.kv_ "Ultimate Speed Skills" (T.v_i +36)
        , T.kv_ "Ultimate Draft Skills" (T.v_i +14)
        , T.kv_ "Ultimate Pass Skills" (T.v_i +40)
        , T.kv_ "Ultimate Burnout Skills" (T.v_i +5)
        , T.kv_ "Ultimate Air Skills" (T.v_i +595)
        , T.kv_ "Ultimate Two-Wheels Skills" (T.v_i +3)
        , T.kv_ "Ultimate E-Drift Skills" (T.v_i +1)
        , T.kv_ "Ultimate Near-Miss Skills" (T.v_i +2)
        , T.kv_ "Ultimate Wreckage Skills" (T.v_i +289)
        , T.kv_ "Ultimate One-Eighty Skills" (T.v_i +59)
        , T.kv_ "Ultimate J-Turn Skills" (T.v_i +13)
        ]
    # T.groupStats "Discovery" [ "00-stats", "04-discovery" ]
        [ T.kv_ "Unique Cars Photographed" (T.v_i +1)
        , T.kv_ "Barn Finds Discovered" (T.v_pi { done = +13, total = +14 })
        , T.kv_ "1,000 XP Boards Smashed" (T.v_pi { done = +100, total = +100 })
        , T.kv_ "3,000 XP Boards Smashed" (T.v_pi { done = +74, total = +75 })
        , T.kv_ "5,000 XP Boards Smashed" (T.v_pi { done = +12, total = +25 })
        , T.kv_ "Bonus Boards Smashed" (T.v_pi { done = +235, total = +250 })
        , T.kv_ "Fast Travel Boards Smashed" (T.v_pi { done = +49, total = +50 })
        , T.kv_ "Roads Discovered" (T.v_pi { done = +578, total = +578 })
        , T.kv_ "Landmarks Discovered" (T.v_pi { done = +56, total = +57 })
        , T.kv_ "Treasure Chests Found" (T.v_pi { done = +2, total = +2 })
        ]
    {- [ T.kv_ "Car Collection" (T.v_pi { done = +121, total = +899 })
    ,
    ] -}
    # T.groupStats "Records" [ "00-stats", "05-records" ]
        [ T.kv_ "Highest Drift Zone Score" (T.v_i +0)
        , T.kv_ "Highest Danger Sign Score" (T.v_mesd 2005.7 "ft")
        , T.kv_ "Highest Speed Trap Score" (T.v_mes +209 "mph")
        , T.kv_ "Highest Speed Zone Score" (T.v_mes +142 "mph")
        , T.kv_ "Longest Skill Chain" (T.v_time { hrs = +0, min = +3, sec = +10 })
        , T.kv_ "Top Speed" (T.v_mes +300 "mph")
        , T.kv_ "Average Speed" (T.v_mes +70 "mph")
        , T.kv_ "Distance Driven" (T.v_mes +2149 "mi")
        , T.kv_ "Biggest Air" (T.v_mesd 7.15 "s")
        , T.kv_ "Longest Drift" (T.v_mesd 856.3 "ft")
        , T.kv_ "Longest Jump" (T.v_mesd 1988.2 "ft")
        ]
    # T.groupStats "Creative Hub" [ "00-stats", "06-creative-hub" ]
        [ T.kv_ "Tuning Setups Shared" (T.v_i +0)
        , T.kv_ "Designs Shared" (T.v_i +0)
        , T.kv_ "Photos Taken" (T.v_i +0)
        , T.kv_ "Eventlab Events Played" (T.v_i +0)
        , T.kv_ "Eventlab Events Made" (T.v_i +0)
        , T.kv_ "Likes Given" (T.v_i +0)
        , T.kv_ "Likes Received" (T.v_i +0)
        ]
    # T.groupStats "Telemetry" [ "00-stats", "07-telemetry" ]
        [ T.kv_ "Max Lateral Gs" (T.v_mesd 12.9 "Gs")
        , T.kv_ "Average Lateral Gs" (T.v_mesd 0.3 "Gs")
        , T.kv_ "Max Acceleration Gs" (T.v_mesd 18.1 "Gs")
        , T.kv_ "Average Acceleration Gs" (T.v_mesd 0.3 "Gs")
        , T.kv_ "Max Braking Gs" (T.v_mesd 19.2 "Gs")
        , T.kv_ "Average Braking Gs" (T.v_mesd 0.0 "Gs")

        , T.kv_ "Max Left-Front Tire Load" (T.v_mesd 64871.8 "lbf")
        , T.kv_ "Average Left-Front Tire Load" (T.v_mesd 1152.8 "lbf")
        , T.kv_ "Max Right-Front Tire Load" (T.v_mesd 64824.4 "lbf")
        , T.kv_ "Average Right-Front Tire Load" (T.v_mesd 64712.6 "lbf")
        , T.kv_ "Max Left-Rear Tire Load" (T.v_mesd 70121.7 "lbf")
        , T.kv_ "Average Left-Rear Tire Load" (T.v_mesd 1499.5 "lbf")
        , T.kv_ "Max Right-Rear Tire Load" (T.v_mesd 70121.6 "lbf")
        , T.kv_ "Average Right-Rear Tire Load" (T.v_mesd 1487.7 "lbf")

        , T.kv_ "Max Left-Front Tire Temp" (T.v_mesd 521.9 "ºF")
        , T.kv_ "Average Left-Front Tire Temp" (T.v_mesd 118.4 "ºF")
        , T.kv_ "Max Right-Front Tire Temp" (T.v_mesd 642.3 "ºF")
        , T.kv_ "Average Right-Front Tire Temp" (T.v_mesd 119.4 "ºF")
        , T.kv_ "Max Left-Rear Tire Temp" (T.v_mesd 1422.1 "ºF")
        , T.kv_ "Average Left-Rear Tire Temp" (T.v_mesd 149.3 "ºF")
        , T.kv_ "Max Right-Rear Tire Temp" (T.v_mesd 1810.7 "ºF")
        , T.kv_ "Average Right-Rear Tire Temp" (T.v_mesd 148.0 "ºF")

        , T.kv_ "Max Left-Front Tire Suspension Stroke" (T.v_mesd 24.38 "in")
        , T.kv_ "Average Left-Front Tire Suspension Stroke" (T.v_mesd 5.70 "in")
        , T.kv_ "Max Right-Front Tire Suspension Stroke" (T.v_mesd 24.38 "in")
        , T.kv_ "Average Right-Front Tire Suspension Stroke" (T.v_mesd 5.71 "in")
        , T.kv_ "Max Left-Rear Tire Suspension Stroke" (T.v_mesd 24.27 "in")
        , T.kv_ "Average Left-Rear Tire Suspension Stroke" (T.v_mesd 5.05 "in")
        , T.kv_ "Max Right-Rear Tire Suspension Stroke" (T.v_mesd 24.27 "in")
        , T.kv_ "Average Right-Rear Tire Suspension Stroke" (T.v_mesd 5.07 "in")
        ]
    # T.groupStats "Hot Wheels" [ "00-stats", "08-hot-wheels" ]
        [ T.kv_ "Medals Earned" (T.v_pi { done = +1290, total = +5000 })
        , T.kv_ "Exhibitions Completed" (T.v_pi { done = +13, total = +18 })
        , T.kv_ "Races Completed" (T.v_i +13)
        , T.kv_ "Races Won" (T.v_i +12)
        , T.kv_ "Podium Finishes" (T.v_i +12)
        , T.kv_ "Qualifier Stars Earned" (T.v_pi { done = +0, total = +12 })
        , T.kv_ "Story Stars Earned" (T.v_pi { done = +24, total = +15 }) -- right stats, checked
        , T.kv_ "Roads Discovered" (T.v_pi { done = +175, total = +175 })
        , T.kv_ "Landmarks Discovered" (T.v_pi { done = +27, total = +28 })
        , T.kv_ "Bonus Boards Smashed" (T.v_pi { done = +25, total = +25 })
        , T.kv_ "Air Balloons Smashed" (T.v_pi { done = +3, total = +20 })
        , T.kv_ "Speed Trap Stars Earned" (T.v_pi { done = +7, total = +15 })
        , T.kv_ "Speed Zone Stars Earned" (T.v_pi { done = +8, total = +15 })
        , T.kv_ "Danger Sign Stars Earned" (T.v_pi { done = +5, total = +15 })
        , T.kv_ "Drift Zone Stars Earned" (T.v_pi { done = +3, total = +1 }) -- right stats, checked
        , T.kv_ "Seasonal Events Entered" (T.v_i +0)
        , T.kv_ "Rivals Beaten" (T.v_i +0)
        , T.kv_ "Credits Earned from Rivals" (T.v_mes +0 "CR")
        , T.kv_ "Eventlab Events Created" (T.v_i +1)
        , T.kv_ "Eventlab Events Played" (T.v_i +0)
        ]
    # T.groupStats "Rally Adventure" [ "00-stats", "09-rally-adventure" ]
        [ T.kv_ "Tournaments Completed" (T.v_i +1)
        , T.kv_ "Races Completed" (T.v_i +45)
        , T.kv_ "Races Won" (T.v_i +36)
        , T.kv_ "Podium Finishes" (T.v_i +42)
        , T.kv_ "Horizon Rally Events Entered" (T.v_i +18)
        , T.kv_ "Horizon Races Entered" (T.v_i +26)
        , T.kv_ "Ambassador Races Entered" (T.v_i +26)
        , T.kv_ "Roads Discovered" (T.v_pi { done = +32, total = +32 })
        , T.kv_ "Landmarks Discovered" (T.v_pi { done = +20, total = +20 })
        , T.kv_ "Palm Trees Smashed" (T.v_i +1113)
        , T.kv_ "Distance Driven in Water" (T.v_mes +7 "mi")
        , T.kv_ "Distance Driven on Sand" (T.v_mes +41 "mi")
        , T.kv_ "Distance Driven on Dirt" (T.v_mes +125 "mi")
        , T.kv_ "Distance Driven on Gravel" (T.v_mes +28 "mi")
        , T.kv_ "Distance Driven on Asphalt" (T.v_mes +295 "mi")
        , T.kv_ "Speed Trap Stars Earned" (T.v_pi { done = +10, total = +15 })
        , T.kv_ "Speed Zone Stars Earned" (T.v_pi { done = +5, total = +15 })
        , T.kv_ "Danger Sign Stars Earned" (T.v_pi { done = +5, total = +15 })
        , T.kv_ "Drift Zone Stars Earned" (T.v_pi { done = +1, total = +15 })
        , T.kv_ "Rivals Beaten" (T.v_i +0)
        , T.kv_ "Credits Earned from Rivals" (T.v_mes +0 "CR")
        , T.kv_ "Eventlab Events Created" (T.v_i +1)
        , T.kv_ "Eventlab Events Played" (T.v_i +0)
        ]

    # T.group "Accolades" [ "01-accolades" ]
        [ T.kv_ "Total" (T.v_pi { done = +227, total = +2237 })
        , T.kv_ "Welcome To Mexico" (T.v_pi { done = +35, total = +35 }) // T.inj/self [ "01-accolades", "00-mexico" ]
        , T.kv_ "Expeditions" (T.v_pi { done = +25, total = +54 }) // T.inj/self [ "01-accolades", "01-expeditions" ]
        , T.kv_ "Festival Playlist" (T.v_pi { done = +3, total = +83 }) // T.inj/self [ "01-accolades", "02-festival" ]
        , T.kv_ "Road Racing" (T.v_pi { done = +4, total = +136 }) // T.inj/self [ "01-accolades", "03-road" ]
        , T.kv_ "Cross Country Racing" (T.v_pi { done = +9, total = +69 }) // T.inj/self [ "01-accolades", "04-cross" ]
        , T.kv_ "Skills" (T.v_pi { done = +30, total = +77 }) // T.inj/self [ "01-accolades", "05-skills" ]
        , T.kv_ "Locked" (T.v_pi { done = +0, total = +77 })
        , T.kv_ "Locked" (T.v_pi { done = +0, total = +37 })
        , T.kv_ "Discovery & Collections" (T.v_pi { done = +39, total = +96 }) // T.inj/self [ "01-accolades", "06-discovery-and-cars" ]
        , T.kv_ "Creative" (T.v_pi { done = +4, total = +193 }) // T.inj/self [ "01-accolades", "07-creative" ]
        , T.kv_ "Hot Wheels" (T.v_pi { done = +17, total = +28 }) // T.inj/self [ "01-accolades", "08-hot-wheels" ]
        , T.kv_ "Festivals" (T.v_pi { done = +45, total = +132 }) // T.inj/self [ "01-accolades", "09-festivals" ]
        , T.kv_ "Stories" (T.v_pi { done = +10, total = +172 }) // T.inj/self [ "01-accolades", "10-stories" ]
        , T.kv_ "Evolving World" (T.v_pi { done = +1, total = +182 }) // T.inj/self [ "01-accolades", "11-evolving-world" ]
        , T.kv_ "Locked" (T.v_pi { done = +0, total = +89 })
        , T.kv_ "Locked" (T.v_pi { done = +0, total = +92 })
        , T.kv_ "Locked" (T.v_pi { done = +0, total = +100 })
        , T.kv_ "Locked" (T.v_pi { done = +0, total = +72 })
        , T.kv_ "Online" (T.v_pi { done = +3, total = +361 }) // T.inj/self [ "01-accolades", "12-online" ]
        , T.kv_ "Rally Adventure" (T.v_pi { done = +2, total = +11 }) // T.inj/self [ "01-accolades", "13-rally" ]
        ]
    # T.group "Welcome to Mexico" [ "01-accolades", "00-mexico" ] -- 35/35
        [ T.kv_ "Welcome to Mexico" (T.v_done) // (T.inj/date { mon = +12, day = +11, year = +2023 })
        , T.kv_ "First Love" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Home Sweet Home" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Ooo, what's this?" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Fast track ticket" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "This is the way" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "First few miles" (T.v_pi { done = +10, total = +10 }) // (T.inj/date { mon = +12, day = +11, year = +2023 })
        , T.kv_ "Icebreaker" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Surprise Awaits" (T.v_done) // (T.inj/date { mon = +12, day = +11, year = +2023 })
        , T.kv_ "Adaptable" (T.v_done) // (T.inj/date { mon = +12, day = +11, year = +2023 })
        , T.kv_ "Horizon Mexico Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +12, year = +2023 })
        , T.kv_ "Emerald Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Mulegé Town Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Desierto Trail Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +12, year = +2023 })
        , T.kv_ "Oasis Cross Country Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Airfield Cross Country Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Horizon Mexico Circuit Pro" (T.v_done) // (T.inj/date { mon = +12, day = +12, year = +2023 })
        , T.kv_ "Emerald Circuit Pro" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Mulegé Town Circuit Pro" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Desierto Trail Pro" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Oasis Cross Country Pro" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Airfield Cross Country Pro" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Drag Strip Novice" (T.v_pi { done = +1, total = +1 }) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Punta Allen Novice" (T.v_pi { done = +1, total = +1 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Runway Novice" (T.v_pi { done = +1, total = +1 }) // (T.inj/date { mon = +12, day = +16, year = +2023 })
        , T.kv_ "Malpaís Novice" (T.v_pi { done = +1, total = +1 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Drag Strip Expert" (T.v_pi { done = +2, total = +2 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Punta Allen Expert" (T.v_pi { done = +2, total = +2 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Runway Expert" (T.v_pi { done = +2, total = +2 }) // T.inj/date { mon = +12, day = +18, year = +2023 }
        , T.kv_ "Malpaís Expert" (T.v_pi { done = +2, total = +2 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Drag Strip Master" (T.v_pi { done = +3, total = +3 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Punta Allen Master" (T.v_pi { done = +3, total = +3 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Runway Master" (T.v_pi { done = +3, total = +3 }) // (T.inj/date { mon = +11, day = +19, year = +2023 })
        , T.kv_ "Malpaís Master" (T.v_pi { done = +3, total = +3 }) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        ]
    # T.group "Expeditions" [ "01-accolades", "01-expeditions" ] -- 25/54
        [ T.kv_ "Tulum Expedition" (T.v_pi { done = +13, total = +13 })    // T.inj/self [ "01-accolades", "01-expeditions", "00-tulum" ]
        , T.kv_ "Jungle Expedition" (T.v_pi { done = +0, total = +16 })    // T.inj/self [ "01-accolades", "01-expeditions", "01-jungle" ]
        , T.kv_ "Baja Expedition" (T.v_pi { done = +12, total = +12 })     // T.inj/self [ "01-accolades", "01-expeditions", "02-baja" ]
        , T.kv_ "Guanajuato Expedition" (T.v_pi { done = +0, total = +7 }) // T.inj/self [ "01-accolades", "01-expeditions", "03-guanajuto" ]
        , T.kv_ "Canyon Expedition" (T.v_pi { done = +0, total = +6 })     // T.inj/self [ "01-accolades", "01-expeditions", "04-canyon" ]
        ]
    # T.group "Tulum Expedition" [ "01-accolades", "01-expeditions", "00-tulum" ] -- 13/13
        [ T.kv_ "Wanderer" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Take Off" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Lost" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "And Found" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "The Cenote Book" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Eye of the Storm" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "A Storm's Brewing" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Break in the Clouds" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "New Tune" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "You Call This Archelogy?" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Wonder Awaits" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Hidden Treasures" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kv_ "Strange Things Happen in Storms Like This" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        ]
    # T.group_ "Jungle Expedition" [ "01-accolades", "01-expeditions", "01-jungle" ] -- 0/16
    # T.group "Baja Expedition" [ "01-accolades", "01-expeditions", "02-baja" ] -- 12/12
        [ T.kv_ "Extreme Activity" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Delecious Desert" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "A Change of Plan" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Driving Hazard" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Hive of Activity" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Friends Reunited" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Geronimo!" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Bajasta la vista" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "For Science" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Research" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Close Encounter" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "In the name of science" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        ]
    # T.group_ "Guanajuto Expedition" [ "01-accolades", "01-expeditions", "03-guanajuto" ] -- 0/7
    # T.group_ "Canyon Expedition" [ "01-accolades", "01-expeditions", "04-canyon" ] -- 0/6
    # T.group "Festival Playlist" [ "01-accolades", "02-festival" ] -- 3/83
        [ T.kvd_ "Daily Dozen" "Complete 12 Daily #Forzathon Challenges" (T.v_pi { done = +12, total = +12 }) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kvd_ "Lootbox" "Find a Treasure Chest" (T.v_done) // (T.inj/date { mon = +4, day = +8, year = +2025 })
        , T.kvd_ "Seasoned Veteran" "Complete at least 1 Festival Playlist activity in every season type" (T.v_pi { done = +4, total = +4 }) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kvd_ "Quench Your Thirst" "Complete 10 Festival Playlist activities during the dry season"  (T.v_pi { done = +3, total = +10 })
        , T.kvd_ "Whipping Up Storm" "Complete 10 Festival Playlist activities during the Storm season"  (T.v_pi { done = +1, total = +10 })
        , T.kvd_ "All in a Day's Work" "Complete 30 Daily #Forzathon Challenges" (T.v_pi { done = +6, total = +30 })
        , T.kvd_ "Treasure Collector" "Find 10 Treasure Chests" (T.v_pi { done = +2, total = +10 })
        , T.kvd_ "The Pioneer" "Complete 50 Festival Playlist activities"  (T.v_pi { done = +7, total = +50 })
        , T.kvd_ "Some Like it Hot" "Complete 10 Festival Playlist activities during the Hot season"  (T.v_pi { done = +1, total = +10 })
        , T.kvd_ "Get Your Feet Wet" "Complete 10 Festival Playlist activities during the Wet season"  (T.v_pi { done = +1, total = +10 })
        ]
    # T.group "Road Racing" [ "01-accolades", "03-road" ] -- 4/136
        [ T.kvd_ "Just Can't get Enough" "Complete 5 Road Racing Events" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kvd_ "A Prime Chance" "Win 3 Road Racing Events" (T.v_pi { done = +3, total = +3 }) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kvd_ "The Odd Chance" "Win 5 Road Racing Events" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        , T.kvd_ "`C` Class Road Challenger" "Complete a Road Racing Event in any `C` Class Car" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        ]
    # T.group "Cross Country" [ "01-accolades", "04-cross" ] -- 9/69
        [ T.kv_ "Estadio Cross Country Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +20, year = +2023 })
        , T.kv_ "Urban Cross Country Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 })
        , T.kv_ "Trópico Cross Country Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 })
        , T.kv_ "Restos Cross Country Circuit Rookie" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 })
        , T.kv_ "Drive into Action" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +12, day = +20, year = +2023 }) // (T.inj/det "Complete 5 Cross Country Events")
        , T.kv_ "Estadio Cross Country Circuit Pro" (T.v_done) // (T.inj/date { mon = +12, day = +20, year = +2023 })
        , T.kv_ "Urban Cross Country Circuit Pro" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 })
        , T.kv_ "Restos Cross Country Circuit Pro" (T.v_done) // (T.inj/date { mon = +12, day = +22, year = +2023 })
        , T.kv_ "Drive Rain or Shine" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +12, day = +20, year = +2023 }) // (T.inj/det "Win 5 Cross Country Events")
        ]
    # T.group "Skills" [ "01-accolades", "05-skills" ] -- 30/77
        [ T.kv_ "Master And Commander" (T.v_pi { done = +141, total = +150 }) // (T.inj/det "Unlock 150 Car Masteries")
        , T.kv_ "Perkception Awaits" (T.v_pi { done = +8, total = +10 }) // (T.inj/det "Unlock 10 Car Mastery Trees")
        , T.kv_ "Predator" (T.v_pi { done = +7, total = +25 }) // (T.inj/det "Earn 25 Slingshot Compbos in an Extreme Track Toy")
        , T.kv_ "Master Key" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "I Wanna Rock, DJ" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Eurobeat intensifies" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Flair" (T.v_pi { done = +25000, total = +25000 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Bank a Skill Chain worth at least 25,000 Skill Score")
        , T.kv_ "Killer Skills" (T.v_pd { done = +5.0, total = +5.0 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Bank a Chain with a Skill Multiplier of 5,0 or higher for the first time")
        , T.kv_ "Bonus Stage" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Take part in 5 Skill Songs")
        , T.kv_ "A Wise Investment" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Tombola" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 })
        , T.kv_ "Branching Out" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 })
        , T.kv_ "Showing Off" (T.v_pi { done = +50000, total = +50000 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Bank a Skill Chain worth at least 25,000 Skill Score")
        , T.kv_ "Prowess" (T.v_pi { done = +75000, total = +75000 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Bank a Skill Chain worth at least 75,000 Skill Score")
        , T.kv_ "Hundred Grand" (T.v_pi { done = +100000, total = +100000 }) // (T.inj/date { mon = +12, day = +22, year = +2023 }) // (T.inj/det "Bank a Skill Chain worth at least 100,000 Skill Score")
        , T.kv_ "Finesse" (T.v_pi { done = +250000, total = +250000 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Bank a Skill Chain worth at least 250,000 Skill Score")
        , T.kv_ "Jump Around" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +12, day = +22, year = +2023 }) // (T.inj/det "Earn 25 Ultimate Air Skills in any Unlimited Offroad Car")
        , T.kv_ "Thrill Seeker" (T.v_pi { done = +15, total = +15 }) // (T.inj/date { mon = +4, day = +8, year = +2025 }) // (T.inj/det "Earn 15 Near Miss Skills in any Lamborgini")
        , T.kv_ "Drifty Smash Smash" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +12, day = +22, year = +2023 }) // (T.inj/det "Earn 2 Ultimate Drift Skills and 3 Sideswipe Combos in 2 minutes")
        , T.kv_ "Double Daredevil" (T.v_pi { done = +2, total = +2 }) // (T.inj/date { mon = +4, day = +8, year = +2025 }) // (T.inj/det "Earn 2 Daredevil Combos from Near Misses within 30 seconds")
        , T.kv_ "Balancing Act" (T.v_done) // (T.inj/date { mon = +10, day = +24, year = +2024 })
        , T.kv_ "Ultimate Road Warrior" (T.v_done) // (T.inj/date { mon = +10, day = +28, year = +2024 })
        , T.kv_ "Mega Skills" (T.v_pd { done = +6.0, total = +6.0 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Bank a Skill Chain with a Skill Multiplier of 6,0 or higher for the first time")
        , T.kv_ "Giga Skills" (T.v_pd { done = +7.0, total = +7.0 }) // (T.inj/date { mon = +12, day = +22, year = +2023 }) // (T.inj/det "Bank a Skill Chain with a Skill Multiplier of 7,0 or higher for the first time")
        , T.kv_ "Choonigan" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Take part in 25 Skill Songs")
        , T.kv_ "Skillz" (T.v_pi { done = +10000000, total = +10000000 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Earn a total of 10,000,000 Skill Score")
        , T.kv_ "Flex" (T.v_pi { done = +10, total = +10 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Unlock 10 Car Masteries")
        , T.kv_ "Mega Master" (T.v_pi { done = +10, total = +10 }) // (T.inj/date { mon = +12, day = +22, year = +2023 }) // (T.inj/det "Unlock 25 Car Masteries")
        , T.kv_ "Chief of Masteries" (T.v_pi { done = +50, total = +50 }) // (T.inj/date { mon = +11, day = +17, year = +2024 }) // (T.inj/det "Unlock 50 Car Masteries")
        , T.kv_ "Car Conqueror" (T.v_pi { done = +100, total = +100 }) // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Unlock 100 Car Masteries")
        , T.kv_ "Skills" (T.v_pi { done = +500, total = +500 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Earn 500 Skills of any type and grade")
        , T.kv_ "More Skills" (T.v_pi { done = +1000, total = +1000 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Earn 1000 Skills of any type and grade")
        , T.kv_ "Moar Skills!" (T.v_pi { done = +5000, total = +5000 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Earn 5000 Skills of any type and grade")
        ]
    # T.group "Discovery & Collections" [ "01-accolades", "06-discovery-and-cars" ] -- 39/96
        [ T.kv_ "Discovery" (T.v_pi { done = +26, total = +42 }) // T.inj/self [ "01-accolades", "06-discovery-and-cars", "00-discovery" ]
        , T.kv_ "Car Collection" (T.v_pi { done = +13, total = +54 }) // T.inj/self [ "01-accolades", "06-discovery-and-cars", "01-cars" ]
        ]
    # T.group "Discovery" [ "01-accolades", "06-discovery-and-cars", "00-discovery" ] -- 26/42
        [ T.kv_ "Teleportation Device" (T.v_pi { done = +49, total = +50 }) // (T.inj/det "Smash 50 Fast Travel Boards")
        , T.kv_ "Silent Mode" (T.v_pi { done = +9, total = +15 }) // (T.inj/det "Smash 15 Phone Boxes")
        , T.kv_ "AAAARRRGGGHHH!" (T.v_pi { done = +2, total = +5 }) // (T.inj/det "Discover 5 Treasure Chests")
        , T.kv_ "Cactus Makes Perfect" (T.v_pi { done = +73, total = +500 }) // (T.inj/det "Smash 500 Cacti during the Wet Season")
        , T.kv_ "GOALLL" (T.v_pi { done = +1, total = +25 }) // (T.inj/det "Smash 25 Football Goal Posts")
        , T.kv_ "Total Eclipse" (T.v_pi { done = +2, total = +100 }) // (T.inj/det "Smash 100 Solar Panels in the 1995 Mitsubishi Eclipse GSX")
        , T.kv_ "Hunter-Gatherer" (T.v_pi { done = +10, total = +10 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Find and Smash 10 Bonus Boards")
        , T.kv_ "Boards Go Smash" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Find and Smash 25 XP Boards")
        , T.kv_ "Cheaper, Better, Faster, Travel" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +12, day = +2, year = +2024 }) // (T.inj/det "Find and Smash 25 Fast Travel Boards")
        , T.kv_ "AHOY!" (T.v_done_) // (T.inj/date { mon = +4, day = +8, year = +2025 }) // (T.inj/det "Discover your first Treasure Chest")
        , T.kv_ "Dry Spell" (T.v_done_) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Experience Dry Season in Freeroam")
        , T.kv_ "Hot, Hot, Hot!" (T.v_done_) // (T.inj/date { mon = +12, day = +2, year = +2024 }) // (T.inj/det "Experience Hot Season in Freeroam")
        , T.kv_ "Weather the Storm" (T.v_done_) // (T.inj/date { mon = +1, day = +8, year = +2025 }) // (T.inj/det "Experience Storm Season in Freeroam")
        , T.kv_ "On The Road Again" (T.v_pi { done = +50, total = +50 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover 50 Roads in Mexico")
        , T.kv_ "Not Half Bad" (T.v_pi { done = +50, total = +50 }) // (T.inj/date { mon = +11, day = +19, year = +2024 }) // (T.inj/det "Find and Smash 50 XP Boards")
        , T.kv_ "Going Overboard" (T.v_pi { done = +100, total = +100 }) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Find and Smash 50 XP Boards")
        , T.kv_ "Triple Strike" (T.v_pi { done = +30, total = +30 }) // (T.inj/date { mon = +12, day = +22, year = +2023 }) // (T.inj/det "Smash 30 Traffic Cones")
        , T.kv_ "Must Destroy All Spiky Things" (T.v_pi { done = +100, total = +100 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Smash 100 Cacti")
        , T.kv_ "Room to Wander" (T.v_pi { done = +100, total = +100 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover 100 Roads in Mexico")
        , T.kv_ "Level Up" (T.v_pi { done = +200, total = +200 }) // (T.inj/date { mon = +11, day = +14, year = +2025 }) // (T.inj/det "Smash 200 XP Boards")
        , T.kv_ "The Start of Many" (T.v_mesd 50.0 "miles") // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Drive 50 Miles (80.5km)")
        , T.kv_ "I like to Drive It, Drive it" (T.v_mesd 100.0 "miles") // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Drive 100 Miles (161km)")
        , T.kv_ "The Complete Collection" (T.v_pi { done = +250, total = +250 }) // (T.inj/date { mon = +1, day = +14, year = +2025 }) // (T.inj/det "Smash 250 Bonus Boards")
        , T.kv_ "Right Up My Street" (T.v_pi { done = +200, total = +200 }) // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Discover 200 Roads in Mexico")
        , T.kv_ "Seing The Sights" (T.v_pi { done = +300, total = +300 }) // (T.inj/date { mon = +12, day = +2, year = +2024 }) // (T.inj/det "Discover 300 Roads in Mexico")
        , T.kv_ "Highways and Byways" (T.v_pi { done = +400, total = +400 }) // (T.inj/date { mon = +1, day = +8, year = +2025 }) // (T.inj/det "Discover 400 Roads in Mexico")
        , T.kv_ "El Explorador" (T.v_pi { done = +14, total = +14 }) // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Discover every region of Mexico")
        , T.kv_ "Discovery Connoisseur" (T.v_done_) // (T.inj/date { mon = +1, day = +15, year = +2025 }) // (T.inj/det "Discover all points of interest")
        , T.kv_ "On The Move" (T.v_mesd 250.0 "miles") // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Drive 250 Miles (402.3km)")
        , T.kv_ "Ride and Seek" (T.v_pi { done = +578, total = +578 }) // (T.inj/date { mon = +1, day = +16, year = +2025 }) // (T.inj/det "Discover all Roads in Mexico") -- 100 percent
        , T.kv_ "Just Keep Driving, Driving, Driving" (T.v_mesd 500.0 "miles") // (T.inj/date { mon = +10, day = +23, year = +2024 }) // (T.inj/det "Drive 500 Miles (804.7km)")
        , T.kv_ "Going the Extra Mile" (T.v_mesd 1000.0 "miles") // (T.inj/date { mon = +11, day = +19, year = +2024 }) // (T.inj/det "Drive 1000 Miles (1610km)")
        ]
    # T.group "Car Collection" [ "01-accolades", "06-discovery-and-cars", "01-cars" ] -- 13/54
        [ T.kv_ "Better than New" (T.v_pi { done = +12, total = +14 }) // (T.inj/det "Restore 14 Barns Finds")
        , T.kv_ "Honorary Owner" (T.v_pi { done = +34, total = +50 }) // (T.inj/det "Own 50 Unique Rare Cars")
        , T.kv_ "The Legendary Collection" (T.v_pi { done = +30, total = +50 }) // (T.inj/det "Own 50 Unique Legendary Cars")
        , T.kv_ "Car Museum" (T.v_pi { done = +28, total = +50 }) // (T.inj/det "Own 50 Unique Epic Cars")
        , T.kv_ "A Dime A Dozen" (T.v_pi { done = +25, total = +50 }) // (T.inj/det "Own 50 Unique Common Cars") -- +26 -- 08/14/2025
        , T.kv_ "Playing for Keeps" (T.v_pi { done = +120, total = +250 }) // (T.inj/det "Own 250 Unique Cars") -- +121 -- 08/14/2025
        , T.kv_ "Special Edition" (T.v_pi { done = +2, total = +5 }) // (T.inj/det "Own 5 Unique Forza Edition Cars")
        , T.kv_ "Completitionist" (T.v_pi { done = +15, total = +50 }) // (T.inj/det "Earn 50 Manufacturer Bonuses")
        , T.kv_ "Bulk Buying" (T.v_pi { done = +3, total = +150 }) // (T.inj/det "Buy 150 Cars from the Autoshow")
        , T.kv_ "Bare Necessities" (T.v_pi { done = +10, total = +10 }) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Own 10 Unique Rare Cars")
        , T.kv_ "Rare Find" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Own a Rare Car")
        , T.kv_ "Get Off to an Epic Start" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Own an Epic Car")
        , T.kv_ "Drive The Legend" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Own a Legendary Car")
        , T.kv_ "A Fine Addition" (T.v_done) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Own a Forza Edition Car")
        , T.kv_ "Manufacturer Affinity" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Earn a Manufacturer Bonus")
        , T.kv_ "A Forza Edition to my Collection" (T.v_done) // (T.inj/date { mon = +11, day = +17, year = +2024 }) // (T.inj/det "Earn a Forza Edition Car via a Wheelspin")
        , T.kv_ "Hidden Gem" (T.v_done) // (T.inj/date { mon = +12, day = +21, year = +2023 }) // (T.inj/det "Restore a Barn Find Car")
        , T.kv_ "Fine Selection" (T.v_pi { done = +50, total = +50 }) // (T.inj/date { mon = +10, day = +24, year = +2024 }) // (T.inj/det "Own 50 Unique Cars")
        , T.kv_ "World Famous" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +1, day = +7, year = +2025 }) // (T.inj/det "Own 25 unique Legendary Cars")
        , T.kv_ "Rediscovered Treasure" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Restore 5 Barn Find Cars")
        , T.kv_ "Factory Fresh" (T.v_pi { done = +15, total = +15 }) // (T.inj/date { mon = +12, day = +2, year = +2024 }) // (T.inj/det "Earn 15 Manufacturer Bonuses")
        , T.kv_ "Spoiled for Choice" (T.v_pi { done = +100, total = +100 }) // (T.inj/date { mon = +11, day = +28, year = +2024 }) // (T.inj/det "Own 100 Unique Cars")
        ]
    # T.group "Creative" [ "01-accolades", "07-creative" ] -- 4/193
        [ T.kv_ "Painting" (T.v_pi { done = +0, total = +44 })   // T.inj/self [ "01-accolades", "07-creative", "00-painting" ]
        , T.kv_ "Tuning" (T.v_pi { done = +0, total = +27 })     // T.inj/self [ "01-accolades", "07-creative", "01-tuning" ]
        , T.kv_ "Test Track" (T.v_pi { done = +0, total = +15 }) // T.inj/self [ "01-accolades", "07-creative", "02-test-track" ]
        , T.kv_ "Photo" (T.v_pi { done = +1, total = +72 })      // T.inj/self [ "01-accolades", "07-creative", "03-photo" ]
        , T.kv_ "Event Lab" (T.v_pi { done = +3, total = +20 })  // T.inj/self [ "01-accolades", "07-creative", "04-eventlab" ]
        , T.kv_ "Super 7" (T.v_pi { done = +0, total = +15 })    // T.inj/self [ "01-accolades", "07-creative", "05-super7" ]
        ]
    # T.group_ "Painting" [ "01-accolades", "07-creative", "00-painting" ] -- 0/44
    # T.group "Tuning" [ "01-accolades", "07-creative", "01-tuning" ] -- 0/27
        [ T.kv_ "The Upgrader" (T.v_pi { done = +2, total = +5 })
        , T.kv_ "The Tuner" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "The Ultra Upgrader" (T.v_pi { done = +2, total = +20 })
        , T.kv_ "The Ultra Tuner" (T.v_pi { done = +1, total = +10 })
        ]
    # T.group_ "Test Track" [ "01-accolades", "07-creative", "02-test-track" ] -- 0/15
    # T.group "Photo" [ "01-accolades", "07-creative", "03-photo" ] -- 1/72
        [ T.kv_ "Snap!" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Say Cheese" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 })
        ]
    # T.group "Event Lab" [ "01-accolades", "07-creative", "04-eventlab" ] -- 3/20
        [ T.kv_ "Mix and Match!" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Architect of Fun" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Pathfinder" (T.v_done) // (T.inj/date { mon = +11, day = +24, year = +2024 })
        , T.kv_ "Show me Your Moves" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 })
        , T.kv_ "Creative Spark" (T.v_done) // (T.inj/date { mon = +11, day = +24, year = +2024 })
        ]
    # T.group_ "Super7" [ "01-accolades", "07-creative", "05-super7" ] -- 0/15
    # T.group "Hot Wheels" [ "01-accolades", "08-hot-wheels" ] -- 17/28
        [ T.kv_ "Over Qualified" (T.v_pi { done = +9, total = +12 }) // (T.inj/det "Earn 3 Stars from each of the Hot Wheels Qualifier Events")
        , T.kv_ "Altitude Quickness" (T.v_pi { done = +12, total = +18 }) // (T.inj/det "Win All Hot Wheels Race Events")
        , T.kv_ "Missions Complete" (T.v_pi { done = +65, total = +155 }) // (T.inj/det "Complete All Missions")
        , T.kv_ "Hot Wheels All Star" (T.v_pi { done = +23, total = +60 }) // (T.inj/det "Earn 3 Stars from every PR Stunt in Hot Wheels Park")
        , T.kv_ "Gotta Pop 'Em All" (T.v_pi { done = +3, total = +10 }) // (T.inj/det "Smash all of the Tank Balloons in Hot Wheels Park")
        , T.kv_ "No one expects the hot wheels expedition" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Arrive at the Horizon Hot Wheels Outpost")
        , T.kv_ "High Roller" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Spin 25 Wheelspins while in Hot Wheels Park")
        , T.kv_ "The Rookie" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kv_ "Pros, no Cons" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kv_ "I am an expert now" (T.v_done) // (T.inj/date { mon = +11, day = +24, year = +2024 })
        , T.kv_ "Missions" (T.v_pi { done = +5, total = +5 }) // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Complete 5 Missions")
        , T.kv_ "More Missions" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Complete 25 Missions")
        , T.kv_ "Soared and Board" (T.v_pi { done = +25, total = +25 }) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Smash All Bonus Boards in Hot Wheels Park")
        , T.kv_ "Fresh Pressed Orange Routes" (T.v_pct_done) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Discover every road in Hot Wheels Park")
        , T.kv_ "Lessons in Hot Wheels History" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kv_ "Major in Hot Wheels History" (T.v_done) // (T.inj/date { mon = +11, day = +24, year = +2024 })
        , T.kv_ "Professor of Hot Wheels History" (T.v_done) // (T.inj/date { mon = +11, day = +24, year = +2024 })
        , T.kv_ "Aquanaut" (T.v_done) // (T.inj/date { mon = +11, day = +23, year = +2024 })
        , T.kv_ "Icy Road Ahead" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kv_ "Shaken not third" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kv_ "Attracted to Victory" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        , T.kv_ "Points means Prizes" (T.v_done) // (T.inj/date { mon = +11, day = +20, year = +2024 })
        ]
    # T.group "Festivals" [ "01-accolades", "09-festivals" ] -- 45/132
        [ T.kv_ "Horizon Festival Mexico" (T.v_pi { done = +22, total = +28 })     // T.inj/self [ "01-accolades", "09-festivals", "00-mexico" ]
        , T.kv_ "Horizon Apex Outpost" (T.v_pi { done = +12, total = +23 })        // T.inj/self [ "01-accolades", "09-festivals", "01-apex" ]
        , T.kv_ "Horizon Wilds Outpost" (T.v_pi { done = +0, total = +23 })        // T.inj/self [ "01-accolades", "09-festivals", "02-wilds" ]
        , T.kv_ "Horizon Baja Outpost" (T.v_pi { done = +11, total = +24 })        // T.inj/self [ "01-accolades", "09-festivals", "03-baja" ]
        , T.kv_ "Horizon Street Scene Outpost" (T.v_pi { done = +0, total = +18 }) // T.inj/self [ "01-accolades", "09-festivals", "04-streets" ]
        , T.kv_ "Horizon Rush Outpost" (T.v_pi { done = +0, total = +18 })         // T.inj/self [ "01-accolades", "09-festivals", "05-rush" ]
        ]
    # T.group "Horizon Festival Mexico" [ "01-accolades", "09-festivals", "00-mexico" ] -- 17/28
        [ T.kv_ "Tulum Expedition" (T.v_done)  // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Unlock Tulum Expedition")
        , T.kv_ "Jungle Expedition" (T.v_done)  // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Unlock Jungle Expedition")
        , T.kv_ "Baja Expedition" (T.v_done)  // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Unlock Baja Expedition")
        , T.kv_ "Guanajuato Expedition" (T.v_done)  // (T.inj/date { mon = +12, day = +23, year = +2023 }) // (T.inj/det "Unlock Guanajuato Expedition")
        , T.kv_ "Far from the Mudding Crowd" (T.v_done)  // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Build the Horizon Apex Festival Outpost")
        , T.kv_ "There's always money in the Baja Stand" (T.v_done)  // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Build the Horizon Baja Festival Outpost")
        , T.kv_ "Initial Introductions" (T.v_done)  // (T.inj/date { mon = +1, day = +7, year = +2025 }) // (T.inj/det "Unlock the `Vocho` Horizon Story at Horizon Festival Mexico")
        , T.kv_ "Goliath Cometh" (T.v_done)  // (T.inj/date { mon = +11, day = +19, year = +2024 }) // (T.inj/det "Unlock the Goliath")
        , T.kv_ "Big Deal" (T.v_done)  // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Complete the Goliath")
        , T.kv_ "Long Gone" (T.v_done)  // (T.inj/date { mon = +11, day = +20, year = +2024 }) // (T.inj/det "Win the Goliath")
        , T.kv_ "Volcanic Activity" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover La Gran Caldera")
        , T.kv_ "Living Up in the Desert" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Discover Baja California")
        , T.kv_ "Talk of the Town" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Discover the Town of Mulegé")
        , T.kv_ "Dam it" (T.v_done) // (T.inj/date { mon = +1, day = +7, year = +2025 }) // (T.inj/det "Discover Sierra Verde Dam")
        , T.kv_ "The Buried City" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover San Juan")
        , T.kv_ "Stargazer" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover the Gran Telescopio at the top of Volcano")
        , T.kv_ "In Deep Waters" (T.v_done) // (T.inj/date { mon = +1, day = +7, year = +2025 }) // (T.inj/det "Discover the Temple of Quechula in the river")
        , T.kv_ "First Contact" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover the De Otro Mundo crop circles")
        , T.kv_ "Hole io One" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Discover the Club de Ópalo de Fuego on the West Coast")
        , T.kv_ "Cultural Heritage" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Discover the Misión Santa Rosalía de Mulegé")
        , T.kv_ "I'll Take the Ocean View Suite" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Discover Palacio Azul del Océano on the West Coast")
        , T.kv_ "On the Rocks" (T.v_done) // (T.inj/date { mon = +12, day = +16, year = +2023 }) // (T.inj/det "Discover Descansar Dorado")
        ]
    # T.group "Horizon Apex Outpost" [ "01-accolades", "09-festivals", "01-apex" ] -- 12/23
        [ T.kv_ "An Architectural Wonder" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Teotihuacán")
        , T.kv_ "Make An Entrance" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover the Arch of Mulegé")
        , T.kv_ "Swamp Creature" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Pantano de la Selva")
        , T.kv_ "Day at the Beach" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Riviera Maya")
        , T.kv_ "I can see Clearly Plow" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Tierra Próspera Farmlands")
        , T.kv_ "Down by the Coast" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover the East Coast of Playa Azul")
        , T.kv_ "Discover Farid Rueda" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Farid Rueda's Bear Mural in Playa Azul")
        , T.kv_ "Discover Farid Rueda Again" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Farid Rueda's Lion Mural in Playa Azul")
        , T.kv_ "Home Comforts" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover the Town of Los Jardines")
        , T.kv_ "Land of Colors" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Granjas de Tapalpa farmland")
        , T.kv_ "Paradise Found" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Río de la Selva in the Swamp Region")
        , T.kv_ "City of Dawn" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Discover Tulum on the East Coast")
        ]
    # T.group_ "Horizon Wilds Outpost" [ "01-accolades", "09-festivals", "02-wilds" ] -- 0/23
    # T.group "Horizon Baja Outpost" [ "01-accolades", "09-festivals", "03-baja" ] -- 11/24
        [ T.kv_ "No Place Like Home" (T.v_done) // (T.inj/date { mon = +12, day = +23, year = +2023 }) // (T.inj/det "Own La Cabaña Player House")
        , T.kv_ "Beast Mode" (T.v_done) // (T.inj/date { mon = +8, day = +11, year = +2025 }) // (T.inj/det "Unlock the Buggy and the Beast Showcase Event")
        , T.kv_ "Close Up Time" (T.v_done) // (T.inj/date { mon = +11, day = +24, year = +2024 }) // (T.inj/det "Unlock the `V10` Horizon Story at Horizon Baja")
        , T.kv_ "Big Leagues" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 }) // (T.inj/det "Unlock the Titan")
        , T.kv_ "Off Track Mind" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 }) // (T.inj/det "Unlock all content associated with the Cross Country Racing Festival")
        , T.kv_ "With a Pinch of Salt" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover Lago Blanco")
        , T.kv_ "I don't like Sand it gets everywhere" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover Dunas Blancas")
        , T.kv_ "Going Coastal" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover the Costa Rocosa")
        , T.kv_ "All About that Dust" (T.v_done) // (T.inj/date { mon = +12, day = +18, year = +2023 }) // (T.inj/det "Discover the Baja Circuit in the Desert Region")
        , T.kv_ "The Coast is Clear" (T.v_done) // (T.inj/date { mon = +12, day = +23, year = +2023 }) // (T.inj/det "Discover Bahía de Plano on the West Coast")
        , T.kv_ "Land's End" (T.v_done) // (T.inj/date { mon = +1, day = +6, year = +2025 }) // (T.inj/det "Discover El Arco de Cobo San Lucas on the West Coast")
        ]
    # T.group_ "Horizon Street Scene Outpost" [ "01-accolades", "09-festivals", "04-streets" ] -- 0/18
    # T.group_ "Horizon Rush Outpost" [ "01-accolades", "09-festivals", "05-rush" ] -- 0/18
    # T.group "Stories" [ "01-accolades", "10-stories" ] -- 10/172
        [ T.kv_ "El Camino" (T.v_pi { done = +0, total = +18 }) // T.inj/self [ "01-accolades", "10-stories", "00-el-camino" ]
        , T.kv_ "Vocho" (T.v_pi { done = +1, total = +15 }) // T.inj/self [ "01-accolades", "10-stories", "01-vocho"  ]
        , T.kv_ "Test Driver" (T.v_pi { done = +0, total = +32 }) // T.inj/self [ "01-accolades", "10-stories",  "02-test-driver" ]
        , T.kv_ "Lucha de Carreteras" (T.v_pi { done = +0, total = +17 }) // T.inj/self [ "01-accolades", "10-stories", "03-lucha" ]
        , T.kv_ "Born Fast" (T.v_pi { done = +0, total = +9 }) // T.inj/self [ "01-accolades", "10-stories", "04-born-fast" ]
        , T.kv_ "V10" (T.v_pi { done = +0, total = +13 }) // T.inj/self [ "01-accolades", "10-stories", "05-v10" ]
        , T.kv_ "Donut Media Hi Low" (T.v_pi { done = +9, total = +20 }) // T.inj/self [ "01-accolades", "10-stories", "06-donut-media-hi-low" ]
        , T.kv_ "Donut Media @ Horizon" (T.v_pi { done = +0, total = +7 }) // T.inj/self [ "01-accolades", "10-stories", "07-donut-media-horizon" ]
        , T.kv_ "Horizon Origins" (T.v_pi { done = +0, total = +12 }) // T.inj/self [ "01-accolades", "10-stories", "08-horizon-origins" ]
        , T.kv_ "Made in Mexico" (T.v_pi { done = +0, total = +10 }) // T.inj/self [ "01-accolades", "10-stories", "09-made-in-mexico" ]
        , T.kv_ "Drift Club" (T.v_pi { done = +0, total = +10 }) // T.inj/self [ "01-accolades", "10-stories", "10-drift-club" ]
        , T.kv_ "Icons of Speed" (T.v_pi { done = +0, total = +9 }) // T.inj/self [ "01-accolades", "10-stories", "11-icons-of-speed" ]
        ]
    # T.group_ "El Camino" [ "01-accolades", "10-stories", "00-el-camino" ] -- 0/18
    # T.group "Vocho" [ "01-accolades", "10-stories", "01-vocho" ] -- 1/15
        [ T.kv_ "Flexibility Beats Everything" (T.v_pi { done = +3, total = +39 }) // (T.inj/det "Earn 3 Stas in all chapters of Vocho")
        , T.kv_ "El Vocho" (T.v_done) // (T.inj/date { mon = +1, day = +7, year = +2025 }) // (T.inj/det "Complete `The Vocho` chapter of Vocho")
        ]
    # T.group_ "Test Driver" [ "01-accolades", "10-stories", "02-test-driver" ] -- 0/32
    # T.group_ "Lucha de Carreteras" [ "01-accolades", "10-stories", "03-lucha" ] -- 0/17
    # T.group_ "Born Fast" [ "01-accolades", "10-stories", "04-born-fast" ] -- 0/9
    # T.group_ "V10" [ "01-accolades", "10-stories", "05-v10" ] -- 0/13
    # T.group "Donut Media Hi Low" [ "01-accolades", "10-stories", "06-donut-media-hi-low" ] -- 9/20
        [ T.kv_ "Hi Performance" (T.v_pi { done = +11, total = +18 }) // (T.inj/det "Earn 3 Stas in all chapters of `Donut Media Hi Team`")
        , T.kv_ "On the Road Again" (T.v_done) // (T.inj/date { mon = +11, day = +28, year = +2024 })
        , T.kv_ "Well, Hi There!" (T.v_done) // (T.inj/date { mon = +12, day = +3, year = +2024 })
        , T.kv_ "Under 3 Minutes or it's Free" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 })
        , T.kv_ "Just a Little Further!" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 })
        , T.kv_ "History of Z" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 })
        , T.kv_ "No Pain No Gain" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 })
        , T.kv_ "Putting it to the Test" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 })
        , T.kv_ "High Five" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 })
        , T.kv_ "Generational Speed" (T.v_done) // (T.inj/date { mon = +1, day = +15, year = +2025 }) // (T.inj/det "Maintain a speed of 120mph for more than 20 seconds in the Up to Speed chapter")
        ]
    # T.group_ "Donut Media @ Horizon" [ "01-accolades", "10-stories", "07-donut-media-horizon" ] -- 0/7
    # T.group_ "Horizon Origins" [ "01-accolades", "10-stories", "08-horizon-origins" ] -- 0/12
    # T.group_ "Made in Mexico" [ "01-accolades", "10-stories", "09-made-in-mexico" ] -- 0/10
    # T.group_ "Drift Club" [ "01-accolades", "10-stories", "10-drift-club" ] -- 0/10
    # T.group_ "Icons of Speed" [ "01-accolades", "10-stories", "11-icons-of-speed" ] -- 0/9
    # T.group "Evolving World" [ "01-accolades", "11-evolving-world" ] -- 1/182  -- TODO
        [ T.kv_ "Horizon Realms" (T.v_pi { done = +0, total = +63 }) // T.inj/self [ "01-accolades", "11-evolving-world", "00-horizon-realms" ]
        , T.kv_ "Evolving World" (T.v_pi { done = +0, total = +20 }) // T.inj/self [ "01-accolades", "11-evolving-world", "01-evolving-world" ]
        , T.kv_ "New Cars" (T.v_pi { done = +0, total = +42 })       // T.inj/self [ "01-accolades", "11-evolving-world", "02-new-cars" ]
        , T.kv_ "Pathfinder" (T.v_pi { done = +0, total = +17 })     // T.inj/self [ "01-accolades", "11-evolving-world", "03-pathfinder" ]
        , T.kv_ "Oval Track" (T.v_pi { done = +1, total = +20 })     // T.inj/self [ "01-accolades", "11-evolving-world", "04-oval-track" ]
        , T.kv_ "Hoonigan" (T.v_pi { done = +0, total = +20 })       // T.inj/self [ "01-accolades", "11-evolving-world", "05-hoonigan" ]
        ]
    # T.group_ "Horizon Realms" [ "01-accolades", "11-evolving-world", "00-horizon-realms" ] -- 0/63
    # T.group_ "Evolving World" [ "01-accolades", "11-evolving-world", "01-evolving-world" ] -- 0/20
    # T.group_ "New Cars" [ "01-accolades", "11-evolving-world", "02-new-cars" ] -- 0/42
    # T.group_ "Pathfinder" [ "01-accolades", "11-evolving-world", "03-pathfinder" ] -- 0/17
    # T.group "Oval Track" [ "01-accolades", "11-evolving-world", "04-oval-track" ] -- 1/20
        [ T.kv_ "An Ellipse of the Heart" (T.v_done) // (T.inj/date { mon = +11, day = +28, year = +2024 }) // (T.inj/det "Visit the Oval Track at the Stadium")
        ]
    # T.group_ "Hoonigan" [ "01-accolades", "11-evolving-world", "05-hoonigan" ] -- 0/20
    # T.group "Online" [ "01-accolades", "12-online" ] -- 3/361
        [ T.kv_ "Horizon Arcade" (T.v_pi { done = +2, total = +70 }) // T.inj/self [ "01-accolades", "12-online", "00-horizon-arcade" ]
        , T.kv_ "Horizon Tour" (T.v_pi { done = +0, total = +66 })   // T.inj/self [ "01-accolades", "12-online", "01-horizont-tour" ]
        , T.kv_ "The Eliminator" (T.v_pi { done = +0, total = +34 }) // T.inj/self [ "01-accolades", "12-online", "02-eliminator" ]
        , T.kv_ "Hide & Seek" (T.v_pi { done = +0, total = +10 })    // T.inj/self [ "01-accolades", "12-online", "03-hide-and-seek" ]
        , T.kv_ "Horizon Open" (T.v_pi { done = +1, total = +41 })   // T.inj/self [ "01-accolades", "12-online", "04-horizon-open" ]
        , T.kv_ "Rivals" (T.v_pi { done = +0, total = +140 })        // T.inj/self [ "01-accolades", "12-online", "05-rivals" ]
        ]
    # T.group "Horizon Arcade" [ "01-accolades", "12-online", "00-horizon-arcade" ] -- 2/70
        [ T.kv_ "Finding Choku Dori" (T.v_pi { done = +1, total = +2 }) // (T.inj/det "Earn 2 Ultimate Drift Skills during a Drift Run in a Horizon Arcade Event")
        , T.kv_ "Angle Grinder" (T.v_pi { done = +1, total = +3 }) // (T.inj/det "Complete 3 Drift Rounds in Horizon Arcade Events")
        , T.kv_ "Taking Five" (T.v_pi { done = +1, total = +5 }) // (T.inj/det "Take part in 5 Horizon Arcade Events and complete at least round one")
        , T.kv_ "Keeping Up Appearances" (T.v_pi { done = +1, total = +10 }) // (T.inj/det "Take part in 10 Horizon Arcade Events and complete at least round one")
        , T.kv_ "DK Country" (T.v_done) // ((T.inj/date { mon = +12, day = +18, year = +2023 }) // T.inj/det "Complete a Drift theme Horizon Arcade Event")
        , T.kv_ "Dori Time" (T.v_done) // ((T.inj/date { mon = +12, day = +18, year = +2023 }) // T.inj/det "Complete a Drift Run round in a Horizon Arcade Event")
        ]
    # T.group_ "Horizon Tour" [ "01-accolades", "12-online", "01-horizont-tour" ] -- 0/66
    # T.group_ "The Eliminator" [ "01-accolades", "12-online", "02-eliminator" ] -- 0/34
    # T.group_ "Hide & Seek" [ "01-accolades", "12-online", "03-hide-and-seek" ] -- 0/10
    # T.group "Horizon Open" [ "01-accolades", "12-online", "04-horizon-open" ] -- 1/41
        [ T.kv_ "The Grand Opening" (T.v_pi { done = +1, total = +10 }) // (T.inj/det "Earn 10 Levels in Horizon Open")
        , T.kv_ "Badge of Honour" (T.v_done) // ((T.inj/date { mon = +12, day = +18, year = +2023 }) // T.inj/det "Earn a Badge")
        ]
    # T.group_ "Rivals" [ "01-accolades", "12-online", "05-rivals" ] -- 0/140
    # T.group "Rally Adventure" [ "01-accolades", "13-rally" ] -- 2/11
        [ T.kv_ "Horizon Badlands Race Champion" (T.v_pi { done = +21, total = +24 }) // (T.inj/det "Win all Horizon Race Events in Sierra Nueva")
        , T.kv_ "Apex Predators Legend" (T.v_pi { done = +33, total = +49 }) // (T.inj/det "Complete every Apex Predators Challenge")
        , T.kv_ "Horizon Raptors Legend" (T.v_pi { done = +33, total = +49 }) // (T.inj/det "Complete every Horizon Raptors Challenge")
        , T.kv_ "Gift Reapers Legend" (T.v_pi { done = +30, total = +49 }) // (T.inj/det "Complete every Gift Reapers Challenge")
        , T.kv_ "Horizon Badlands Rally Champion" (T.v_pi { done = +10, total = +24 }) // (T.inj/det "Win all Horizon Rally Events in Sierra Nueva")
        , T.kv_ "Sierra Nueva PR Stunt Champion" (T.v_pi { done = +21, total = +60 }) // (T.inj/det "Earn all Stars from PR Stunts in Sierra Nueva")
        , T.kv_ "Taking in the Sights" (T.v_pct_done) // (T.inj/date { mon = +10, day = +27, year = +2024 }) // (T.inj/det "Discover all Landmarks in Sierra Nueva")
        , T.kv_ "Recce Complete" (T.v_pct_done) // (T.inj/date { mon = +11, day = +19, year = +2024 }) // (T.inj/det "Discover all Roads in Sierra Nueva")
        ]
    # T.group "Badges" [ "02-badges" ] -- 2/81
        [ T.kv_ "The Grand Opening" (T.v_pi { done = +1, total = +10 }) // (T.inj/det "Reach Level 10 in Horizon Open")
        , T.kv_ "Horizon Open Newcomer" (T.v_pi { done = +1, total = +100 }) // (T.inj/det "Reach Level 100 in Horizon Open")
        , T.kv_ "Horizon Open Rookie" (T.v_pi { done = +1, total = +200 }) // (T.inj/det "Reach Level 200 in Horizon Open")
        , T.kv_ "Horizon Open Amateur" (T.v_pi { done = +1, total = +300 }) // (T.inj/det "Reach Level 300 in Horizon Open")
        , T.kv_ "Horizon Open Enthusiast" (T.v_pi { done = +1, total = +400 }) // (T.inj/det "Reach Level 400 in Horizon Open")
        , T.kv_ "Horizon Open Expert" (T.v_pi { done = +1, total = +500 }) // (T.inj/det "Reach Level 500 in Horizon Open")
        , T.kv_ "Horizon Open Pro" (T.v_pi { done = +1, total = +600 }) // (T.inj/det "Reach Level 600 in Horizon Open")
        , T.kv_ "Horizon Open Elite" (T.v_pi { done = +1, total = +700 }) // (T.inj/det "Reach Level 700 in Horizon Open")
        , T.kv_ "Horizon Open Specialist" (T.v_pi { done = +1, total = +800 }) // (T.inj/det "Reach Level 800 in Horizon Open")
        , T.kv_ "Horizon Open Master" (T.v_pi { done = +1, total = +900 }) // (T.inj/det "Reach Level 900 in Horizon Open")
        , T.kv_ "Horizon Open Champion" (T.v_pi { done = +1, total = +1000 }) // (T.inj/det "Reach Level 1000 in Horizon Open")
        , T.kv_ "You're the Boss" (T.v_done) // (T.inj/det "Play Forza Horizon 5 during the Dry Season of the `Horizon 10 Year Annivarsary`")
        , T.kv_ "Spring into Action" (T.v_done) // (T.inj/det "Play Forza Horizon 5 during the Hot Season of the `Horizon 10 Year Annivarsary`")
        ]
    # T.group "Car Collection" [ "03-car-collection" ]
        [ T.kv_ "Total" (T.v_pi { done = +121, total = +899 })
        , T.kv_ "Abarth" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Acura" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Alfa Romeo" (T.v_pi { done = +0, total = +7 })
        , T.kv_ "Alpine" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Alumicraft" (T.v_pi { done = +2, total = +3 })
        , T.kv_ "AMC" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "AMG Transport Dynamic" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Apollo" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Ariel" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Ascari" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Aston Martin" (T.v_pi { done = +1, total = +16 })
        , T.kv_ "ATS" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Audi" (T.v_pi { done = +3, total = +25 })
        , T.kv_ "Austin-Healey" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Automobile Pinifarina" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Autozam" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "BAC" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Bentley" (T.v_pi { done = +0, total = +7 })
        , T.kv_ "BMW" (T.v_pi { done = +8, total = +37 })
        , T.kv_ "Brabham" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Bugatti" (T.v_pi { done = +0, total = +5 })
        , T.kv_ "Buick" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Cadillac" (T.v_pi { done = +2, total = +7 })
        , T.kv_ "Can-Am" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Casie Curie Motorsport" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Caterham" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Chevrolet" (T.v_pi { done = +6, total = +36 })
        , T.kv_ "Citroën" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Cupra" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Czinger" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Datsun" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "DeBerti" (T.v_pi { done = +1, total = +8 })
        , T.kv_ "DeLorean DMC" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Dodge" (T.v_pi { done = +5, total = +18 })
        , T.kv_ "Donkervoort" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "DS Automobiles" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Eagle" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Elemental" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Exomotive" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Extreme E" (T.v_pi { done = +0, total = +10 })
        , T.kv_ "Fast and Furious" (T.v_pi { done = +0, total = +5 })
        , T.kv_ "Ferrari" (T.v_pi { done = +2, total = +43 })
        , T.kv_ "Fiat" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Ford" (T.v_pi { done = +12, total = +78 })
        , T.kv_ "Formula Drift" (T.v_pi { done = +2, total = +16 })
        , T.kv_ "Forsberg Racing" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Funco Motorsports" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Ginetta" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "GMC" (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Gordon Murray Automotive" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "HDT" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Hennessey" (T.v_pi { done = +0, total = +5 })
        , T.kv_ "Holden" (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Honda" (T.v_pi { done = +2, total = +16 })
        , T.kv_ "Hoonigan" (T.v_pi { done = +5, total = +14 })
        , T.kv_ "Hot Wheels" (T.v_pi { done = +7, total = +11 })
        , T.kv_ "HSV" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Hummer" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Hyundai" (T.v_pi { done = +0, total = +7 })
        , T.kv_ "Infiniti" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "International" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Italdesign" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Jaguar" (T.v_pi { done = +3, total = +20 })
        , T.kv_ "Jeep" (T.v_pi { done = +2, total = +6 })
        , T.kv_ "Jimco" (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Kia" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Koenigsegg" (T.v_pi { done = +0, total = +8 })
        , T.kv_ "KTM" (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Lamborghini" (T.v_pi { done = +4, total = +32 })
        , T.kv_ "Lancia" (T.v_pi { done = +0, total = +5 })
        , T.kv_ "Land Rover" (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Lexus" (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Lincoln" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Local Motors" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Lola" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Lotus" (T.v_pi { done = +1, total = +12 })
        , T.kv_ "Lucid" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Lync & Co" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Maserati" (T.v_pi { done = +1, total = +5 })
        , T.kv_ "Mazda" (T.v_pi { done = +2, total = +10 })
        , T.kv_ "McLaren" (T.v_pi { done = +0, total = +17 })
        , T.kv_ "Mersedes-AMG" (T.v_pi { done = +0, total = +9 })
        , T.kv_ "Mersedes-Benz" (T.v_pi { done = +2, total = +24 })
        , T.kv_ "Mercury" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Meyers" (T.v_pi { done = +2, total = +3 })
        , T.kv_ "MG" (T.v_pi { done = +0, total = +7 })
        , T.kv_ "Mini" (T.v_pi { done = +1, total = +8 })
        , T.kv_ "Mitsubishi Motors" (T.v_pi { done = +2, total = +13 })
        , T.kv_ "Morgan" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Morris" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Moster" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Napier" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "NIO" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Nissan" (T.v_pi { done = +7, total = +37 })
        , T.kv_ "Noble" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Oldsmobile" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Opel" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Pagani" (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Peel" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Penhall" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Peugeot" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Plymouth" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Polaris" (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Pontiac" (T.v_pi { done = +1, total = +7 })
        , T.kv_ "Porsche" (T.v_pi { done = +6, total = +52 })
        , T.kv_ "Radical" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "RAESR" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Ram" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Reliant" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Renault" (T.v_pi { done = +2, total = +10 })
        , T.kv_ "Rimac" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Rivian" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "RJ Anderson" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Rossion" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Saleen" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "Schuppan" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Shelby" (T.v_pi { done = +0, total = +3 })
        , T.kv_ "SIERRA Cars" (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Spania GTA" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Subaru" (T.v_pi { done = +1, total = +14 })
        , T.kv_ "Toyota" (T.v_pi { done = +3, total = +27 })
        , T.kv_ "TVR" (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Ultima" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Universal Studios" (T.v_pi { done = +0, total = +5 })
        , T.kv_ "Vauxhall" (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Volkswagen" (T.v_pi { done = +2, total = +23 })
        , T.kv_ "Volvo" (T.v_pi { done = +0, total = +4 })
        , T.kv_ "VUHL" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "W Motors" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Willys" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Wuling" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Xpeng" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Zenyo" (T.v_pi { done = +0, total = +2 })
        ]

    )

{-

--\s+(\d+)/(\d+)/(\d+)
// (T.inj/date { mon = +$1, day = +$2, year = +$3 })

-}