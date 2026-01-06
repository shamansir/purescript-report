let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "lego-city-undercover"
        , name = "Lego City: Undercover"
        , platform = GT.Platform.Switch
        , playtime = GT.Playtime.MoreThan { hrs = +35, min = +0, sec = +0 }
        }
        { day = +16, mon = +9, year = +2025 } (

    T.groupStats "Stats" [ "00-stats" ]

        [ T.kv_ "Yellow Dots" (T.v_i +1864960)
        , T.kv_ "White Squares" (T.v_i +31489)
        , T.kv_ "Completion" (T.v_pct 40.7)

        , T.kv_ "Red Bricks" (T.v_pi { done = +14, total = +39 })
        , T.kv_ "People" (T.v_pi { done = +107, total = +305 })
        , T.kv_ "Major Super Bricks" (T.v_pi { done = +7, total = +15 }) -- Chains
        , T.kv_ "Vehicles" (T.v_pi { done = +39, total = +120 })
        , T.kv_ "Gold Bricks" (T.v_pi { done = +133, total = +450 })
        ]

    # T.group "Lego City (Total)" [ "01-city", "00-lego-city" ]
        [ T.kv_ "People"                   (T.v_pi { done = +107, total = +305 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +39, total = +120 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +14, total = +39 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +133, total = +450 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +1, total = +16 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +2, total = +18 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +8, total = +22 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +4, total = +17 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +8, total = +20 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +3, total = +17 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +1, total = +20 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +12 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +2, total = +13 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +10, total = +17 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +3, total = +20 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +5, total = +22 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +3, total = +17 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +1, total = +17 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +35, total = +65 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +4, total = +14 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +19 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +1, total = +16 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +16, total = +17 })
        ]


    # T.group "1. Albatross Island" [ "01-city", "01-albatross-island" ]

        [ T.kv_ "People"                   (T.v_pi { done = +4, total = +9 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +1, total = +15 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "2. Apollo Island" [ "01-city", "02-apollo-island" ]

        [ T.kv_ "People"                   (T.v_pi { done = +4, total = +11 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +5, total = +23 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +2 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +3, total = +7 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "3. Auburn" [ "01-city", "03-auburn" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +21 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +1, total = +7 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +5, total = +26 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +2, total = +6 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "4. Auburn Bay Bridge" [ "01-city", "04-auburn-bay-bridge" ]

        [ T.kv_ "People"                   (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +1, total = +4 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +0, total = +0 })
        ]

    # T.group "5. Blackwell Bridge" [ "01-city", "05-blackwell-bridge" ]

        [ T.kv_ "People"                   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +1, total = +2 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +0, total = +0 })
        ]

    # T.group "6. Bluebell National Park" [ "01-city", "06-bluebell-park" ]

        [ T.kv_ "People"                   (T.v_pi { done = +4, total = +17 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +7, total = +25 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +3, total = +5 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "7. Bright Lights Plaze" [ "01-city", "07-bright-lights" ]

        [ T.kv_ "People"                   (T.v_pi { done = +4, total = +14 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +2, total = +4 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +6, total = +20 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "8. Cherry Tree Hills" [ "01-city", "08-cherry-tree-hills" ]

        [ T.kv_ "People"                   (T.v_pi { done = +15, total = +23 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +5, total = +7 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +19, total = +25 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +1, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +1, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +6, total = +6 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]


    # T.group "9. Crescent Park" [ "01-city", "09-crescent-park" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +14 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +1, total = +16 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "10. Crosstown Tunnel" [ "01-city", "10-crosstown-tunnel" ]

        [ T.kv_ "People"                   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +0, total = +0 })
        ]

    # T.group "11. Downtown" [ "01-city", "11-downtown" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +11 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +3, total = +5 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +8, total = +22 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +3, total = +3 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "12. Festival Square" [ "01-city", "12-festival-square" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +12 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +2, total = +5 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +4, total = +20 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +2, total = +3 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "13. Fort Meadows" [ "01-city", "13-fort-meadows" ]

        [ T.kv_ "People"                   (T.v_pi { done = +8, total = +14 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +2, total = +4 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +11, total = +22 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +2 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +3, total = +5 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "14. Fresco" [ "01-city", "14-fresco" ]

        [ T.kv_ "People"                   (T.v_pi { done = +6, total = +14 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +2, total = +7 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +10, total = +5 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +2, total = +3 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "15. Grand Canal" [ "01-city", "15-grand-canal" ]

        [ T.kv_ "People"                   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +0, total = +2 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +0, total = +0 })
        ]

    # T.group "16. Heritage Bridge" [ "01-city", "16-heritage-bridge" ]

        [ T.kv_ "People"                   (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +1, total = +5 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +0 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +0, total = +0 })
        ]

    # T.group "17. Kings Court" [ "01-city", "17-kings-court" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +10 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +1, total = +7 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +4, total = +22 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "18. Lady Liberty Island" [ "01-city", "18-lady-liberty-island" ]

        [ T.kv_ "People"                   (T.v_pi { done = +0, total = +6 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +3 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +0, total = +15 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +0, total = +1 })
        ]

    # T.group "19. LEGO City Airport" [ "01-city", "19-lego-city-airport" ]

        [ T.kv_ "People"                   (T.v_pi { done = +2, total = +11 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +0, total = +8 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +4, total = +26 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +1, total = +2 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +1, total = +6 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "20. Pagoda" [ "01-city", "20-pagoda" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +16 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +1, total = +3 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +4, total = +23 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +2 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +2, total = +5 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "21. Paradise Sands" [ "01-city", "21-paradise-sands" ]

        [ T.kv_ "People"                   (T.v_pi { done = +6, total = +17 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +3, total = +7 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +10, total = +26 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +3, total = +7 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    # T.group "22. Uptown" [ "01-city", "22-uptown" ]

        [ T.kv_ "People"                   (T.v_pi { done = +3, total = +11 })
        , T.kv_ "Vehicles"                 (T.v_pi { done = +2, total = +4 })
        , T.kv_ "Red Bricks"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Gold Bricks"              (T.v_pi { done = +6, total = +17 })

        , T.kv_ "Arrest Gangs"             (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Smash ATMs"               (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destory Precious Bolders" (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Catch Aliens"             (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Water Flower Boxes"       (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Rescue Cats"              (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Take Coffee Breaks"       (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Arrest Vehicle Robbers"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Steal Vehicles"           (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Destroy Silver Statues"   (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Conquer Districts"        (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Return Pigs"              (T.v_pi { done = +1, total = +1 })
        , T.kv_ "Extinquish BBQ Fires"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Complete Drill Thrills"   (T.v_pi { done = +0, total = +1 })

        , T.kv_ "Construct Super Builds"   (T.v_pi { done = +2, total = +2 })
        , T.kv_ "Activate Ticket Machines" (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Free Runs"       (T.v_pi { done = +0, total = +1 })
        , T.kv_ "Complete Time Trials"     (T.v_pi { done = +0, total = +0 })
        , T.kv_ "Activate Disquise Booths" (T.v_pi { done = +1, total = +1 })
        ]

    )