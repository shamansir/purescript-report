let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "kirby-the-forgotten-land"
        , name = "Kirby: The Forgotten Land"
        , platform = GT.Platform.Switch
        , playtime = GT.Playtime.MoreThan { hrs = +10, min = +0, sec = +0 }
        }
        { day = +18, mon = +9, year = +2025 } (

    T.groupStats "Basic" [ "00-basic" ]
        [ T.kv_ "Stars" (T.v_i +7221)
        , T.kv_ "Super Stars" (T.v_i +2)
        , T.kv_ "Total Waddles" (T.v_i +128)
        , T.kv_ "Wondaria Remains" (T.v_pi { done = +41, total = +48 }) -- Waddle Dees
        , T.kv_ "Natural Plains" (T.v_pi { done = +42, total = +46 }) -- Waddle Dees
        , T.kv_ "Everbay Coast" (T.v_pi { done = +45, total = +47 }) -- Waddle Dees
        , T.kv_ "Wondaria Remains: Treasure Road" (T.v_pi { done = +1, total = +9 })
        , T.kv_ "Natural Plain: Treasure Road" (T.v_pi { done = +7, total = +7 })
        , T.kv_ "Everbay Coast: Treasure Road" (T.v_pi { done = +8, total = +8 })
        , T.kv_ "Figure Collection: Vol.1" (T.v_pi { done = +74, total = +79 }) -- Figres
        ]

    # T.group "World 1: Wondaria Remains" [ "01-world", "00-world-1", "00-wondaria-remains" ]

        [ T.kv_ "Welcome to Wondaria" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "Circuit Speedway" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "Invasion at the House of Horrors" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "The Wondaria Dream Parade" (T.v_pi { done = +11, total = +11 }) -- Waddle Dees
        , T.kv_ "Danger under the Big Top" (T.v_pi { done = +0, total = +7 }) -- Waddle Dees
        ]

    # T.group "World 1: Natural Plains" [ "01-world", "00-world-1", "01-natural-plains" ]

        [ T.kv_ "Downtown Grassland" (T.v_pi { done = +9, total = +9 }) -- Waddle Dees
        , T.kv_ "Through the Tunnel" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "Rocky Rollin' Road" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "A Trip to Alivel Mall" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "The Brawl at the Mall" (T.v_pi { done = +3, total = +7 }) -- Waddle Dees
        ]

    # T.group "World 1: Everbay Coast" [ "01-world", "00-world-1", "03-everbay-coast" ]

        [ T.kv_ "Abandoned Beach" (T.v_pi { done = +9, total = +9 }) -- Waddle Dees
        , T.kv_ "Concrete Isles" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "Scale the Cement Summit" (T.v_pi { done = +10, total = +10 }) -- Waddle Dees
        , T.kv_ "Fast-Flowing Waterworks" (T.v_pi { done = +11, total = +11 }) -- Waddle Dees
        , T.kv_ "The Tropical Terror" (T.v_pi { done = +5, total = +7 }) -- Waddle Dees
        ]

    # T.group "World 1: Wondaria Remains: Treasure Road" [ "01-world", "00-world-1", "00-wondaria-remains", "00-treasure-road" ]

        [ T.kv_ "Ring Mouth" (T.v_none) -- Difficulty: **; Best Time: --:--.--; Target Time: 01:00.00
        , T.kv_ "Pipe Mouth" (T.v_none) -- Difficulty: ***; Best Time: --:--.--; Target Time: 00:40.00
        , T.kv_ "Needle" (T.v_none) -- Difficulty: *; Best Time: --:--.--; Target Time: 00:30.00
        , T.kv_ "Cutter" (T.v_done) -- Difficulty: *; Best Time: 01:07.83; Target Time: 00:45.00
        , T.kv_ "Bomb" (T.v_none) -- Difficulty: **; Best Time: --:--.--; Target Time: 00:45.00
        , T.kv_ "Fleur Tornado" (T.v_none) -- Difficulty: ***; Best Time: --:--.--; Target Time: 01:00.00
        , T.kv_ "Hammer" (T.v_none) -- Difficulty: **; Best Time: --:--.--; Target Time: 01:20.00
        , T.kv_ "Fire" (T.v_none) -- Difficulty: **; Best Time: --:--.--; Target Time: 00:45.00
        , T.kv_ "Sword" (T.v_none) -- Difficulty: **; Best Time: --:--.--; Target Time: 01:00.00
        ]

    # T.group "World 1: Natural Plains: Treasure Road" [ "01-world", "00-world-1", "01-natural-plains", "00-treasure-road" ]

        [ T.kv_ "Vending Mouth" (T.v_done) -- Difficulty: **; Best Time: 01:42.55; Target Time: 01:20.00
        , T.kv_ "Cone Mouth" (T.v_done) -- Difficulty: ***; Best Time: 00:51.37; Target Time: 00:45.00
        , T.kv_ "Cutter" (T.v_done) -- Difficulty: *; Best Time: 01:24.52; Target Time: 01:00.00
        , T.kv_ "Bomb" (T.v_done) -- Difficulty: *; Best Time: 01:16.10; Target Time: 00:30.00
        , T.kv_ "Fire" (T.v_done) -- Difficulty: *; Best Time: 00:33.31; Target Time: 00:45.00 BEATEN
        , T.kv_ "Ice" (T.v_done) -- Difficulty: *; Best Time: 01:06.30; Target Time: 00:40.00
        , T.kv_ "Ranger" (T.v_none) -- Difficulty: **; Best Time: 00:56.22; Target Time: 01:00.00 BEATEN
        ]

    # T.group "World 1: Everbay Coast : Treasure Road" [ "01-world", "00-world-1", "03-everbay-coast", "00-treasure-road" ]

        [ T.kv_ "Normal" (T.v_done) -- Difficulty: **; Best Time: 00:35.32; Target Time: 00:30.00
        , T.kv_ "Stairs Mouth" (T.v_done) -- Difficulty: ***; Best Time: 02:13.63; Target Time: 01:45.00
        , T.kv_ "Hammer" (T.v_done) -- Difficulty: *; Best Time: 01:12.86; Target Time: 00:45.00
        , T.kv_ "Drill" (T.v_done) -- Difficulty: *; Best Time: 02:01.17; Target Time: 01:10.00
        , T.kv_ "Tornado" (T.v_done) -- Difficulty: *; Best Time: 00:53.18; Target Time: 00:30.00
        , T.kv_ "Volcano Fire" (T.v_done) -- Difficulty: **; Best Time: 01:37.84; Target Time: 01:00.00
        , T.kv_ "Sword" (T.v_done) -- Difficulty: *; Best Time: 01:23.14; Target Time: 01:00.00
        , T.kv_ "Chakram Cutter" (T.v_done) -- Difficulty: **; Best Time: 01:34.15; Target Time: 01:00.00

        ]

    -- Yellow Vol. 1 Figures are most common, being found right from the beginning of the game, while the machine doesn't appear in Waddle Dee Town until you complete Natural Plains.
    -- Green Vol. 2 Figures don't appear until Everbay Coast, while the machine doesn't appear until you beat its boss stage, The Tropical Terror.
    -- Red Vol. 3 Figures don't appear until Originull Wasteland stages, and the machine doesn't appear until you beat its boss stage, Collector in the Sleepless Valley.
    -- Blue Vol. 4 Figures only appear in Isolated Isles, with the machine appearing only after you roll credits for the first time after Lab Discovera. Since they're the only figures found in Isolated Isles, they're technically more common than the red Vol. 3 Figures!

    # T.group "Figures" [ "02-figures" ]

        [ T.kv_ "Kirby" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Kirby was pulled into a mysterious vortex that appeared in the sky over his home on Planet Popstar. When he woke up, he was in a new world! Traveling through that vortex also gave Kirby a new and mysterious power... Let's see what it can do! Onward to adventure! This is fittingly always the first figure Kirby collects."
        , T.kv_ "Elfilin" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "A mystical new friend you met in a mysterious new world. You found Elfilin as he was trying to save the Waddle Dees from the Beast Pack. He seems happy to be Kirby's guide, sharing helpful advice to save the Waddle Dees and rebuild their town. Thanks, Elfilin!"
        , T.kv_ "Bandana Waddle Dee" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Bandana Waddle Dee was pulled through the vortex along with Kirby. When he heard that his fellow Waddle Dees were being captured by the Beast Pack, he grabbed his trusty spear and ran to help! He can join you as P2 while you explore the new world."
        , T.kv_ "Waddle Dee" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "These Dee-lightful residents of Planet Popstar landed in the new world before Kirby arrived. Things looked grim when wild beasts began to capture them and steal their precious food...but with Kirby's help, they're rebuilding their town and starting over!"
        , T.kv_ "Sword" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Slice through this new world as a sword-wielding, green-hatted hero! Try charging up before you swing for extra power. Train hard, and become this world's new sword master!"
        , T.kv_ "Bomb" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Hold down the button to aim and throw. Got it? Hold, aim, throw! Throw, hit, BOOM! You can run and throw them too, or roll them ahead of you to get an explosive strike! Contrary to the English description, it is not possible to run while throwing bombs in this game. This error does not exist in Japanese."
        , T.kv_ "Cutter" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Swish! Sling that sharp-edged boomerang! Use it to grab far-off items. Hold the button down to freeze it in midair and charge it up. That will make it extra powerful! Swish!"
        , T.kv_ "Fire" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "This ability is hot, hot, hot! Light fuses, burn through obstacles, and toast your enemies with the power of a raging fire. Run, jump, then attack to blast forward as a fireball!"
        , T.kv_ "Ranger" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Time for some shooting-star sharpshooting! Pew, pew! Hold the button down to charge and aim, then let go to send a sparkling shot flying. Charge it up for bigger blasts!"
        , T.kv_ "Needle" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Ouch! Ooch! Careful with those spikes! Jab enemies in place, or pick 'em up by rolling around. Nab a whole bunch of 'em together, then launch them off all at once! POW!"
        , T.kv_ "Ice" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Brrr! So cold! Can you see your own breath? Make a chunk of ice, then kick it forward! Skate and slide wherever you go. You'll even glide right over mud and magma!"
        , T.kv_ "Car" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Car-Mouth Kirby" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Kirby gained the mysterious Mouthful Mode ability after he flew through that vortex... Now he can stuff an entire car into his mouth! When he does, he turns into a peppy, pink car that can jump and use Turbo Dash. It's a nice day for a scenic drive. Turn up the radio! Across all Mouthful Mode English figure descriptions (except for Spring-Mouth Kirby, Gear-Mouth Kirby, and Sign-Mouth Kirby), the first sentence labels the Mouthful Mode ability as mysterious when the Japanese version labels the vortex as such instead."
        , T.kv_ "Traffic Cone" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Cone-Mouth Kirby" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Kirby gained the mysterious Mouthful Mode ability after he flew through that vortex... Now he can stuff an entire cone into his mouth! Use your pointy head to jab below you and bust open cracks in the ground or on pipes. You're out of cone-trol, Kirby!"
        , T.kv_ "Water Tank" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Dome-Mouth Kirby" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Kirby gained the mysterious Mouthful Mode ability after he flew through that vortex... Now he can stuff an entire dome into his mouth! Wrap around one of these, then twist until it pops open. Think there's anything good inside? Only one way to find out!"
        , T.kv_ "Storage Cabinet" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Storage-Mouth Kirby" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Kirby gained the mysterious Mouthful Mode ability after he flew through that vortex... Now he can stuff an entire set of lockers into his mouth! Wiggle and thrash until you tip over... Hey! Who put this behind the lockers?"
        , T.kv_ "Rental Lockers" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Bolted-Storage-Mouth Kirby" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Kirby gained the mysterious Mouthful Mode ability after he flew through that vortex... Now he can stuff an entire set of lockers into his mouth! This one won't budge. Wiggle, wiggle, wiggle, and— Whoa! He just took a whole chunk of that wall down! Use this to find hidden routes you didn't know about. Despite what the end of the English description implies, the only time Bolted-Storage-Mouth is used is required to complete the stage it appears in. Again, this error is not present in Japanese."
        , T.kv_ "Captured Waddle Dee" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "One of the Waddle Dees being held captive by the fearsome Beast Pack. There are so many out there waiting to be saved. You can almost hear them calling for help... \"Wa-wa! Wa-wa!\" Let me out, let me out! Wait, you hear that too? One of them must be nearby!"
        , T.kv_ "Captured Waddle Dees" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "The Beast Pack managed to catch these three Waddle Dees with one golden cage! If they work together, they might be able to... Oh. Never mind. They're too sad to move. All they can do is cry out for help. \"Wa-wa! Wa-wa!\" Somebody save them!"
        , T.kv_ "Awoofy" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "A common beast that can be found all over the new world. They're pretty cute, but they have a dangerous wild side. They'll growl and jump at anyone who crosses them! A whole bunch of these critters attacked the Waddle Dees. Funny, you'd think they'd get along! The end of the description alludes to Awoofies taking the role of \"most basic enemy type\" that Waddle Dees held in previous games."
        , T.kv_ "Cappy" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Bronto Burt" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Kabu" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Bouncy" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Gabon" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Shotzo" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Gordo" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Gordo Bar" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Big Kabu" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Buffahorn" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Behold! It's the brutal, brutish Buffahorn! Weak attacks won't stand a chance against this critter's forward tackle. Good thing they have a hard time stopping! If you're careful, you can trick them into running off cliffs. They'll be OK down there. They're real tough."
        , T.kv_ "Tortorner" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Tortorner has a whole shell made of concrete, as if it walked off with somebody's sidewalk! It has a thick skull and a mean bite, but its body is pretty delicate. A Mouthful Mode ability might help you crack through its shell and land a brutal blow from above!"
        , T.kv_ "Tortuilding" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Whoa! Tortuilding's shell is a whole building! This big baddie must be some kind of boss for the smaller beasts. It enjoys basking in the sun, so it climbs to high spots and claims the whole area as its territory. (It must take a long time for this critter to get up there...)"
        , T.kv_ "Blade Knight" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Poppy Bros. Jr." (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Sir Kibble" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Hot Head" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Bernard" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "This uppity pup is an expert marksman—pew, pew—who's quick on his feet as he patrols the new world. His eyes are hidden under his hat, but that doesn't seem to affect his aim! He also has an impressive sniffer that can track prey, near or far."
        , T.kv_ "Needlous" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Chilly" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Jabhog" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "Charge those points up, then...zing! Let 'em fly! Jabhog is famous in the new world for its spiky spines. They were short and cute when it was young, but they eventually grew into dangerous needles. It'll jab anything that gets too close, so approach with caution!"
        , T.kv_ "Wanted Posters" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A A wanted poster of Elfilin can be found hidden on the back of this figure if spun around while observing it."
        , T.kv_ "Arrow Sign" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Pop Flower" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Tulip" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Star Block" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Bomb Block" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Switch" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Target Switch" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Lantern Switch" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Cannon" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Warp Star" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Treasure Chest" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Radio" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Car-Shop Sign" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "This logo belonged to an auto shop named Holine Custom Autos—part of Holine Corp. Holine also sold industrial parts, managed construction, and made all kinds of stuff. From buildings to streets to amusement-park rides, remember: if it's quality, it's Holine! Err, perhaps \"it was\" would be more accurate. If decoded from the alien language, the sign actually reads \"Holine Auto Custom\"."
        , T.kv_ "Alivel Mall Sign" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "This was the logo for Alivel Mall, owned by Alivel Holding Company. \"Making life even livelier\" was their corporate slogan. They ran a wide range of businesses: entertainment arenas, food shops, service industries... Now the remains of those businesses can be found in all kinds of places, empty and alone."
        , T.kv_ "Lightron Works Sign" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "The Lightron Works Company was a massive corporation that invested in research and development within all kinds of fields: electro, bio, astro, and more! Lightron eventually split up, giving rise to many rival companies and countless heated corporate battles."
        , T.kv_ "Cherry" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Watermelon" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Tangerine" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Melon" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Star Coin" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Green Star Coin" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Red Star Coin" (T.v_done) {- ★ -} {- NOT RARE -} // T.inj/det "N/A Can only be obtained from the Gotcha Machine Alley."
        , T.kv_ "Blue Star Coin" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Invincible Candy" (T.v_done) {- ★★ -} {- NOT RARE -} // T.inj/det "N/A"
        , T.kv_ "Maxim Tomato" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "If your health is low, just eat a Maxim Tomato to fully heal yourself. (They're packed with nutrients!) Did these fall through the same vortex as Kirby and his friends, or were they in the new world already? It's hard to tell, but everyone seems to enjoy them—even the Beast Pack!"
        , T.kv_ "Café-Staff Kirby" (T.v_none) {- ★★★★ -} {- RARE -} // T.inj/det "Kirby's taken on a side gig at the counter of the Waddle Dee Café! He's dressed like a focused employee, but he's secretly fighting the urge to gobble up each dish himself. This might be his greatest struggle yet... Stay strong, Kirby! The \"Frenzy Gig\" in the Waddle Dee Café: Help Wanted! Sub-Game must be completed in order to earn this figure."
        , T.kv_ "Fishing-Pond Kirby" (T.v_none) {- ★★★★ -} {- RARE -} // T.inj/det "Ahhh. Kirby's doing a bit of fishin' at the ol' fishin' pond. He looks super, super relaxed... Maybe too relaxed. Hey! Pay attention, Kirby! There's exciting stuff swimming in that pond, including the legendary \"Bling Blipper\" of Waddle Dee Town! The Bling Blipper must be caught in the Flash Fishing Sub-Game in order to earn this figure."
        , T.kv_ "Wise Waddle Dee" (T.v_done) {- ★★★★ -} {- RARE -} // T.inj/det "When you need wisdom, visit Wise Waddle Dee! He always has a tip handy and seems to know a lot about this new world. His magical encyclopedia can collect and share rankings from all over the world! (Where did he even find that book?!) Earn this figure by talking to Wise Waddle Dee, or asking him to continue talking, 5 total times."
        , T.kv_ "Delivery Waddle Dee" (T.v_none) {- ★★★★ -} {- RARE -} // T.inj/det "This dutiful delivery Dee works for Waddle Dee-liveries in town. He gets helpful items to your doorstep with blinding speed! Kirby's handwriting makes it hard to read the Present Codes sometimes, but this kind soul approves the orders anyway. Three offline/town Present Codes must be entered at Waddle Dee-liveries in order to earn this figure."
        , T.kv_ "Café-Staff Waddle Dee" (T.v_done) {- ★★★★ -} {- RARE -} // T.inj/det "This short-order cook loves to feed his fellow Waddle Dees at the café in town. He even kept them fed after they crashed in the new world! His cooking smelled so good...which is how the Beast Pack found them. Now that the café has been rebuilt, it's time to get to work! 300 Star Coins must be spent at Waddle Dee Café in order to earn this figure."
        , T.kv_ "Game-Shop Waddle Dee" (T.v_none) {- ★★★★ -} {- RARE -} // T.inj/det "Step right up, step right up! This Waddle Dee runs the town's favorite game, Tilt-and-Roll Kirby. The other Waddle Dees love to play his game. In fact, they wouldn't stop asking him for more! That might explain why he added the daunting Extra Hard difficulty. All three levels in the Tilt-and-Roll Kirby Sub-Game must be completed on standard difficulty in order to earn this figure."
        , T.kv_ "Item-Shop Waddle Dee" (T.v_none) {- ★★★★ -} {- RARE -} // T.inj/det "This enterprising Waddle Dee opened his own item shop in town. He sells special items that will help you out. He also supplies the café with an Energy Drink that he makes by hand. They have a special business arrangement: two Energy Drinks for three Kirby Burgers! 1050 Star Coins must be spent at Waddle Dee's Item Shop in order to earn this figure."
        , T.kv_ "Wild Frosty" (T.v_done) {- ★★★ -} {- NOT RARE -} // T.inj/det "The mysterious vortex brought Mr. Frosty to the new world too! He arrived shortly before Kirby and immediately joined the Beast Pack. (The change in wardrobe must have made it an easy choice.) It's tough to be the new guy, but his fellow beasts love working with him."
        , T.kv_ "Strong-Armed BeastGorimondo" (T.v_done) {- ★★★★ -} {- NOT RARE -} // T.inj/det "Gorimondo considers the local shopping mall to be his personal territory. As part of the Beast Pack's executive council, he's in charge of capturing Waddle Dees and gathering food. He tends to eat all the fruit himself... He just can't help it! This behavior has earned him an earful from his boss more than once."
        ]

    )

