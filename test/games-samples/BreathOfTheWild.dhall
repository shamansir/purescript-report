let T = ./Types.dhall
let GT = ./Game.Types.dhall

in
    GT.collapseAt
        { id = "zelda-breath-of-the-wild"
        , name = "Zelda: Breath of the Wild"
        , platform = GT.Platform.Switch
        , playtime = GT.Playtime.MoreThan { hrs = +205, min = +0, sec = +0 }
        }
        { day = +29, mon = +8, year = +2025 }

    ( T.group "General Stats" [ "00-stats" ]
        [ T.kv_ "Health Hearts" (T.v_i +18)
        , T.kv_ "Stamina Circles" (T.v_i +3)
        , T.kv_ "Divine Orbs" (T.v_i +3)
        , T.kv_ "Shrines Discovered" (T.v_i +87)
        , T.kv_ "Korok Seeds" (T.v_i +225)
        , T.kv_ "Rupees" (T.v_i +8116)
        , T.kv_ "Divine Beasts" (T.v_pi { done = +4, total = +4 })
        ]
    -- Creatures : 001, 012, 018, 028, 032, 043, 059, 063 (000-083)
    -- Monsters : 085, 096, 104, 114, 121, 122, 125, 138, 147 (084-164)
    -- Food : 166, 167, 176, 180, 186, 198 (165-200)
    -- Equipment : 269, 314, 378  (201-385)
    -- Treasure : 386, 387, 388, 389 (386-389)
    -- Memories : - : #1, #2, #3, #4
    -- Memories Ex : - : #1, #2, #3, #4
    # T.group "Recovered Memories" [ "01-memories" ]
        [ T.kv_ "#1: Subdued Ceremony" (T.v_done)
        , T.kv_ "#2: Revali's Flap" (T.v_done)
        , T.kv_ "#3: ???" (T.v_none)
        , T.kv_ "#4: Daruk's Meetle" (T.v_done)
        , T.kv_ "#5: Zelda's Resentement" (T.v_done)
        , T.kv_ "#6: Urbosa's Hand" (T.v_done)
        , T.kv_ "#7: ???" (T.v_none)
        , T.kv_ "#8: ???" (T.v_none)
        , T.kv_ "#9: ???" (T.v_none)
        , T.kv_ "#10: Mipha's Touch" (T.v_done)
        , T.kv_ "#11: Shelter from the Storm" (T.v_done)
        , T.kv_ "#12: ???" (T.v_none)
        , T.kv_ "#13: Slumbering Power" (T.v_done)
        , T.kv_ "#14: ???" (T.v_none)
        , T.kv_ "#15: Return of Galamity Ganon" (T.v_done)
        , T.kv_ "#16: Despair" (T.v_done)
        , T.kv_ "#17: ???" (T.v_none)
        , T.kv_ "#18: The Master Sword" (T.v_done)
        ]
    # T.group "Shrines" [ "02-shrines" ]

        [ T.kv_ "Oman Au :: Great Plateau" (T.p_done) // T.inj/det "BOTW-001"
        , T.kv_ "Ja Baij :: Great Plateau" (T.p_done) // T.inj/det "BOTW-002"
        , T.kv_ "Owa Daim :: Great Plateau" (T.p_done) // T.inj/det "BOTW-003"
        , T.kv_ "Keh Namut :: Great Plateau" (T.p_done) // T.inj/det "BOTW-004"
        , T.kv_ "Bosh Kala :: Dueling Peaks" (T.p_done) // T.inj/det "BOTW-005"
        , T.kv_ "Toto Sah :: Dueling Peaks" (T.p_todo) // T.inj/det "BOTW-006"
        , T.kv_ "Shee Vaneer :: Dueling Peaks" (T.p_done) // T.inj/det "BOTW-007"
        , T.kv_ "Ree Dahee :: Dueling Peaks" (T.p_done) // T.inj/det "BOTW-008"
        , T.kv_ "Shee Venath :: Dueling Peaks" (T.p_todo) // T.inj/det "BOTW-009"
        , T.kv_ "Ha Dahamar :: Dueling Peaks" (T.p_done) // T.inj/det "BOTW-010"
        , T.kv_ "Ta'loh Naeg :: Dueling Peaks" (T.p_done) // T.inj/det "BOTW-011"
        , T.kv_ "Hila Rao :: Dueling Peaks" (T.p_done) // T.inj/det "BOTW-012 :: Watch Out for the Flowers"
        , T.kv_ "Lakna Rokee :: Dueling Peaks" (T.p_todo) // T.inj/det "BOTW-013 :: The Stolen Heirloom"
        , T.kv_ "Chaas Qeta :: Hateno" (T.p_doing) // T.inj/det "BOTW-014"
        , T.kv_ "Myahm Agana :: Hateno" (T.p_done) // T.inj/det "BOTW-015"
        , T.kv_ "Tahno O'ah :: Hateno" (T.p_done) // T.inj/det "BOTW-016 :: Secret of the Cedars"
        , T.kv_ "Jitan Sa'mi :: Hateno" (T.p_done) // T.inj/det "BOTW-017 :: The Spring of Wisdom"
        , T.kv_ "Dow Na'eh :: Hateno" (T.p_todo) // T.inj/det "BOTW-018"
        , T.kv_ "Kam Urog :: Hateno" (T.p_done) // T.inj/det "BOTW-019 :: The Cursed Statue"
        , T.kv_ "Mezza Lo :: Hateno" (T.p_todo) // T.inj/det "BOTW-020 :: The Crowned Beast"
        , T.kv_ "Daka Tuss :: Lanayru" (T.p_done) // T.inj/det "BOTW-021"
        , T.kv_ "Kaya Wan :: Lanayru" (T.p_done) // T.inj/det "BOTW-022"
        , T.kv_ "Soh Kofi :: Lanayru" (T.p_done) // T.inj/det "BOTW-023"
        , T.kv_ "Sheh Rata :: Lanayru" (T.p_done) // T.inj/det "BOTW-024"
        , T.kv_ "Rucco Maag :: Lanayru" (T.p_doing) // T.inj/det "BOTW-025"
        , T.kv_ "Shai Yota :: Lanayru" (T.p_done) // T.inj/det "BOTW-026 :: Master of the Wind"
        , T.kv_ "Dagah Keek :: Lanayru" (T.p_done) // T.inj/det "BOTW-027 :: The Ceremonial Song"
        , T.kv_ "Ne'ez Yohma :: Lanayru" (T.p_doing) // T.inj/det "BOTW-028"
        , T.kv_ "Kah Mael :: Lanayru" (T.p_done) // T.inj/det "BOTW-029"
        , T.kv_ "Rona Kachta :: Woodland" (T.p_done) // T.inj/det "BOTW-030"
        , T.kv_ "Monya Toma :: Woodland" (T.p_done) // T.inj/det "BOTW-031"
        , T.kv_ "Kuhn Sidajj :: Woodland" (T.p_done) // T.inj/det "BOTW-032 :: Trial of Second Sight"
        , T.kv_ "Daag Chokah :: Woodland" (T.p_done) // T.inj/det "BOTW-033 :: The Lost Pilgrimage"
        , T.kv_ "Keo Ruug :: Woodland" (T.p_done) // T.inj/det "BOTW-034"
        , T.kv_ "Maag Halan :: Woodland" (T.p_done) // T.inj/det "BOTW-035 :: The Test of Wood"
        , T.kv_ "Ketoh Wawai :: Woodland" (T.p_done) // T.inj/det "BOTW-036 :: Shrouded Shrine"
        , T.kv_ "Mirro Shaz :: Woodland" (T.p_doing) // T.inj/det "BOTW-037"
        , T.kv_ "Dah Kaso :: Central" (T.p_done) // T.inj/det "BOTW-038"
        , T.kv_ "Rota Ooh :: Central" (T.p_done) // T.inj/det "BOTW-039"
        , T.kv_ "Wahgo Katta :: Central" (T.p_done) // T.inj/det "BOTW-040"
        , T.kv_ "Kaam Ya'tak :: Central" (T.p_done) // T.inj/det "BOTW-041"
        , T.kv_ "Katah Chuki :: Central" (T.p_done) // T.inj/det "BOTW-042"
        , T.kv_ "Noya Neha :: Central" (T.p_done) // T.inj/det "BOTW-043"
        , T.kv_ "Saas Ko'sah :: Central" (T.p_todo) // T.inj/det "BOTW-044"
        , T.kv_ "Namika Ozz :: Central" (T.p_doing) // T.inj/det "BOTW-045"
        , T.kv_ "Ishto Soh :: Lake" (T.p_done) // T.inj/det "BOTW-046"
        , T.kv_ "Shoqa Tatone :: Lake" (T.p_todo) // T.inj/det "BOTW-047 :: Guardian Slideshow"
        , T.kv_ "Ka'o Makagh :: Lake" (T.p_done) // T.inj/det "BOTW-048"
        , T.kv_ "Pumaag Nitae :: Lake" (T.p_done) // T.inj/det "BOTW-049"
        , T.kv_ "Ya Naga :: Lake" (T.p_done) // T.inj/det "BOTW-050"
        , T.kv_ "Shae Katha :: Lake" (T.p_todo) // T.inj/det "BOTW-051 :: The Serpent's Jaws"
        , T.kv_ "Shai Utoh :: Faron" (T.p_done) // T.inj/det "BOTW-052"
        , T.kv_ "Qukah Nata :: Faron" (T.p_done) // T.inj/det "BOTW-053 :: A Song of Storms"
        , T.kv_ "Shoda Sah :: Faron" (T.p_todo) // T.inj/det "BOTW-054"
        , T.kv_ "Tawa Jinn :: Faron" (T.p_done) // T.inj/det "BOTW-055 :: The Three Giant Brothers"
        , T.kv_ "Yah Rin :: Faron" (T.p_done) // T.inj/det "BOTW-056"
        , T.kv_ "Kah Yah :: Faron" (T.p_done) // T.inj/det "BOTW-057 :: A Fragmented Monument"
        , T.kv_ "Muwo Jeem :: Faron" (T.p_done) // T.inj/det "BOTW-058"
        , T.kv_ "Korgu Chideh :: Faron" (T.p_done) // T.inj/det "BOTW-059 :: Stranded on Eventide"
        , T.kv_ "Mijah Rokee :: Ridgeland" (T.p_todo) // T.inj/det "BOTW-060 :: Under a Red Moon"
        , T.kv_ "Shae Loya :: Ridgeland" (T.p_done) // T.inj/det "BOTW-061"
        , T.kv_ "Sheem Dagoze :: Ridgeland" (T.p_done) // T.inj/det "BOTW-062 :: The Two Rings"
        , T.kv_ "Mogg Latan :: Ridgeland" (T.p_doing) // T.inj/det "BOTW-063"
        , T.kv_ "Zalta Wa :: Ridgeland" (T.p_todo) // T.inj/det "BOTW-064"
        , T.kv_ "Maag No'rah :: Ridgeland" (T.p_done) // T.inj/det "BOTW-065"
        , T.kv_ "Toh Yahsa :: Ridgeland" (T.p_done) // T.inj/det "BOTW-066 :: Trial of Thunder"
        , T.kv_ "Sha Warvo :: Tabantha" (T.p_doing) // T.inj/det "BOTW-067"
        , T.kv_ "Voo Lota :: Tabantha" (T.p_todo) // T.inj/det "BOTW-068 :: Recital at Warbler's Nest"
        , T.kv_ "Akh Va'quot :: Tabantha" (T.p_doing) // T.inj/det "BOTW-069"
        , T.kv_ "Bareeda Naag :: Tabantha" (T.p_done) // T.inj/det "BOTW-070 :: The Ancient Rito Song"
        , T.kv_ "Tena Ko'sah :: Tabantha" (T.p_done) // T.inj/det "BOTW-071"
        , T.kv_ "Kah Okeo :: Tabantha" (T.p_doing) // T.inj/det "BOTW-072"
        , T.kv_ "Hia Miu :: Hebra" (T.p_done) // T.inj/det "BOTW-073"
        , T.kv_ "To Quomo :: Hebra" (T.p_done) // T.inj/det "BOTW-074"
        , T.kv_ "Mozo Shenno :: Hebra" (T.p_done) // T.inj/det "BOTW-075 :: The Bird in the Mountains"
        , T.kv_ "Shada Naw :: Hebra" (T.p_done) // T.inj/det "BOTW-076"
        , T.kv_ "Rok Uwog :: Hebra" (T.p_done) // T.inj/det "BOTW-077"
        , T.kv_ "Sha Gehma :: Hebra" (T.p_doing) // T.inj/det "BOTW-078"
        , T.kv_ "Qaza Tokki :: Hebra" (T.p_done) // T.inj/det "BOTW-079 :: Trial on the Cliff"
        , T.kv_ "Goma Asaagh :: Hebra" (T.p_done) // T.inj/det "BOTW-080"
        , T.kv_ "Maka Rah :: Hebra" (T.p_doing) // T.inj/det "BOTW-081"
        , T.kv_ "Dunba Taag :: Hebra" (T.p_done) // T.inj/det "BOTW-082"
        , T.kv_ "Lanno Kooh :: Hebra" (T.p_done) // T.inj/det "BOTW-083"
        , T.kv_ "Gee Ha'rah :: Hebra" (T.p_done) // T.inj/det "BOTW-084"
        , T.kv_ "Rin Oyaa :: Hebra" (T.p_doing) // T.inj/det "BOTW-085"
        , T.kv_ "Hawa Koth :: Wasteland" (T.p_done) // T.inj/det "BOTW-086"
        , T.kv_ "Kema Zoos :: Wasteland" (T.p_done) // T.inj/det "BOTW-087 :: The Silent Swordswomen"
        , T.kv_ "Tho Kayu :: Wasteland" (T.p_todo) // T.inj/det "BOTW-088"
        , T.kv_ "Raqa Zunzo :: Wasteland" (T.p_todo) // T.inj/det "BOTW-089 :: The Undefeated Champ"
        , T.kv_ "Misae Suma :: Wasteland" (T.p_done) // T.inj/det "BOTW-090 :: The Perfect Drink"
        , T.kv_ "Dila Maag :: Wasteland" (T.p_done) // T.inj/det "BOTW-091 :: The Desert Labyrinth"
        , T.kv_ "Korsh O'hu :: Wasteland" (T.p_done) // T.inj/det "BOTW-092 :: The Seven Heroines"
        , T.kv_ "Kay Noh :: Wasteland" (T.p_done) // T.inj/det "BOTW-093"
        , T.kv_ "Dako Tah :: Wasteland" (T.p_done) // T.inj/det "BOTW-094 :: The Eye of the Sandstorm"
        , T.kv_ "Suma Sahma :: Wasteland" (T.p_done) // T.inj/det "BOTW-095 :: Secret of the Snowy Peaks"
        , T.kv_ "Jee Noh :: Wasteland" (T.p_done) // T.inj/det "BOTW-096"
        , T.kv_ "Daqo Chisay :: Wasteland" (T.p_done) // T.inj/det "BOTW-097"
        , T.kv_ "Keeha Yoog :: Gerudo" (T.p_todo) // T.inj/det "BOTW-098 :: Cliffside Etchings"
        , T.kv_ "Kuh Takkar :: Gerudo" (T.p_done) // T.inj/det "BOTW-099"
        , T.kv_ "Kema Kosassa :: Gerudo" (T.p_done) // T.inj/det "BOTW-100"
        , T.kv_ "Sasa Kai :: Gerudo" (T.p_todo) // T.inj/det "BOTW-101 :: Sign of the Shadow"
        , T.kv_ "Joloo Nah :: Gerudo" (T.p_todo) // T.inj/det "BOTW-102 :: Test of Will"
        , T.kv_ "Sho Dantu :: Gerudo" (T.p_done) // T.inj/det "BOTW-103"
        , T.kv_ "Shora Hah :: Eldin" (T.p_doing) // T.inj/det "BOTW-104"
        , T.kv_ "Daqa Koh :: Eldin" (T.p_done) // T.inj/det "BOTW-105"
        , T.kv_ "Qua Raym :: Eldin" (T.p_doing) // T.inj/det "BOTW-106"
        , T.kv_ "Tah Muhl :: Eldin" (T.p_done) // T.inj/det "BOTW-107 :: A Landscape of a Stable"
        , T.kv_ "Mo'a Keet :: Eldin" (T.p_doing) // T.inj/det "BOTW-108"
        , T.kv_ "Sah Dahaj :: Eldin" (T.p_doing) // T.inj/det "BOTW-109"
        , T.kv_ "Gorae Torr :: Eldin" (T.p_done) // T.inj/det "BOTW-110 :: The Gut Check Challenge"
        , T.kv_ "Kayra Mah :: Eldin" (T.p_todo) // T.inj/det "BOTW-111 :: A Brother's Roast"
        , T.kv_ "Shae Mo'sah :: Eldin" (T.p_done) // T.inj/det "BOTW-112"
        , T.kv_ "Zuna Kai :: Akkala" (T.p_done) // T.inj/det "BOTW-113 :: The Skull's Eye"
        , T.kv_ "Ze Kasho :: Akkala" (T.p_done) // T.inj/det "BOTW-114"
        , T.kv_ "Ke'nai Shakah :: Akkala" (T.p_done) // T.inj/det "BOTW-115"
        , T.kv_ "Ritaag Zumo :: Akkala" (T.p_done) // T.inj/det "BOTW-116 :: Into the Vortex"
        , T.kv_ "Tutsuwa Nima :: Akkala" (T.p_todo) // T.inj/det "BOTW-117 :: The Spring of Power"
        , T.kv_ "Tu Ka'loh :: Akkala" (T.p_done) // T.inj/det "BOTW-118 :: Trial of the Labyrinth"
        , T.kv_ "Dah Hesho :: Akkala" (T.p_doing) // T.inj/det "BOTW-119"
        , T.kv_ "Katosa Aug :: Akkala" (T.p_done) // T.inj/det "BOTW-120"
        ]

    # T.group "Towers" [ "03-towers" ]
        [ T.kv_ "Great Plateau Tower" (T.p_done) // T.inj/det "TWR-001"
        , T.kv_ "Dueling Peaks Tower" (T.p_done) // T.inj/det "TWR-002"
        , T.kv_ "Hateno Tower" (T.p_done) // T.inj/det "TWR-003"
        , T.kv_ "Lanayru Tower" (T.p_done) // T.inj/det "TWR-004"
        , T.kv_ "Faron Tower" (T.p_done) // T.inj/det "TWR-005"
        , T.kv_ "Lake Tower" (T.p_done) // T.inj/det "TWR-006"
        , T.kv_ "Central Tower" (T.p_done) // T.inj/det "TWR-007"
        , T.kv_ "Woodland Tower" (T.p_done) // T.inj/det "TWR-008"
        , T.kv_ "Eldin Tower" (T.p_done) // T.inj/det "TWR-009"
        , T.kv_ "Akkala Tower" (T.p_done) // T.inj/det "TWR-010"
        , T.kv_ "Hebra Tower" (T.p_done) // T.inj/det "TWR-011"
        , T.kv_ "Tabantha Tower" (T.p_done) // T.inj/det "TWR-012"
        , T.kv_ "Ridgeland Tower" (T.p_done) // T.inj/det "TWR-013"
        , T.kv_ "Wasteland Tower" (T.p_done) // T.inj/det "TWR-014"
        , T.kv_ "Gerudo Tower" (T.p_done) // T.inj/det "TWR-015"
        ]

-- | ID      | Tower Name          | Region Revealed  | Notes                                            |
-- | ------- | ------------------- | ---------------- | ------------------------------------------------ |
-- | TWR-001 | Great Plateau Tower | Great Plateau    | First tower, part of tutorial sequence           |
-- | TWR-002 | Dueling Peaks Tower | Dueling Peaks    | Near Kakariko Village and Hateno Road            |
-- | TWR-003 | Hateno Tower        | Hateno           | On a cliff, surrounded by deep water             |
-- | TWR-004 | Lanayru Tower       | Lanayru          | Located in swampy water                          |
-- | TWR-005 | Faron Tower         | Faron            | Surrounded by tall trees and jungle              |
-- | TWR-006 | Lake Tower          | Lake Hylia       | Isolated on a small island                       |
-- | TWR-007 | Central Tower       | Central Hyrule   | Guarded by multiple Guardians                    |
-- | TWR-008 | Woodland Tower      | Woodland         | Surrounded by swampy area                        |
-- | TWR-009 | Eldin Tower         | Eldin            | In volcanic region, near Gortram Cliff           |
-- | TWR-010 | Akkala Tower        | Akkala           | On tall pillar, needs climbing blocks            |
-- | TWR-011 | Hebra Tower         | Hebra            | Buried in snow/ice, requires melting             |
-- | TWR-012 | Tabantha Tower      | Tabantha         | Surrounded by Calamity sludge                    |
-- | TWR-013 | Ridgeland Tower     | Ridgeland        | On an isolated rock formation                    |
-- | TWR-014 | Wasteland Tower     | Wasteland        | In a swamp near Gerudo Desert edge               |
-- | TWR-015 | Gerudo Tower        | Gerudo Highlands | High on a cliff, needs Revali’s Gale or climbing |


-- BOTW-(\d+),([\w\s]+),([\w\s']+),([\w\s']*)(,(\w*))?
-- , T.kv_ "$3 :: $2" ($5) // T.inj/det "$1 :: $4"


-- DLC-001,Great Plateau,Etsu Korima,EX The Champions' Ballad
-- DLC-002,Great Plateau,Rohta Chigah,EX The Champions' Ballad
-- DLC-003,Great Plateau,Ruvo Korbah,EX The Champions' Ballad
-- DLC-004,Great Plateau,Yowaka Ita,EX The Champions' Ballad
-- DLC-005,Eldin,Kamia Omuna,EX Champion Daruk's Song
-- DLC-006,Eldin,Rinu Honika,EX Champion Daruk's Song
-- DLC-007,Eldin,Sharo Lun,EX Champion Daruk's Song
-- DLC-008,Hebra,Kiah Toza,EX Champion Revali's Song
-- DLC-009,Lanayru,Kee Dafunia,EX Champion Mipha's Song
-- DLC-010,Lanayru,Mah Eliya,EX Champion Mipha's Song
-- DLC-011,Lanayru,Sato Koda,EX Champion Mipha's Song
-- DLC-012,Ridgeland,Shira Gomar,EX Champion Revali's Song
-- DLC-013,Tabantha,Noe Rajee,EX Champion Revali's Song
-- DLC-014,Wasteland,Keive Tala,EX Champion Urbosa's Song
-- DLC-015,Wasteland,Kihiro Moh,EX Champion Urbosa's Song
-- DLC-016,Wasteland,Takama Shiri,EX Champion Urbosa's Song


    )
