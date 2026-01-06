let T = ./Types.dhall

let Platform =
    < Switch
    | Steam
    | GPlay
    | Playstation5
    | IOS
    | Other -- Web? Shared?
    >


let platformToTag
    : Platform -> Text
    = \(p : Platform) ->
        merge
            { Switch = "Switch"
            , Steam = "Steam"
            , GPlay = "GPlay"
            , Playstation5 = "Playstation5"
            , IOS = "IOS"
            , Other = "Other"
            }
            p


let Playtime =
    < Unknown
    | Irrelevant
    | MoreThan : T.TIME
    | Exact : T.TIME
    | LessThan : T.TIME
    >


let Game =
    { name : Text
    , id : Text
    , platform : Platform
    , playtime : Playtime
    }


let AchievementsData =
    { game : Game
    , trackedAt : Optional T.DATE
    , properties : List T.Property
    }


let AD/collectTabular
    : AchievementsData -> List T.TabularKVR
    = \(ach : AchievementsData) ->
        [
            { key = "trackedAt"
            , value =
                T.tag (
                    merge
                        { None = T.v_unk
                        , Some = \(date : T.DATE) -> T.v_date date
                        }
                    ach.trackedAt
                )
            }
        ,   { key = "playtime"
            , value =
                T.tag (
                    merge
                        { Unknown = T.v_unk
                        , Irrelevant = T.v_unk
                        , MoreThan = \(time : T.TIME) -> T.v_relt (T.rel_more_than T.TIME time)
                        , Exact = \(time : T.TIME)    -> T.v_relt (T.rel_exact     T.TIME time)
                        , LessThan = \(time : T.TIME) -> T.v_relt (T.rel_less_than T.TIME time)
                        }
                    ach.game.playtime
                )
            }
        ]


let AD/toSubject
    : AchievementsData -> T.Subject
    = \(ach : AchievementsData) ->
        { name = ach.game.name
        , id = ach.game.id
        , properties = ach.properties
        , tabular = AD/collectTabular ach
        , tags = [ platformToTag ach.game.platform ]
        }


let collapseAD
    : Game -> List T.Property -> AchievementsData
    = \(game : Game) -> \(properties : List T.Property) ->
        { trackedAt = None T.DATE
        , properties = properties
        , game = game
        }


let collapse
    : Game -> List T.Property -> T.Subject
    = \(game : Game) -> \(properties : List T.Property) ->
        AD/toSubject (collapseAD game properties)


let collapseAtAD
    : Game -> T.DATE -> List T.Property -> AchievementsData
    = \(game : Game) -> \(date : T.DATE) -> \(properties : List T.Property) ->
        { trackedAt = Some date
        , properties = properties
        , game = game
        }


let collapseAt
    : Game -> T.DATE -> List T.Property -> T.Subject
    = \(game : Game) -> \(date : T.DATE) -> \(properties : List T.Property) ->
        AD/toSubject (collapseAtAD game date properties)


let introduceAD
    : Game -> AchievementsData
    = \(game : Game) ->
        { trackedAt = None T.DATE
        , properties = ([] : List T.Property)
        , game = game
        }


let introduce
    : Game -> T.Subject
    = \(game : Game) ->
        AD/toSubject (introduceAD game)



in { Platform, Playtime, Game, AchievementsData
   , collapse, collapseAt, introduce
   }