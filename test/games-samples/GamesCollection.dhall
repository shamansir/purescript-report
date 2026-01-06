let T = ./Types.dhall
let GT = ./Game.Types.dhall

let Forza5 = ./ForzaHorizon5.dhall
-- let YakuzaInfiniteWealth = ./Steam/_YakuzaInfiniteWealth.dhall

let AnimalCrossing  = ./AnimalCrossing.dhall
let AstralChain     = ./AstralChain.dhall
let BreathOfTheWild = ./BreathOfTheWild.dhall
let DoomDarkAges    = ./DoomDarkAges.dhall
let KirbyTheForgottenLand = ./KirbyTheForgottenLand.dhall
let LegoCityUndercover = ./LegoCityUndercover.dhall
let MarioOdissey    = ./MarioOdissey.dhall
let StarlinkBattleOfAtlas = ./StarlinkBattleOfAtlas.dhall
let NoRestForTheWicked = ./NoRestForTheWicked.dhall
let Torchlight2     = ./Torchlight2.dhall
let Skyrim          = ./Skyrim.dhall

let NonogramsKatana = ./NonogramsKatana.dhall


in
    { collection =
        [ Forza5

        , AnimalCrossing
        , AstralChain
        , BreathOfTheWild
        , DoomDarkAges
        , KirbyTheForgottenLand
        , LegoCityUndercover
        , MarioOdissey
        , NoRestForTheWicked
        , StarlinkBattleOfAtlas
        , Torchlight2
        , Skyrim

        , NonogramsKatana
        ]
    }