let T = ./Types.dhall

let Forza5 = ./ForzaHorizon5.dhall
-- let YakuzaInfiniteWealth = ./Steam/_YakuzaInfiniteWealth.dhall

let AnimalCrossing  = ./AnimalCrossing.dhall
let AstralChain     = ./AstralChain.dhall
let BreathOfTheWild = ./BreathOfTheWild.dhall
let MarioOdissey    = ./MarioOdissey.dhall
let StarlinkBattleOfAtlas = ./StarlinkBattleOfAtlas.dhall
let Torchlight2     = ./Torchlight2.dhall
let Skyrim          = ./Skyrim.dhall

let NonogramsKatana = ./NonogramsKatana.dhall


in
    { collection =
        [ Forza5

        , AnimalCrossing
        , AstralChain
        , BreathOfTheWild
        , MarioOdissey
        , StarlinkBattleOfAtlas
        , Torchlight2
        , Skyrim

        , NonogramsKatana
        ]
    }