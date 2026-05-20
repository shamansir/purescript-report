module Test.Samples.ArtistsReport where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Int (fromString) as Int
import Data.Tuple.Nested ((/\), type (/\))

import Foreign as F

import Yoga.JSON as JSON

import Report
import Report as Report
import Report.Chain as C
import Report.Builder as RB
import Report.Class
import Report.Convert.Types (SubjectId, UnitSubject(..), UnitTag(..))
import Report.Convert.Generic (class ToImport, class ToExport)
import Report.GroupPath (pathFromArray) as GP
import Report.Decorator as Decorators
import Report.Decorators.Stats (Stats(..)) as ST
import Report.Tabular as Tabular

import Yoga.Tree.Extended.Convert (toString) as Tree
import Yoga.Tree.Extended.Convert (Mode(..)) as Mode



data ArtistTag
    = Genre String
    | Country String
    | AlbumsCount Int
    -- | YearsActive Int Int


newtype MyGroup = G (Array String)
derive instance Newtype MyGroup _
derive newtype instance Show MyGroup
derive newtype instance Eq MyGroup
derive newtype instance Ord MyGroup


newtype Artist = A String
derive instance Newtype Artist _
derive newtype instance Show Artist
derive newtype instance Eq Artist


instance IsItem        Artist where i_title      = unwrap
instance HasDecorators Artist where i_decorators = const Decorators.empty
instance HasTabular    Artist where i_tabular    = const Tabular.empty


instance IsGroup MyGroup where
    g_title = unwrap >>> map String.toUpper >>> String.joinWith "::"
    g_path = unwrap >>> GP.pathFromArray


instance HasStats MyGroup where i_stats = const ST.SNotRelevant


instance HasTags ArtistTag Artist where
    i_tags = unwrap >>> case _ of
        "NIN" -> [ Country "USA", Genre "Industrial", AlbumsCount 15 ]
        "Queen" -> [ Country "UK", Genre "Rock", Genre "Pop Rock", AlbumsCount 15 ]
        "Rammstein" -> [ Country "Germany", Genre "Rock", Genre "Industrial", AlbumsCount 8 ]
        "The Chemical Brothers" -> [ Country "UK", Genre "Break Beat", Genre "Big Beat", AlbumsCount 10 ]
        "The Prodigy" -> [ Country "UK", Genre "Break Beat", Genre "Big Beat", AlbumsCount 7 ]
        "Nirvana" -> [ Country "USA", Genre "Grunge", AlbumsCount 3 ]
        "Moby" -> [ Country "USA", Genre "Electronic", AlbumsCount 23 ]
        "Massive Attack" -> [ Country "UK", Genre "Trip Hop", AlbumsCount 5 ]
        "GusGus" -> [ Country "Iceland", Genre "Electronic", AlbumsCount 12 ]
        "The Knife" -> [ Country "Sweden", Genre "Electronic", Genre "Synth Pop", AlbumsCount 5 ]
        "Fever Ray" -> [ Country "Sweden", Genre "Electronic", AlbumsCount 3 ]
        "Depeche Mode" -> [ Country "UK", Genre "Electronic", Genre "Synth Pop", Genre "Pop Rock", AlbumsCount 15 ]
        _ -> []


data ArtistTagKind
    = KGenre
    | KCountry
    | KAlbumsCount


derive instance Eq ArtistTagKind


instance IsSortable ArtistTagKind ArtistTag where
    kindOf = case _ of
        Genre _ -> KGenre
        Country _ -> KCountry
        AlbumsCount _ -> KAlbumsCount


instance Same ArtistTagKind where
    same = eq


instance IsTag ArtistTag where
    tagContent = case _ of
        Genre genre -> C.More "Genre" $ C.End genre
        Country country -> C.More "Country" $ C.End country
        AlbumsCount albumsCount -> C.More "Albums #" $ C.End $ show albumsCount
    tagColors = const defaultTagColors



instance ConvertTo (C.Chain String) ArtistTag where
    convertTo = case _ of
        Genre genre     -> C.More "genre"    $ C.End $ genreToId genre
        Country contry  -> C.More "country"  $ C.End $ countryToId contry
        AlbumsCount ac  -> C.More "albums_n" $ C.End $ show ac


instance ConvertFrom (C.Chain String) ArtistTag where
    convertFrom = C.toArray >>> case _ of
        [ "genre", genreId ]        -> genreId     # genreFromId    <#> Genre
        [ "country", countryId ]    -> countryId   # countryFromId  <#> Country
        [ "albums_n", albumsCount ] -> albumsCount # Int.fromString <#> AlbumsCount
        _ -> Nothing


genreToId :: String -> String
genreToId = case _ of
    "Industrial" -> "industrial"
    "Electronic" -> "electronic"
    "Rock" -> "rock"
    "Grunge" -> "grunge"
    "Pop Rock" -> "pop-rock"
    "Break Beat" -> "break-beat"
    "Big Beat" -> "big-beat"
    "Synth Pop" -> "synth-pop"
    "Trip Hop" -> "trip-hop"
    _ -> "?"


genreFromId :: String -> Maybe String
genreFromId = case _ of
    "industrial" -> Just "Industrial"
    "electronic" -> Just "Electronic"
    "rock" -> Just "Rock"
    "grunge" -> Just "Grunge"
    "pop-rock" -> Just "Pop Rock"
    "break-beat" -> Just "Break Beat"
    "big-beat" -> Just "Big Beat"
    "synth-pop" -> Just "Synth Pop"
    "trip-hop" -> Just "Trip Hop"
    _ -> Nothing


countryToId :: String -> String
countryToId = case _ of
    "USA" -> "usa"
    "UK" -> "uk"
    "Sweden" -> "sw"
    "Germany" -> "de"
    "Ireland" -> "ir"
    _ -> "?"


countryFromId :: String -> Maybe String
countryFromId = case _ of
    "usa" -> Just "USA"
    "uk" -> Just "UK"
    "sw" -> Just "Sweden"
    "de" -> Just "Germany"
    "ir" -> Just "Ireland"
    _ -> Nothing


instance ConvertTo (C.Chain String) ArtistTagKind where
    convertTo = mkChainEncode $ case _ of
        KGenre -> "genre"
        KCountry -> "country"
        KAlbumsCount -> "albums_n"


instance ConvertFrom (C.Chain String) ArtistTagKind where
    convertFrom = mkChainDecode $ case _ of
        "genre" -> Just KGenre
        "country" -> Just KCountry
        "albums_n" -> Just KAlbumsCount
        _ -> Nothing


instance IsGroupable MyGroup ArtistTag where
    t_group = case _ of
        Genre genre -> case genre of
            "Pop Rock" ->   Just $ C.More (G [ "Analogue" ]) $ C.More (G [ "Analogue", "Rock" ]) $ C.End $ G [ "Analogue", "Rock", "Pop Rock" ]
            "Rock" ->       Just $ C.More (G [ "Analogue" ]) $ C.End (G [ "Analogue", "Rock" ])
            "Grunge" ->     Just $ C.More (G [ "Analogue" ]) $ C.End (G [ "Analogue", "Grunge" ])
            "Electronic" -> Just $ C.End (G [ "Electronic" ])
            "Industrial" -> Just $ C.More (G [ "Electronic" ]) $ C.End $ G [ "Electronic", "Industrial" ]
            "Synth Pop" ->  Just $ C.More (G [ "Electronic" ]) $ C.End $ G [ "Electronic", "Synth Pop" ]
            "Break Beat" -> Just $ C.More (G [ "Electronic" ]) $ C.More (G [ "Electronic", "Beats" ]) $ C.End $ G [ "Electronic", "Beats", "Break Beat" ]
            "Big Beat" ->   Just $ C.More (G [ "Electronic" ]) $ C.More (G [ "Electronic", "Beats" ]) $ C.End $ G [ "Electronic", "Beats", "Big Beat" ]
            _ -> Nothing
        Country country ->
            case country of
                "UK" -> Just $ C.More (G [ "Europe" ]) $ C.End $ G [ "Europe", "UK" ]
                "Germany" -> Just $ C.More (G [ "Europe" ]) $ C.End $ G [ "Europe", "Germany" ]
                "Sweden" -> Just $ C.More (G [ "Europe" ]) $ C.End $ G [ "Europe", "Sweden" ]
                "USA" -> Just $ C.More (G [ "Americas" ]) $ C.End $ G [ "Americas", "USA" ]
                "Iceland" -> Just $ C.End (G [ "Iceland" ])
                _ -> Nothing
        AlbumsCount acount ->
            Just $
                if acount < 5 then C.End (G [ "Less-than-5" ])
                else if acount < 10 then C.End (G [ "More-than-5" ])
                else if acount < 20 then C.End (G [ "More-than-10" ])
                else C.End (G [ "More-than-20" ])


{-
intToDecimals :: Int -> Array Int
intToDecimals = go [] >Array.reverse
    where
        go prev n =
            let nextPos = n `div` 10
            in if nextPos <= 0
                then 0 : prev
                else
                    if nextPos == 1 then 1 : prev
                    else go (nextPos : prev) nextPos
-}


newtype ArtistReport = AR (Report UnitSubject MyGroup Artist)
derive instance Newtype ArtistReport _


instance ToReport UnitSubject MyGroup Artist ArtistReport where toReport = unwrap


instance ToExport String UnitTag ArtistTag UnitSubject MyGroup Artist ArtistReport


artistsReport =
    RB.buildG
        [ US /\
            [ G [ "root" ] /\
                (A <$>
                    [ "NIN", "Queen", "Rammstein", "The Chemical Brothers"
                    , "The Prodigy", "Nirvana", "Moby", "Massive Attack"
                    , "GusGus", "The Knife", "Fever Ray", "Depeche Mode"
                    ])
            ]
        ]
        # Report.fromBuilder
        # wrap
    :: ArtistReport