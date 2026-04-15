module Test.Demo.MusicReport.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Svg.Attributes (m)

type Genre = String


type Country = String


data Tag
    = AlbumTag AlbumTag
    | ArtistTag ArtistTag
    | TrackTag TrackTag


data AlbumTag
    = SYear Int
    | SGenre Genre
    | SCountry Country


data ArtistTag
    = AGenre Genre
    | ACountry Country
    | AAlbumsCount Int


data TrackTag
    = TLength { minutes :: Int, seconds :: Int }


type LifeSpan =
    { begin  :: Maybe String
    , end    :: Maybe String
    , ended  :: Maybe Boolean
    }


newtype Artist = Artist
    { id        :: String
    , name      :: String
    -- , sortName  :: String
    , country   :: Maybe Country
    -- , area      :: Maybe MbArea
    , lifeSpan  :: Maybe LifeSpan
    -- , tags      :: Maybe (Array ArtistTag)
    , discorgraphy :: Array ReleaseGroup
    }


newtype Album = Album
    { name      :: String
    , artist    :: Artist
    , playcount :: String
    -- , tags :: Array AlbumTag
    , tracks :: Array Track
    }


newtype Track = Track
    { id       :: String
    , number   :: String   -- display number, e.g. "A1" on vinyl
    , position :: Int
    , title    :: String
    , length   :: Maybe { minutes :: Int, seconds :: Int }
    }


type ReleaseGroup =
    { id               :: String
    , title            :: String
    , primaryType      :: Maybe String   -- "primary-type": "Album" | "Single" | "EP" | …
    , secondaryTypes   :: Array String   -- "secondary-types": ["Live"], ["Compilation"], …
    , firstReleaseDate :: Maybe String   -- "first-release-date": partial date string
    , releases         :: Array Release
    }


type Medium =
    { position   :: Int
    , format     :: Maybe String  -- "CD", "Vinyl", "Digital Media", …
    , trackCount :: Int           -- "track-count"
    , tracks     :: Array Track
    }


type Release =
    { id      :: String
    , title   :: String
    , date    :: Maybe String  -- partial date, e.g. "1991" or "1991-09-24"
    , country :: Maybe String
    , status  :: Maybe String  -- "Official" | "Promotion" | "Bootleg" | …
    , media   :: Array Medium
    }


data Subj
    = ArtistSubj Artist
    | GenreSubj Genre
    | CountrySubj Country


data Item
    = AlbumItem Album
    | TrackItem Track