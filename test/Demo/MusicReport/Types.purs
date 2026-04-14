module Test.Demo.MusicReport.Types where

import Prelude

import Test.Report (ArtistTag(..))


type Genre = String


type Country = String


data Tag
    = Year Int
    | Genre Genre


newtype Artist = Artist
    { name :: String
    , tags :: Array Tag
    }


newtype Album = Album
    { name      :: String
    , artist    :: Artist
    , playcount :: String
    , tags :: Array Tag
    }


newtype Track = Track
    { name      :: String
    , length    :: { minutes :: Int, seconds :: Int }
    }


data Subj
    = ArtistSubj Artist
    | GenreSubj Genre
    | CountrySubj Country


data Item
    = AlbumItem Album
    | TrackItem Track