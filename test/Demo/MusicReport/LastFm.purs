module Demo.MusicReport.LastFm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.String (Pattern(..), Replacement(..), replaceAll)

import Effect.Aff (Aff)

import Affjax.Node (get) as Aj
import Affjax.ResponseFormat (string) as Aj

import Yoga.JSON (readJSON_, writeJSON)

-- All types in this module are plain record type aliases, so Yoga.JSON's
-- generic record machinery provides both ReadForeign and WriteForeign for
-- them automatically — no explicit instances needed.
-- Use `writeJSON :: WriteForeign a => a -> String` to serialise any value.


-- ─── Helpers ─────────────────────────────────────────────────────────────────

-- | Minimal URI encoding for artist names (spaces → %20).
-- | For production, bind a proper JS encodeURIComponent via FFI.
encodeComponent :: String -> String
encodeComponent =
    replaceAll (Pattern " ") (Replacement "%20")
  >>> replaceAll (Pattern "&") (Replacement "%26")
  >>> replaceAll (Pattern "/") (Replacement "%2F")


-- ─── API base ────────────────────────────────────────────────────────────────

lfmBase :: String
lfmBase = "https://ws.audioscrobbler.com/2.0/"


-- ─── Types ───────────────────────────────────────────────────────────────────
--
-- Field names follow the last.fm JSON response exactly.
-- ReadForeign instances are auto-derived via Yoga.JSON record machinery
-- as long as all field names are valid PureScript identifiers.
--
-- Exceptions / caveats:
--   • stats.listeners / stats.playcount arrive as strings, not numbers.
--   • The "@attr" pagination object has an invalid PS identifier as key;
--     it is omitted here — decode it separately if needed.
--   • Image objects contain a "#text" field; also omitted for the same reason.

-- | A genre or style tag.
type LfmTag =
    { name :: String
    , url  :: String
    }

-- | Play statistics for an artist.
-- | last.fm sends listeners/playcount as quoted strings, e.g. "4461182".
type LfmStats =
    { listeners :: String
    , playcount :: String
    }

-- | Biography block.
type LfmBio =
    { summary   :: String
    , content   :: String
    , published :: String
    }

-- | Slim artist reference used inside album and similar-artist lists.
-- | mbid is absent from similar-artist entries, present in album-artist entries.
type LfmArtistRef =
    { name :: String
    , mbid :: Maybe String
    , url  :: String
    }

-- | Full artist record from artist.getInfo.
type LfmArtist =
    { name    :: String
    , mbid    :: String
    , url     :: String
    , stats   :: LfmStats
    -- last.fm wraps the tag array: { "tags": { "tag": [ … ] } }
    , tags    :: { tag :: Array LfmTag }
    , bio     :: LfmBio
    , similar :: { artist :: Array LfmArtistRef }
    }

type LfmArtistInfoResponse =
    { artist :: LfmArtist }


-- | One album entry from artist.getTopAlbums.
-- | mbid is absent for releases not linked to MusicBrainz.
type LfmAlbum =
    { name      :: String
    , playcount :: Int
    , mbid      :: Maybe String
    , url       :: String
    , artist    :: LfmArtistRef
    }

-- | The "topalbums" body (omits "@attr" pagination — see note above).
type LfmTopAlbumsBody =
    { album :: Array LfmAlbum }

type LfmTopAlbumsResponse =
    { topalbums :: LfmTopAlbumsBody }


-- | One tag from artist.getTopTags.
type LfmTopTagsBody =
    { tag :: Array LfmTag }

type LfmTopTagsResponse =
    { toptags :: LfmTopTagsBody }


-- ─── URL builders ────────────────────────────────────────────────────────────

artistInfoUrl :: String -> String -> String
artistInfoUrl apiKey name =
    lfmBase
        <> "?method=artist.getinfo"
        <> "&artist="   <> encodeComponent name
        <> "&api_key="  <> apiKey
        <> "&autocorrect=1"
        <> "&format=json"

topAlbumsUrl :: String -> String -> Int -> String
topAlbumsUrl apiKey name page =
    lfmBase
        <> "?method=artist.gettopalbums"
        <> "&artist="  <> encodeComponent name
        <> "&api_key=" <> apiKey
        <> "&limit=50"
        <> "&page="    <> show page
        <> "&format=json"

topTagsUrl :: String -> String -> String
topTagsUrl apiKey name =
    lfmBase
        <> "?method=artist.gettoptags"
        <> "&artist="  <> encodeComponent name
        <> "&api_key=" <> apiKey
        <> "&format=json"


-- ─── Fetch functions ─────────────────────────────────────────────────────────

-- | Fetch the full artist info block: bio, stats, tags, similar artists.
fetchArtistInfo :: String -> String -> Aff (Maybe LfmArtist)
fetchArtistInfo apiKey name = do
    result <- Aj.get Aj.string (artistInfoUrl apiKey name)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body >>= \(resp :: LfmArtistInfoResponse) ->
        Just resp.artist
        ) result

-- | Fetch one page of top albums (up to 50 per page, 1-indexed).
fetchTopAlbums :: String -> String -> Int -> Aff (Maybe (Array LfmAlbum))
fetchTopAlbums apiKey name page = do
    result <- Aj.get Aj.string (topAlbumsUrl apiKey name page)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body >>= \(resp :: LfmTopAlbumsResponse) ->
        Just resp.topalbums.album
        ) result

-- | Fetch the artist's top genre tags.
fetchTopTags :: String -> String -> Aff (Maybe (Array LfmTag))
fetchTopTags apiKey name = do
    result <- Aj.get Aj.string (topTagsUrl apiKey name)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body >>= \(resp :: LfmTopTagsResponse) ->
        Just resp.toptags.tag
        ) result
