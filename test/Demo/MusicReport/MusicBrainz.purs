module Demo.MusicReport.MusicBrainz where

import Prelude

import Effect.Aff (Aff)

-- import Control.Monad.ST.Internal (new)
import Foreign (Foreign, F, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Object (fromFoldable) as FO

import Data.Tuple (Tuple(..))

import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Newtype (unwrap, wrap, class Newtype)

import Affjax.Node (Error, Response, request, defaultRequest, printError) as Aj
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (string) as RF

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl, readJSON_, writeJSON)


-- ─── API base & policy ───────────────────────────────────────────────────────

mbBase :: String
mbBase = "https://musicbrainz.org/ws/2/"

-- | MusicBrainz requires a descriptive User-Agent; requests without one are
-- | blocked. Rate limit: 1 request / second for anonymous access.
mbUserAgent :: String
mbUserAgent = "purescript-report-demo/0.1 (https://github.com/shamansir/purescript-report)"

-- | Minimal URI encoding.
encodeComponent :: String -> String
encodeComponent =
    replaceAll (Pattern " ") (Replacement "%20")
  >>> replaceAll (Pattern "&") (Replacement "%26")
  >>> replaceAll (Pattern "/") (Replacement "%2F")


-- ─── Plain types (auto-derived ReadForeign via Yoga.JSON records) ─────────────

-- | Country / region area.
type MbArea =
    { id   :: String
    , name :: String
    }

-- | Active years of an artist.
-- | "ended" is a boolean; begin/end are partial dates ("1991", "1991-09", "1991-09-24").
type MbLifeSpan =
    { begin  :: Maybe String
    , end    :: Maybe String
    , ended  :: Maybe Boolean
    }

-- | Genre / style tag with community vote count.
type MbTag =
    { count :: Int
    , name  :: String
    }

-- | Single track inside a release medium.
-- | "length" is the duration in milliseconds; null when unknown.
type MbTrack =
    { id       :: String
    , number   :: String   -- display number, e.g. "A1" on vinyl
    , position :: Int
    , title    :: String
    , length   :: Maybe Int
    }


-- ─── Types with hyphenated JSON keys (custom ReadForeign) ────────────────────
--
-- MusicBrainz uses kebab-case in its JSON (sort-name, life-span, release-groups,
-- first-release-date, track-count, …).  PureScript record fields cannot contain
-- hyphens, so we rename them to camelCase and write manual ReadForeign instances
-- using Foreign.Index.readProp to access the raw key names.

-- | Artist as returned by /artist/{mbid} or inside a search result.
type MbArtist =
    { id        :: String
    , name      :: String
    , sortName  :: String          -- "sort-name"
    , country   :: Maybe String
    , area      :: Maybe MbArea
    , lifeSpan  :: Maybe MbLifeSpan  -- "life-span"
    , tags      :: Maybe (Array MbTag)
    }

newtype MbArtistJ = MbArtistJ MbArtist
derive newtype instance Show MbArtistJ
derive newtype instance Eq MbArtistJ
derive instance Newtype MbArtistJ _

instance ReadForeign MbArtistJ where
    readImpl :: Foreign -> F MbArtistJ
    readImpl f = do
        id       <- readImpl =<< readProp "id"        f
        name     <- readImpl =<< readProp "name"      f
        sortName <- readImpl =<< readProp "sort-name" f
        country  <- readImpl =<< readProp "country"   f
        area     <- readImpl =<< readProp "area"      f
        lifeSpan <- readImpl =<< readProp "life-span" f
        tags     <- readImpl =<< readProp "tags"      f
        pure $ MbArtistJ { id, name, sortName, country, area, lifeSpan, tags }

instance WriteForeign MbArtistJ where
    writeImpl (MbArtistJ a) = unsafeToForeign $ FO.fromFoldable
        [ Tuple "id"        (writeImpl a.id)
        , Tuple "name"      (writeImpl a.name)
        , Tuple "sort-name" (writeImpl a.sortName)
        , Tuple "country"   (writeImpl a.country)
        , Tuple "area"      (writeImpl a.area)
        , Tuple "life-span" (writeImpl a.lifeSpan)
        , Tuple "tags"      (writeImpl a.tags)
        ]


-- | Artist search response wrapper.
type MbArtistSearchResponse =
    { artists :: Array MbArtistJ
    , count   :: Int
    , offset  :: Int
    }


-- | A release group = an album / EP / single as a logical entity
-- | (may have multiple regional releases).
type MbReleaseGroup =
    { id               :: String
    , title            :: String
    , primaryType      :: Maybe String   -- "primary-type": "Album" | "Single" | "EP" | …
    , secondaryTypes   :: Array String   -- "secondary-types": ["Live"], ["Compilation"], …
    , firstReleaseDate :: Maybe String   -- "first-release-date": partial date string
    }

newtype MbReleaseGroupJ = MbReleaseGroupJ MbReleaseGroup
derive newtype instance Show MbReleaseGroupJ
derive newtype instance Eq MbReleaseGroupJ
derive instance Newtype MbReleaseGroupJ _

instance ReadForeign MbReleaseGroupJ where
    readImpl :: Foreign -> F MbReleaseGroupJ
    readImpl f = do
        id               <- readImpl =<< readProp "id"                 f
        title            <- readImpl =<< readProp "title"              f
        primaryType      <- readImpl =<< readProp "primary-type"       f
        secondaryTypes   <- readImpl =<< readProp "secondary-types"    f
        firstReleaseDate <- readImpl =<< readProp "first-release-date" f
        pure $ MbReleaseGroupJ { id, title, primaryType, secondaryTypes, firstReleaseDate }

instance WriteForeign MbReleaseGroupJ where
    writeImpl (MbReleaseGroupJ rg) = unsafeToForeign $ FO.fromFoldable
        [ Tuple "id"                 (writeImpl rg.id)
        , Tuple "title"              (writeImpl rg.title)
        , Tuple "primary-type"       (writeImpl rg.primaryType)
        , Tuple "secondary-types"    (writeImpl rg.secondaryTypes)
        , Tuple "first-release-date" (writeImpl rg.firstReleaseDate)
        ]


-- | Paginated release-group list from /release-group?artist={mbid}
type MbReleaseGroupsResponse =
    { releaseGroups      :: Array MbReleaseGroupJ  -- "release-groups"
    , releaseGroupCount  :: Int                    -- "release-group-count"
    , releaseGroupOffset :: Int                    -- "release-group-offset"
    }

newtype MbReleaseGroupsResponseJ = MbReleaseGroupsResponseJ MbReleaseGroupsResponse
derive instance Newtype MbReleaseGroupsResponseJ _

instance ReadForeign MbReleaseGroupsResponseJ where
    readImpl :: Foreign -> F MbReleaseGroupsResponseJ
    readImpl f = do
        releaseGroups      <- readImpl =<< readProp "release-groups"       f
        releaseGroupCount  <- readImpl =<< readProp "release-group-count"  f
        releaseGroupOffset <- readImpl =<< readProp "release-group-offset" f
        pure $ MbReleaseGroupsResponseJ { releaseGroups, releaseGroupCount, releaseGroupOffset }

instance WriteForeign MbReleaseGroupsResponseJ where
    writeImpl (MbReleaseGroupsResponseJ r) = unsafeToForeign $ FO.fromFoldable
        [ Tuple "release-groups"       (writeImpl r.releaseGroups)
        , Tuple "release-group-count"  (writeImpl r.releaseGroupCount)
        , Tuple "release-group-offset" (writeImpl r.releaseGroupOffset)
        ]


-- | One disc / side / medium in a release, with its track listing.
type MbMedium =
    { position   :: Int
    , format     :: Maybe String  -- "CD", "Vinyl", "Digital Media", …
    , trackCount :: Int           -- "track-count"
    , tracks     :: Array MbTrack
    }

newtype MbMediumJ = MbMediumJ MbMedium
derive instance Newtype MbMediumJ _

instance ReadForeign MbMediumJ where
    readImpl :: Foreign -> F MbMediumJ
    readImpl f = do
        position   <- readImpl =<< readProp "position"    f
        format     <- readImpl =<< readProp "format"      f
        trackCount <- readImpl =<< readProp "track-count" f
        tracks     <- readImpl =<< readProp "tracks"      f
        pure $ MbMediumJ { position, format, trackCount, tracks }

instance WriteForeign MbMediumJ where
    writeImpl (MbMediumJ m) = unsafeToForeign $ FO.fromFoldable
        [ Tuple "position"    (writeImpl m.position)
        , Tuple "format"      (writeImpl m.format)
        , Tuple "track-count" (writeImpl m.trackCount)
        , Tuple "tracks"      (writeImpl m.tracks)
        ]


-- | A concrete release (pressing) with its full track listing.
type MbRelease =
    { id      :: String
    , title   :: String
    , date    :: Maybe String  -- partial date, e.g. "1991" or "1991-09-24"
    , country :: Maybe String
    , status  :: Maybe String  -- "Official" | "Promotion" | "Bootleg" | …
    , media   :: Array MbMediumJ
    }

newtype MbReleaseJ = MbReleaseJ MbRelease
derive instance Newtype MbReleaseJ _

derive newtype instance WriteForeign MbReleaseJ

instance ReadForeign MbReleaseJ where
    readImpl :: Foreign -> F MbReleaseJ
    readImpl f = do
        id      <- readImpl =<< readProp "id"       f
        title   <- readImpl =<< readProp "title"    f
        date    <- readImpl =<< readProp "date"     f
        country <- readImpl =<< readProp "country"  f
        status  <- readImpl =<< readProp "status"   f
        media   <- readImpl =<< readProp "media"    f
        pure $ MbReleaseJ { id, title, date, country, status, media }


-- | Releases listed inside a release-group (slim, no track data).
type MbReleaseStub =
    { id     :: String
    , title  :: String
    , date   :: Maybe String
    , status :: Maybe String
    }


-- ─── URL builders ────────────────────────────────────────────────────────────

searchArtistUrl :: String -> String
searchArtistUrl name =
    mbBase <> "artist?query=artist:" <> encodeComponent name
           <> "&fmt=json&limit=5"

artistUrl :: String -> String
artistUrl mbid =
    mbBase <> "artist/" <> mbid
           <> "?inc=tags&fmt=json"

-- | All studio albums for an artist (offset for pagination, 100 per page).
releaseGroupsUrl :: String -> Int -> String
releaseGroupsUrl mbid offset =
    mbBase <> "release-group?artist=" <> mbid
           <> "&type=album"
           <> "&fmt=json&limit=100&offset=" <> show offset

-- | All release types (albums, EPs, singles, live, …).
allReleaseGroupsUrl :: String -> Int -> String
allReleaseGroupsUrl mbid offset =
    mbBase <> "release-group?artist=" <> mbid
           <> "&fmt=json&limit=100&offset=" <> show offset

-- | All releases (pressings) belonging to a specific release group.
releasesByGroupUrl :: String -> String
releasesByGroupUrl rgMbid =
    mbBase <> "release?release-group=" <> rgMbid
           <> "&fmt=json&limit=5"

-- | A specific release with full recording/track data included.
releaseUrl :: String -> String
releaseUrl mbid =
    mbBase <> "release/" <> mbid
           <> "?inc=recordings&fmt=json"


-- ─── Fetch helpers ───────────────────────────────────────────────────────────

-- | MusicBrainz requires a User-Agent header on every request.
mbGet :: String -> Aff (Either Aj.Error (Aj.Response String))
mbGet url =
    Aj.request $ Aj.defaultRequest
        { url            = url
        , responseFormat = RF.string
        , headers        = [ RequestHeader "User-Agent" mbUserAgent ]
        }

-- | Search for an artist by name; returns the best-scoring match.
-- | The "score" field in each result indicates match confidence (0–100).
fetchArtistSearch :: String -> Aff (Maybe MbArtistSearchResponse)
fetchArtistSearch name = do
    result <- mbGet (searchArtistUrl name)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body
        ) result

-- | Fetch discography (studio albums) for an artist MBID.
-- | Call repeatedly with increasing offsets to page through large discographies.
fetchDiscography :: String -> Int -> Aff (Maybe MbReleaseGroupsResponse)
fetchDiscography mbid offset = do
    result <- mbGet (releaseGroupsUrl mbid offset)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body >>= \(MbReleaseGroupsResponseJ resp) ->
        Just resp
        ) result

-- | Fetch the list of pressings (releases) for a given release group.
-- | Returns slim stubs; use fetchRelease to get full track data for a pressing.
fetchReleasesByGroup :: String -> Aff (Maybe (Array MbReleaseStub))
fetchReleasesByGroup rgMbid = do
    result <- mbGet (releasesByGroupUrl rgMbid)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body >>= \(resp :: { releases :: Array MbReleaseStub }) ->
        Just resp.releases
        ) result

-- | Fetch a concrete release with its full track listing.
-- | Use the MBID of a specific pressing (from a release-group's releases list).
fetchRelease :: String -> Aff (Maybe MbRelease)
fetchRelease mbid = do
    result <- mbGet (releaseUrl mbid)
    pure $ either (const Nothing) (\r ->
        readJSON_ r.body >>= \(MbReleaseJ release) ->
        Just release
        ) result
