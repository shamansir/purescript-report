module Test.MusicApi where

import Prelude

import Control.Parallel (parTraverse)
import Control.Applicative.Extra (whenJust, whenJustE)

import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.Array (head, length, take, zip) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))

import Node.Process (lookupEnv)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

import Yoga.JSON (writePrettyJSON)

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldSatisfy, fail)

import Demo.MusicReport.LastFm as LFM
import Demo.MusicReport.MusicBrainz (MbArtistJ(..), MbMediumJ(..), MbReleaseGroupJ(..))
import Demo.MusicReport.MusicBrainz as MB


-- | Artists used across all tests.
testArtists :: Array String
testArtists = [ "Radiohead", "Massive Attack", "Portishead" ]



spec :: Spec Unit
spec =
    describe "Music APIs" do

        -- ── last.fm ──────────────────────────────────────────────────────────
        -- Requires LASTFM_API_KEY env var; tests are skipped (with a log line)
        -- when it is absent so CI does not fail without credentials.
        -- Multiple artists are fetched in parallel (no strict rate limit).

        describe "last.fm" do

            it "artist.getInfo — fetches bio, stats and tags for multiple artists in parallel" do
                mbKey <- liftEffect $ lookupEnv "LASTFM_API_KEY"
                case mbKey of
                    Nothing ->
                        liftEffect $ Console.log "    Skipping last.fm tests: LASTFM_API_KEY not set"
                    Just key -> do
                        results <- parTraverse (LFM.fetchArtistInfo key) testArtists
                        for_ (Array.zip testArtists results) \(Tuple name result) -> do
                            when (isNothing result) $
                                fail $ "fetchArtistInfo returned Nothing for " <> name
                            whenJust result \artist -> do
                                artist.name            `shouldSatisfy` (_ /= "")
                                artist.stats.listeners `shouldSatisfy` (_ /= "")
                                Array.length artist.tags.tag `shouldSatisfy` (_ > 0)
                                artist.bio.summary     `shouldSatisfy` (_ /= "")
                                liftEffect $ writeTextFile UTF8 ("./test/fetched-data/artist-lastfm-info-" <> name <> ".json") $ writePrettyJSON 2 artist


            it "artist.getTopAlbums — returns albums for multiple artists in parallel" do
                mbKey <- liftEffect $ lookupEnv "LASTFM_API_KEY"
                case mbKey of
                    Nothing ->
                        liftEffect $ Console.log "    Skipping last.fm tests: LASTFM_API_KEY not set"
                    Just key -> do
                        results <- parTraverse (\name -> LFM.fetchTopAlbums key name 1) testArtists
                        for_ (Array.zip testArtists results) \(Tuple name result) -> do
                            when (isNothing result) $
                                fail $ "fetchTopAlbums returned Nothing for " <> name
                            whenJust result \albums -> do
                                Array.length albums `shouldSatisfy` (_ > 0)
                                case Array.head albums of
                                    Nothing    -> fail $ "Album list was empty for " <> name
                                    Just album -> album.name `shouldSatisfy` (_ /= "")
                                liftEffect $ writeTextFile UTF8 ("./test/fetched-data/artist-lastfm-albums-" <> name <> ".json") $ writePrettyJSON 2 albums

        -- ── MusicBrainz ──────────────────────────────────────────────────────
        -- No API key needed; rate-limited to 50 req/sec.
        -- All requests are sequential with mbDelay between each.

        describe "MusicBrainz" do

            it "artist search — finds multiple artists sequentially with rate limiting" do
                results <- for testArtists \name -> do
                    delay mbDelay
                    MB.fetchArtistSearch name
                for_ (Array.zip testArtists results) \(Tuple name result) -> do
                    when (isNothing result) $
                        fail $ "fetchArtistSearch returned Nothing for " <> name
                    whenJust result \resp -> do
                        Array.length resp.artists `shouldSatisfy` (_ > 0)
                        case Array.head resp.artists of
                            Nothing            -> fail $ "Artist list was empty for " <> name
                            Just (MbArtistJ a) -> do
                                a.id   `shouldSatisfy` (_ /= "")
                                a.name `shouldSatisfy` (_ /= "")
                        liftEffect $ writeTextFile UTF8 ("./test/fetched-data/artists-mbrainz-" <> name <> ".json") $ writePrettyJSON 2 resp.artists

            it "release groups — fetches discography for multiple artists sequentially" do
                for_ testArtists \name -> do
                    delay mbDelay
                    searchResult <- MB.fetchArtistSearch name
                    case searchResult >>= \r -> Array.head r.artists of
                        Nothing            ->
                            fail $ "Could not get artist MBID for " <> name
                        Just (MbArtistJ a) -> do
                            delay mbDelay
                            discog <- MB.fetchDiscography a.id 0
                            when (isNothing discog) $
                                fail $ "fetchDiscography returned Nothing for " <> name
                            whenJust discog \resp -> do
                                Array.length resp.releaseGroups `shouldSatisfy` (_ > 0)
                                case Array.head resp.releaseGroups of
                                    Nothing                   -> fail $ "Release-group list was empty for " <> name
                                    Just (MbReleaseGroupJ rg) -> do
                                        rg.id    `shouldSatisfy` (_ /= "")
                                        rg.title `shouldSatisfy` (_ /= "")
                                liftEffect $ writeTextFile UTF8 ("./test/fetched-data/artists-mbrainz-discography-" <> name <> ".json") $ writePrettyJSON 2 resp.releaseGroups

            it "release tracks — fetches track listings for 2 albums from Radiohead's discography" do
                let artistName = "Radiohead"
                delay mbDelay
                searchResult <- MB.fetchArtistSearch artistName
                case searchResult >>= \r -> Array.head r.artists of
                    Nothing            -> fail $ "Could not find artist " <> artistName
                    Just (MbArtistJ mbArtist) -> do
                        delay mbDelay
                        discog <- MB.fetchDiscography mbArtist.id 0
                        case discog of
                            Nothing   -> fail "fetchDiscography returned Nothing"
                            Just resp -> do
                                let rgs = Array.take 2 resp.releaseGroups
                                Array.length rgs `shouldSatisfy` (_ > 0)
                                _ <- for rgs \(MbReleaseGroupJ rg) -> do
                                    delay mbDelay
                                    releases <- MB.fetchReleasesByGroup rg.id
                                    whenJust (releases >>= Array.head) \mbRelease -> do
                                        delay mbDelay
                                        release <- MB.fetchRelease mbRelease.id
                                        whenJust release \rel -> do
                                            Array.length rel.media `shouldSatisfy` (_ > 0)
                                            case Array.head rel.media of
                                                Nothing             -> fail $ "No medium in release " <> rel.title
                                                Just (MbMediumJ m) -> do
                                                    Array.length m.tracks `shouldSatisfy` (_ > 0)
                                                    case Array.head m.tracks of
                                                        Nothing -> fail $ "No tracks in first medium of " <> rel.title
                                                        Just t  -> t.title `shouldSatisfy` (_ /= "")
                                            liftEffect $ writeTextFile UTF8
                                                ("./test/fetched-data/release-tracks-" <> artistName <> "-" <> rg.title <> ".json")
                                                (writePrettyJSON 2 rel)
                                pure unit


-- | Minimum gap between MusicBrainz requests: 50 request / second, for safety make it bigger.
mbDelay :: Milliseconds
mbDelay = Milliseconds 300.0
