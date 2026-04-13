module Test.MusicApi where

import Prelude

import Data.Array (head, length) as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Node.Process (lookupEnv)

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldSatisfy, fail)

import Demo.MusicReport.LastFm as LFM
import Demo.MusicReport.MusicBrainz (MbArtistJ(..), MbReleaseGroupJ(..))
import Demo.MusicReport.MusicBrainz as MB


-- | Artist used across all tests.
testArtist :: String
testArtist = "Radiohead"


spec :: Spec Unit
spec =
    describe "Music APIs" do

        -- ── last.fm ──────────────────────────────────────────────────────────
        -- Requires LASTFM_API_KEY env var; tests are skipped (with a log line)
        -- when it is absent so CI does not fail without credentials.

        describe "last.fm" do

            it "artist.getInfo — name, stats, tags and bio are present" do
                mbKey <- liftEffect $ lookupEnv "LASTFM_API_KEY"
                case mbKey of
                    Nothing ->
                        liftEffect $ Console.log "    Skipping last.fm tests: LASTFM_API_KEY not set"
                    Just key -> do
                        result <- LFM.fetchArtistInfo key testArtist
                        result `shouldSatisfy` (_ /= Nothing)
                        case result of
                            Nothing -> fail "fetchArtistInfo returned Nothing"
                            Just artist -> do
                                artist.name           `shouldSatisfy` (_ /= "")
                                artist.stats.listeners `shouldSatisfy` (_ /= "")
                                Array.length artist.tags.tag `shouldSatisfy` (_ > 0)
                                artist.bio.summary    `shouldSatisfy` (_ /= "")

            it "artist.getTopAlbums — returns at least one album with a non-empty name" do
                mbKey <- liftEffect $ lookupEnv "LASTFM_API_KEY"
                case mbKey of
                    Nothing ->
                        liftEffect $ Console.log "    Skipping last.fm tests: LASTFM_API_KEY not set"
                    Just key -> do
                        result <- LFM.fetchTopAlbums key testArtist 1
                        result `shouldSatisfy` (_ /= Nothing)
                        case result of
                            Nothing -> fail "fetchTopAlbums returned Nothing"
                            Just albums -> do
                                Array.length albums `shouldSatisfy` (_ > 0)
                                case Array.head albums of
                                    Nothing    -> fail "Album list was empty"
                                    Just album -> album.name `shouldSatisfy` (_ /= "")

        -- ── MusicBrainz ──────────────────────────────────────────────────────
        -- No API key needed; all requests must carry a User-Agent header
        -- (handled inside MB.mbGet).  MusicBrainz rate-limits to 1 req/sec
        -- for anonymous access.  Each test waits 1 s before its first request
        -- so back-to-back tests stay within the limit; requests within the same
        -- test are also separated by a 1 s delay.

        describe "MusicBrainz" do

            it "artist search — finds artist and returns a non-empty MBID" do
                delay mbDelay
                result <- MB.fetchArtistSearch testArtist
                result `shouldSatisfy` (_ /= Nothing)
                case result of
                    Nothing   -> fail "fetchArtistSearch returned Nothing"
                    Just resp -> do
                        Array.length resp.artists `shouldSatisfy` (_ > 0)
                        case Array.head resp.artists of
                            Nothing             -> fail "Artist list was empty"
                            Just (MbArtistJ a)  -> do
                                a.id   `shouldSatisfy` (_ /= "")
                                a.name `shouldSatisfy` (_ /= "")

            it "release groups — discography fetched via MBID from search result" do
                -- Two chained requests: search → discography.
                -- A 1 s delay is inserted before each request.
                delay mbDelay
                searchResult <- MB.fetchArtistSearch testArtist
                case searchResult >>= \r -> Array.head r.artists of
                    Nothing            -> fail "Could not obtain artist MBID from search"
                    Just (MbArtistJ a) -> do
                        delay mbDelay
                        discog <- MB.fetchDiscography a.id 0
                        discog `shouldSatisfy` (_ /= Nothing)
                        case discog of
                            Nothing   -> fail "fetchDiscography returned Nothing"
                            Just resp -> do
                                Array.length resp.releaseGroups `shouldSatisfy` (_ > 0)
                                case Array.head resp.releaseGroups of
                                    Nothing                   -> fail "Release-group list was empty"
                                    Just (MbReleaseGroupJ rg) -> do
                                        rg.id    `shouldSatisfy` (_ /= "")
                                        rg.title `shouldSatisfy` (_ /= "")


-- | Minimum gap between MusicBrainz requests: 1 request / second.
mbDelay :: Milliseconds
mbDelay = Milliseconds 300.0
