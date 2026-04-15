module Test.Demo.MusicReport.Convert where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Test.Demo.MusicReport.Types
import Demo.MusicReport.MusicBrainz



mbTrackToTrack :: MbTrack -> Track
mbTrackToTrack t = Track
  { id: t.id
  , number: t.number
  , position: t.position
  , title: t.title
  , length: case t.length of
      Just ms ->
        let
          minutes = ms `div` 60000
          seconds = (ms `mod` 60000) `div` 1000
        in
          Just { minutes, seconds }
      Nothing -> Nothing
  }

mbMediumToMedium :: MbMedium -> Medium
mbMediumToMedium m =
  { position: m.position
  , format: m.format
  , trackCount: m.trackCount
  , tracks: map mbTrackToTrack m.tracks
  }

mbReleaseGroupToReleaseGroup :: MbReleaseGroup -> Array MbRelease -> ReleaseGroup
mbReleaseGroupToReleaseGroup rg releases =
  { id: rg.id
  , title: rg.title
  , primaryType: rg.primaryType
  , secondaryTypes: rg.secondaryTypes
  , firstReleaseDate: rg.firstReleaseDate
  , releases: mbReleaseToRelease <$> releases
  }

mbReleaseToRelease :: MbRelease -> Release
mbReleaseToRelease r =
  { id: r.id
  , title: r.title
  , date: r.date
  , country: r.country
  , status: r.status
  , media: mbMediumToMedium <$> unwrap <$> r.media
  }

artistFromMbArtist :: MbArtist -> Array (MbReleaseGroup /\ Array MbRelease) -> Artist
artistFromMbArtist artist releaseGroups =
  Artist
    { id: artist.id
    , name: artist.name
    , country: artist.country
    , lifeSpan: artist.lifeSpan
    , discorgraphy : uncurry mbReleaseGroupToReleaseGroup <$> releaseGroups
    }