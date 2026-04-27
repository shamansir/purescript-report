module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Aff (runAff_)

import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.ReportImpl as ReportImpl
import Test.Report as Report
import Test.Groups as Groups
import Test.ParseTree as ParseTree
import Test.MusicApi as MusicApi
import Test.Export.DhallDodo as DhallDodo
import Test.Export.OrgDodo as OrgDodo
import Test.Export.TextDodo as TextDodo
import Test.Export.RepDodo as RepDodo


main :: Effect Unit
main = do
    -- runAff_ (const $ pure unit) $ MusicApi.fetchArtist Nothing "Queen"

    runSpecAndExitProcess [consoleReporter] do
        DhallDodo.spec
        OrgDodo.spec
        TextDodo.spec
        RepDodo.spec
        Report.spec
        ReportImpl.spec
        Groups.spec
        ParseTree.spec
        -- MusicApi.spec