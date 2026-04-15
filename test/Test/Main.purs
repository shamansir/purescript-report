module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Aff (runAff_)

import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.DhallDodo as DhallDodo
import Test.OrgDodo as OrgDodo
import Test.TextDodo as TextDodo
import Test.ReportImpl as ReportImpl
import Test.Report as Report
import Test.Groups as Groups
import Test.ParseTree as ParseTree
import Test.MusicApi as MusicApi


main :: Effect Unit
main = do
    -- runAff_ (const $ pure unit) $ MusicApi.fetchArtist Nothing "Queen"

    runSpecAndExitProcess [consoleReporter] do
        DhallDodo.spec
        OrgDodo.spec
        TextDodo.spec
        Report.spec
        ReportImpl.spec
        Groups.spec
        ParseTree.spec
    -- MusicApi.spec