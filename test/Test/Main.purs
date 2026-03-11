module Test.Main where

import Prelude

import Effect (Effect)

import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.DhallDodo as DhallDodo
import Test.OrgDodo as OrgDodo
import Test.TextDodo as TextDodo
import Test.ReportImpl as ReportImpl
import Test.Report as Report
import Test.Groups as Groups
import Test.ParseTree as ParseTree


main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    DhallDodo.spec
    OrgDodo.spec
    TextDodo.spec
    Report.spec
    ReportImpl.spec
    Groups.spec
    ParseTree.spec