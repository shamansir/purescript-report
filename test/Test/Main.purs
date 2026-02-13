module Test.Main where

import Prelude

import Effect (Effect)

import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.DhallDodo as DhallDodo
import Test.OrgDodo as OrgDodo
import Test.ReportImpl as ReportImpl
import Test.Report as Report
import Test.Groups as Groups


main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    DhallDodo.spec
    OrgDodo.spec
    Report.spec
    ReportImpl.spec
    Groups.spec