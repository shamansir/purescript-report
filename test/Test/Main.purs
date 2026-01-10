module Test.Main where

import Prelude

import Effect (Effect)

import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.DhallDodo as DhallDodo
import Test.ReportImpl as ReportImpl


main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    DhallDodo.spec
    ReportImpl.spec