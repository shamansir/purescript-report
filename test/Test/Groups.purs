module Test.Groups where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (wrap)

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (shouldEqual, fail)

import Report.Group as RG


spec :: Spec Unit
spec =
    describe "groups" do
      it "quick-chain" do
        (RG.explode $ RG.cg_ "foo") `shouldEqual` [ "foo" /\ [ wrap "foo" ] ]
        (RG.explode $ RG.cgx_ $ "foo" /\ "bar") `shouldEqual` [ "bar" /\ (wrap <$> [ "foo" ]) ]
        (RG.explode $ RG.cg [ "p1", "p2", "p3" ] "p4") `shouldEqual`
            [ "p1" /\ (wrap <$> [ "p1" ])
            , "p2" /\ (wrap <$> [ "p1", "p2" ])
            , "p3" /\ (wrap <$> [ "p1", "p2", "p3" ])
            , "p4" /\ (wrap <$> [ "p1", "p2", "p3", "p4" ])
            ]
        (RG.explode $ RG.cgx [ "p1" /\ "Name1", "p2" /\ "Name2", "p3" /\ "Name3" ] $ "p4" /\ "Name4") `shouldEqual`
            [ "Name1" /\ (wrap <$> [ "p1" ])
            , "Name2" /\ (wrap <$> [ "p1", "p2" ])
            , "Name3" /\ (wrap <$> [ "p1", "p2", "p3" ])
            , "Name4" /\ (wrap <$> [ "p1", "p2", "p3", "p4" ])
            ]