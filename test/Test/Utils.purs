module Test.Utils where

import Prelude

import Effect.Exception (Error)
import Effect.Class (class MonadEffect)

import Control.Monad.Error.Class (class MonadThrow)

import Data.FoldableWithIndex (foldlWithIndex, class FoldableWithIndex)
import Data.Text.Diff (Comparator(..), Limit(..)) as Diff
import Data.Text.Diff.Effectful (compareByWP) as Diff


shouldEqual :: forall m. MonadEffect m ⇒ MonadThrow Error m ⇒ String -> String -> m Unit
shouldEqual = Diff.compareByWP (Diff.OnlyDifferent $ Diff.NoLimit)