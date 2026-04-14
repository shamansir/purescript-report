module Control.Applicative.Extra where

import Prelude


import Data.Maybe (Maybe, maybe)
-- import Data.Foldable (for_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))


whenJust :: forall m a. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
whenJust = whenJust' unit


whenJust' :: forall m a x. Applicative m => x -> Maybe a -> (a -> m x) -> m x
whenJust' x mg f = maybe (pure x) f mg


whenJust2 :: forall m a b. Applicative m => Maybe a -> Maybe b -> (a -> b -> m Unit) -> m Unit
whenJust2 = whenJust2' unit


whenJust2' :: forall m a b x. Applicative m => x -> Maybe a -> Maybe b -> (a -> b -> m x) -> m x
whenJust2' x ma mb = whenJust' x ((/\) <$> ma <*> mb) <<< uncurry


whenJust_ :: forall m a. Applicative m => (a -> m Unit) -> Maybe a -> m Unit
whenJust_ = flip whenJust


whenJust'_ :: forall m a x. Applicative m => x -> (a -> m x) -> Maybe a -> m x
whenJust'_ = flip <<< whenJust'


whenJustE :: forall m a x. Applicative m => (Unit -> m x) -> Maybe a -> (a -> m x) -> m x
whenJustE onerr mg f = maybe (onerr unit) f mg