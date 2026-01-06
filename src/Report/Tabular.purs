module Report.Tabular where

import Prelude

import Data.Int as Int
import Data.Array (snoc, sortWith) as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (joinWith) as String

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol, reflectSymbol)

import Prim.Row as Row
import Prim.RowList as RL

import Record (get) as R
import Record.Extra (class Keys) as Record

import Report.Convert.Keyed

import Yoga.JSON (class ReadForeign, class WriteForeign)

-- import Data.Text.Format (class Formatter)
-- import Data.Text.Format as F


class ToTabularValue a v where
    toTV :: a -> Maybe v


newtype Item v = Item { key :: String, label :: String, value :: v }
derive instance Newtype (Item v) _
derive instance Functor Item
derive newtype instance ReadForeign v => ReadForeign (Item v)
derive newtype instance WriteForeign v => WriteForeign (Item v)


instance Keyed String (Item v) where
    keyOf (Item item) = item.key


newtype Tabular v = Tabular (Array (Item v))
derive instance Functor Tabular
derive newtype instance ReadForeign v => ReadForeign (Tabular v)
derive newtype instance WriteForeign v => WriteForeign (Tabular v)


class ToTabular v x where
    toTabular :: x -> Tabular v


class TabularRow :: RL.RowList Type -> Row Type -> Type -> Constraint
class TabularRow rl row v | rl -> row where
    toTabularBase :: Proxy v -> Proxy rl -> Record row -> Tabular v -> Tabular v


instance tabularRowNil :: TabularRow RL.Nil row v where
    toTabularBase _ _ _ _ = empty
else instance fromValuesInChannelRowBaseCons ::
  ( IsSymbol name
  , ToTabularValue a v
  , Row.Cons name a trash row
  , TabularRow tail row v
  ) => TabularRow (RL.Cons name a tail) row v where
    toTabularBase pv _ rec prev =
      case toTV value of
        Just val -> insert (reflectSymbol nameP) val rest
        Nothing -> rest
      where
        nameP = Proxy :: _ name
        value = R.get nameP rec
        rest = toTabularBase pv (Proxy :: _ tail) rec prev


instance ToTabularValue String String where toTV = Just <<< identity
instance ToTabularValue Int String where toTV = Just <<< Int.toStringAs Int.decimal
instance ToTabularValue Number String where toTV = Just <<< show


empty :: forall v. Tabular v
empty = Tabular []


singleton :: forall v. Item v -> Tabular v
singleton = pure >>> Tabular


items :: forall v. Tabular v -> Array (Item v)
items (Tabular theItems) = theItems


insert :: forall v. String -> v -> Tabular v -> Tabular v
insert s v (Tabular vs) = Tabular $ Array.snoc vs $ wrap { key : s, label : s, value : v }


fromRec :: forall rl row v. RL.RowToList row rl => Record.Keys rl => TabularRow rl row v => Record row -> Tabular v
fromRec record = toTabularBase (Proxy :: _ v) (Proxy :: _ rl) record empty


-- reorder :: forall v. (String -> Int) -> Tabular v -> Tabular v
-- reorder f (Tabular vs) = Tabular $ Array.sortWith (Tuple.fst >>> _.key >>> f) vs


ordered :: forall @o @x v. TabularOrder o v x => Tabular v -> Tabular v
ordered (Tabular vs) = Tabular $ Array.sortWith (orderFor @o @_ @x) vs


class Ord o <= TabularOrder o v x where
    orderFor :: Item v -> o


label :: forall v. (String -> String) -> Tabular v -> Tabular v
label f (Tabular vs) = Tabular $ wrap <$> labelFor <$> unwrap <$> vs
    where labelFor r = r { label = f r.key }


toString :: forall v. String -> (String -> v -> String) -> Tabular v -> String
toString sep f (Tabular vs) = String.joinWith sep $ (\{ label, value } -> f label value) <$> unwrap <$> vs

{-
instance Formatter (Tabular String) where
    format :: Tabular String -> F.Tag
    format (Tabular vs) =
        F.joinWith (F.nl <> F.nl) $ (\{ label, value } -> F.bold (F.s label) <> F.s " : " <> F.s value) <$> vs
        -- F.dl $ uncurry (\prop val -> F.dt (F.s prop) (F.s val)) <$> vs
else instance Formatter v => Formatter (Tabular v) where
    format :: Tabular v -> F.Tag
    format (Tabular vs) =
        F.joinWith (F.nl <> F.nl) $ (\{ label, value } -> F.bold (F.s label) <> F.s " : " <> F.format value) <$> vs
        -- F.dl $ uncurry (\prop val -> F.dt (F.s prop) (F.format val)) <$> vs
-}