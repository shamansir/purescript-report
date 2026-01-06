module Report.Convert.Keyed where

import Prelude

import Foreign (F, Foreign)
import Foreign (fail, ForeignError(..)) as F

import Data.Maybe (Maybe(..))

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)


class Keyed k v where
    keyOf :: v -> k


class EncodableKey k where
    encodeKey :: k -> String
    decodeKey :: String -> Maybe k


class DecodeKeyed k v where
    toValue :: k -> Foreign -> F v


newtype KeyedValue k v = TM { mod_key :: k, mod_v :: v }

type JsonTM = KeyedValue String Foreign

derive newtype instance (ReadForeign k,  ReadForeign v)  => ReadForeign  (KeyedValue k v)
derive newtype instance (WriteForeign k, WriteForeign v) => WriteForeign (KeyedValue k v)


mark :: forall k v. Keyed k v => v -> KeyedValue k v
mark v = TM { mod_key : keyOf v, mod_v : v }


encodeKeyed :: forall @k v. EncodableKey k => WriteForeign v => Keyed k v => v -> JsonTM
encodeKeyed v = TM { mod_key: encodeKey @k (keyOf v), mod_v: writeImpl v }


decodeKeyed :: forall @k @v. EncodableKey k => DecodeKeyed k v => ReadForeign v => JsonTM -> F v
decodeKeyed (TM { mod_key, mod_v }) = do
    case decodeKey @k mod_key of
        Just key -> toValue key mod_v
        Nothing  -> F.fail $ F.ForeignError $ "Failed to decode modifier key: " <> mod_key



