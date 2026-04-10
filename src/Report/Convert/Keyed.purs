module Report.Convert.Keyed where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (F, Foreign)
import Foreign (fail, ForeignError(..)) as F

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl) as Y


class Keyed k v where
    keyOf :: v -> k


class EncodableKey k where -- FIXME: same as ConvertTo String k / ConvertFrom String k
    encodeKey :: k -> String
    decodeKey :: String -> Maybe k
    -- default :: k


instance EncodableKey String where
    encodeKey = identity
    decodeKey = Just



class KeyedReadForeign k v where
    keyedReadImpl :: k -> Foreign -> F v


class KeyedWriteForeign k v where
    keyedWriteImpl :: k -> v -> Foreign


newtype KeyedValue k v = KV { mod_key :: k, mod_v :: v }
derive instance Functor (KeyedValue k)

-- newtype KJ v = KJ (KeyedValue String v)
-- derive instance Functor KJ

-- derive newtype instance (ReadForeign k,  ReadForeign v)  => ReadForeign  (KeyedValue k v)
-- derive newtype instance (WriteForeign k, WriteForeign v) => WriteForeign (KeyedValue k v)


-- derive newtype instance ReadForeign JsonTM
-- derive newtype instance WriteForeign JsonTM


key :: forall k v. KeyedValue k v -> k
key (KV { mod_key }) = mod_key


value :: forall k v. KeyedValue k v -> v
value (KV { mod_v }) = mod_v


make :: forall k v. k -> v -> KeyedValue k v
make k v = KV { mod_key: k, mod_v: v }


mark :: forall k v. Keyed k v => v -> KeyedValue k v
mark v = KV { mod_key : keyOf @k v, mod_v : v }


rekey :: forall k k' v. (k -> k') -> KeyedValue k v -> KeyedValue k' v
rekey f (KV { mod_key, mod_v }) = KV { mod_key: f mod_key, mod_v }


toJson :: forall @k @v. EncodableKey k => KeyedWriteForeign k v => KeyedValue k v -> KeyedValue String Foreign
toJson kv = keyedWriteImpl (key kv) <$> rekey encodeKey kv


toJson' :: forall @k @v. EncodableKey k => Y.WriteForeign v => KeyedValue k v -> KeyedValue String Foreign
toJson' kv = Y.writeImpl <$> rekey encodeKey kv


writeImpl :: forall @k @v. EncodableKey k => KeyedWriteForeign k v => KeyedValue k v -> Foreign
writeImpl kv = toJson kv # \(KV rec) -> Y.writeImpl rec


writeImpl' :: forall @k @v. EncodableKey k => Y.WriteForeign v => KeyedValue k v -> Foreign
writeImpl' kv = toJson' kv # \(KV rec) -> Y.writeImpl rec


readImpl :: forall @k @v. EncodableKey k => KeyedReadForeign k v => Foreign -> F (KeyedValue k v)
readImpl frgn = do
    ({ mod_key, mod_v } :: { mod_key :: String, mod_v :: Foreign }) <- Y.readImpl frgn
    case decodeKey mod_key of
        Just decoded_key -> do
            decoded_v <- keyedReadImpl decoded_key mod_v
            pure $ KV { mod_key : decoded_key, mod_v: decoded_v }
        Nothing -> F.fail $ F.ForeignError $ "Failed to decode key: " <> mod_key


readImpl' :: forall @k @v. EncodableKey k => Y.ReadForeign v => Foreign -> F (KeyedValue k v)
readImpl' frgn = do
    ({ mod_key, mod_v } :: { mod_key :: String, mod_v :: Foreign }) <- Y.readImpl frgn
    case decodeKey mod_key of
        Just decoded_key -> do
            decoded_v <- Y.readImpl mod_v
            pure $ KV { mod_key : decoded_key, mod_v: decoded_v }
        Nothing -> F.fail $ F.ForeignError $ "Failed to decode key: " <> mod_key



{-
toJson :: forall @k @v. EncodableKey k => KeyedValue k v -> KJ v
toJson = KJ <<< rekey encodeKey


toJson' :: forall @k @v. EncodableKey k => WriteForeign v => KeyedValue k v -> KJ Foreign
toJson' = KJ <<< (rekey encodeKey <#> map writeImpl)


encodeKeyed :: forall @k v. EncodableKey k => Keyed k v => v -> KJ v
encodeKeyed v = KJ $ TM { mod_key: encodeKey @k (keyOf v), mod_v : v }


encodeKeyed' :: forall @k @v. EncodableKey k => WriteForeign v => Keyed k v => v -> KJ Foreign
encodeKeyed' v = encodeKeyed @k v <#> writeImpl


defaultReadImpl_ :: forall @k. EncodableKey k => Foreign -> F k
defaultReadImpl_ frgn = readImpl frgn >>= \str ->
    case decodeKey @k str of
        Just key -> pure key
        Nothing  -> F.fail $ F.ForeignError $ "Failed to decode modifier key: " <> str


defaultWriteImpl :: forall @k @v. EncodableKey k => DecodeKeyed k v => Keyed k v => WriteForeign v => v -> Foreign
defaultWriteImpl v = encodeKeyed @k v <#> writeImpl # (\(KJ kj) -> kj) # writeImpl


decodeKeyed :: forall @k @v. EncodableKey k => DecodeKeyed k v => KJ Foreign -> F v
decodeKeyed (KJ (TM { mod_key, mod_v })) = do
    case decodeKey @k mod_key of
        Just key -> toValue key mod_v
        Nothing  -> F.fail $ F.ForeignError $ "Failed to decode modifier key: " <> mod_key


-- decodeKeyed' :: forall @k @v. EncodableKey k => DecodeKeyed k v => ReadForeign v => KJ v -> F v
-- decodeKeyed' (KJ (TM { mod_key, mod_v })) = do
--     case decodeKey @k mod_key of
--         Just key -> toValue key mod_v
--         Nothing  -> F.fail $ F.ForeignError $ "Failed to decode modifier key: " <> mod_key

-}

