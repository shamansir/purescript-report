module Report.Convert.Tagged where

import Prelude

import Foreign (F, Foreign)
import Foreign (fail, ForeignError(..)) as F

import Data.Maybe (Maybe(..))

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

import Report.Modifiers (class IsModifier, modifierKey)


class EncodableKey k where
    encodeKey :: k -> String
    decodeKey :: String -> Maybe k


class DecodeTagged k v where
    toValue :: k -> Foreign -> F v


newtype TaggedModifier k v = TM { mod_key :: k, mod_v :: v }

type JsonTM = TaggedModifier String Foreign

derive newtype instance (ReadForeign k,  ReadForeign v)  => ReadForeign  (TaggedModifier k v)
derive newtype instance (WriteForeign k, WriteForeign v) => WriteForeign (TaggedModifier k v)


tag :: forall k v. IsModifier k v => v -> TaggedModifier k v
tag v = TM { mod_key : modifierKey v, mod_v : v }


encodeTagged :: forall @k v. EncodableKey k => WriteForeign v => IsModifier k v => v -> JsonTM
encodeTagged v = TM { mod_key: encodeKey @k (modifierKey v), mod_v: writeImpl v }


decodeTagged :: forall @k @v. EncodableKey k => DecodeTagged k v => ReadForeign v => JsonTM -> F v
decodeTagged (TM { mod_key, mod_v }) = do
    case decodeKey @k mod_key of
        Just key -> toValue key mod_v
        Nothing  -> F.fail $ F.ForeignError $ "Failed to decode modifier key: " <> mod_key



