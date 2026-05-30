module Data.Yaml.Extra where

import Prelude

import Data.Either (Either(..), either)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Bifunctor (lmap, rmap)
import Data.String (joinWith) as String

import Control.Monad.Except (runExcept)

import Foreign (ForeignError, renderForeignError)

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError) as AG
import Data.YAML.Foreign.Decode (parseYAMLToJson)


type YamlDecodeError = Either (NonEmptyList ForeignError) AG.JsonDecodeError
type YamlDecodeResult a = Either YamlDecodeError a


decodeFromYaml :: forall a. DecodeJson a => String -> YamlDecodeResult a
decodeFromYaml = parseYAMLToJson >>> runExcept  >>> lmap Left >>> flip bind (decodeJson >>> lmap Right)


printYamlDecodeError :: YamlDecodeError -> String
printYamlDecodeError = either (NEL.toUnfoldable >>> map renderForeignError >>> String.joinWith " ; ") AG.printJsonDecodeError
