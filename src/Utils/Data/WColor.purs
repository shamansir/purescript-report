module Utils.Data.WColor where

import Prelude

import Foreign (F)
import Foreign (fail, ForeignError(..)) as F

import Color (Color)
import Color (toHexString, fromHexString, rgb, rgba, hsl, hsla, hsv, hsva) as Color
import Foreign.Object (Object)
import Foreign.Object as Object

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), note)
import Data.String as String

import Control.Alt ((<|>))

import Yoga.JSON (class WriteForeign, writeImpl, class ReadForeign, readImpl, readJSON')
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
-- import Data.Argonaut.Decode.Generic (genericDecodeJson)
-- import Data.Argonaut.Encode.Generic (genericEncodeJson)

import Utils.Data.Color.Named (byName) as Named



newtype WColor = WColor Color

derive instance Newtype WColor _
derive newtype instance Eq WColor


toColor :: WColor -> Color
toColor (WColor c) = c


instance ReadForeign WColor where
    readImpl f = do
        ((readImpl f :: F String) <#> fromText # informIfError)
        <|>
        (fromObject f)
        where
            fromObject f =
                ((readImpl f :: F { r :: Int, g :: Int, b :: Int, a :: Number }) <#> (\{ r, g, b, a } -> Color.rgba r g b a) <#> WColor)
                <|>
                ((readImpl f :: F { h :: Number, s :: Number, l :: Number, a :: Number }) <#> (\{ h, s, l, a } -> Color.hsla h s l a) <#> WColor)
                <|>
                ((readImpl f :: F { h :: Number, s :: Number, v :: Number, a :: Number }) <#> (\{ h, s, v, a } -> Color.hsva h s v a) <#> WColor)
                <|>
                ((readImpl f :: F { r :: Int, g :: Int, b :: Int }) <#> (\{ r, g, b } -> Color.rgb r g b) <#> WColor)
                <|>
                ((readImpl f :: F { h :: Number, s :: Number, l :: Number }) <#> (\{ h, s, l } -> Color.hsl h s l) <#> WColor)
                <|>
                ((readImpl f :: F { h :: Number, s :: Number, v :: Number }) <#> (\{ h, s, v } -> Color.hsv h s v) <#> WColor)
            fromText str = Color.fromHexString str <|> Named.byName (String.toLower str) {- <|> (readJSON' str # ?wh) -}
            informIfError :: F (Maybe Color) -> F WColor
            informIfError f = do
                mbColor <- f
                case mbColor of
                    Just color -> pure $ WColor color
                    Nothing -> F.fail $ F.ForeignError "Failed to parse from text"



instance DecodeJson WColor where
    decodeJson json = WColor <$> case toString json of
        Just str -> note (UnexpectedValue json) $ fromText $ String.trim str
        Nothing -> decodeJson json >>= fromObject
        where
            fromText str = Color.fromHexString str <|> Named.byName (String.toLower str)
            fromObject :: Object Json -> Either JsonDecodeError Color
            fromObject obj = do
                a <- fromMaybe 1.0 <$> obj .:? "a"
                let has k = Object.member k obj
                if has "l" then Color.hsla <$> obj .: "h" <*> obj .: "s" <*> obj .: "l" <*> pure a
                else if has "v" then Color.hsva <$> obj .: "h" <*> obj .: "s" <*> obj .: "v" <*> pure a
                else if has "r" then (\r g b -> Color.rgba r g b a) <$> obj .: "r" <*> obj .: "g" <*> obj .: "b"
                else Left $ TypeMismatch "a hex string, a color name, or { h, s, l } / { h, s, v } / { r, g, b }"


instance EncodeJson WColor where
    encodeJson = toColor >>> Color.toHexString >>> encodeJson


instance WriteForeign WColor where
    writeImpl = toColor >>> Color.toHexString >>> writeImpl


fromHex :: String -> Maybe WColor
fromHex = Color.fromHexString >>> map WColor


toHex :: WColor -> String
toHex = unwrap >>> Color.toHexString
