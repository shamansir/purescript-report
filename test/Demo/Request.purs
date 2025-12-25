module Test.Demo.Request where

import Prelude

import Effect.Aff (Aff)

import Foreign (ForeignError, renderForeignError)

import Data.Either (Either(..), either)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (singleton) as NEL
import Data.Bifunctor (lmap)

import Control.Monad.Except (runExcept)

import Affjax.Node (Error, URL, get, printError) as Aj
import Affjax.ResponseFormat (string) as Aj

import Yoga.JSON (readJSON', class ReadForeign)


json :: forall a. ReadForeign a => Aj.URL -> Aff (E a)
json = map _joinErrors' <<< map (map $ runExcept <<< readJSON' <<< _.body) <<< Aj.get Aj.string
    where -- FIXME: use Aj.ResponceBodyError to wrap ForeignError of Decoding
        _joinErrors :: forall ea eb ec. (ea -> ec) -> (eb -> ec) -> Either ea (Either (NonEmptyList eb) a) -> Either (NonEmptyList ec) a
        _joinErrors atoc btoc = either (Left <<< NEL.singleton <<< atoc) (lmap $ map btoc)
        _joinErrors' :: Either Aj.Error (Either (NonEmptyList ForeignError) a) -> Either MultipleErrors a
        _joinErrors' = _joinErrors RequestError DecodingError


data Error
    = RequestError Aj.Error
    | DecodingError ForeignError -- FIXME: use Aj.ResponceBodyError


type MultipleErrors = NonEmptyList Error


type E a = Either MultipleErrors a


instance Show Error where
    show = printError


printError :: Error -> String
printError = case _ of
    RequestError aje -> Aj.printError aje -- # String.replace (Pattern "\n") (Replacement "<br>")
    DecodingError fe -> renderForeignError fe -- # String.replace (Pattern "\n") (Replacement "<br>")
