module Report.Web.Helpers.InlineOrBlock where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Web.Helpers (H)


data InlineOrBlock w i
    = Inline (H w i)
    | Block (H w i) (Array (H w i))
    | OnlyBlock (Array (H w i))
    | Skip


mapInlineContent :: forall w i. (H w i -> H w i) -> InlineOrBlock w i -> InlineOrBlock w i
mapInlineContent f = case _ of
    Inline inlineContent -> Inline $ f inlineContent
    Block inlineContent blockContent -> Block (f inlineContent) blockContent
    OnlyBlock blockContent -> OnlyBlock blockContent
    Skip -> Skip



mapBlockContent :: forall w i. (Array (H w i) -> Array (H w i)) -> InlineOrBlock w i -> InlineOrBlock w i
mapBlockContent f = case _ of
    Inline inlineContent -> Inline inlineContent
    Block inlineContent blockContent -> Block inlineContent $ f blockContent
    OnlyBlock blockContent -> OnlyBlock $ f blockContent
    Skip -> Skip


mapHtml :: forall w i. (H w i -> H w i) -> InlineOrBlock w i -> InlineOrBlock w i
mapHtml f = mapInlineContent f >>> mapBlockContent (map f)


loadInlineContent :: forall w i. InlineOrBlock w i -> Maybe (H w i)
loadInlineContent = case _ of
    Inline inlineContent -> Just inlineContent
    Block inlineContent _ -> Just $ inlineContent
    OnlyBlock _ -> Nothing
    Skip -> Nothing


loadBlockContent :: forall w i. InlineOrBlock w i -> Maybe (Array (H w i))
loadBlockContent = case _ of
    Inline _ -> Nothing
    Block _ blockContent -> Just blockContent
    OnlyBlock blockContent -> Just blockContent
    Skip -> Nothing


-- type RenderedDecorator w i =
--     { key :: Decorator.Key
--     , rendered :: InlineOrBlock w i
--     }
    -- , affectsTitle :: Maybe Progress