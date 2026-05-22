module Report.Impl.Q.Subject where

import Prelude

import Data.Newtype (wrap, unwrap)
import Data.Tuple.Nested ((/\), type (/\))

import Report.Impl.Subject
import Report.Tabular as Tabular
import Report.Decorators.Tabular.TabularValue
import Report.Decorators.Stats


mk :: forall subj_id subj_tag. subj_id -> String -> Subject subj_id subj_tag
mk = init


tags :: forall subj_id subj_tag. Array subj_tag -> Subject subj_id subj_tag -> Subject subj_id subj_tag
tags nextTags = unwrap >>> _ { tags = nextTags } >>> wrap


tabs :: forall subj_id subj_tag. Array (String /\ TabularValue) -> Subject subj_id subj_tag -> Subject subj_id subj_tag
tabs nextTabs = unwrap >>> _ { tabular = Tabular.fromArray nextTabs } >>> wrap


stats :: forall subj_id subj_tag. Stats -> Subject subj_id subj_tag -> Subject subj_id subj_tag
stats nextStats = unwrap >>> _ { stats = nextStats } >>> wrap