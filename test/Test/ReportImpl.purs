module Test.ReportImpl where

import Prelude

import Effect (Effect)
import Foreign (fail, ForeignError(..))

import Data.Maybe (Maybe(..))

import Type.Proxy (Proxy(..))

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (shouldEqual)

import Halogen as H
import Halogen.HTML as HH

import Yoga.JSON (class WriteForeign, class ReadForeign, writeImpl, readImpl)

import Report (Report)
import Report.Class
import Report as Report

import Report.Chain (Chain(..))
import Report.Impl.Group (Group)
import Report.Impl.Item (Item)
import Report.Impl.Subject (Subject)
import Report.Impl.Tag (altDefaultColors) as Tag
import Report.Convert.Generic (class ToExport) as Report
import Report.Web.Component as Report
import Report.Web.Component (defaultConfig) as RepComponent




type SubjectId = String
data SubjectTag = SubjectTag
data ItemTag = ItemTag


derive instance Eq SubjectTag
derive instance Eq ItemTag
derive instance Ord ItemTag


instance LimitedSet SubjectTag where
    values = [ SubjectTag ]

instance LimitedSet ItemTag where
    values = [ ItemTag ]


instance IsTag SubjectTag where
    tagColors _ = Tag.altDefaultColors
    tagContent _ = End "subj"


instance ConvertTo (Chain String) SubjectTag where
    convertTo = mkChainEncode $ const "subj"
    -- encodeTag = const "subj"
    -- decodeTag = const $ Just SubjectTag
    -- allTags = [ SubjectTag ]


instance ConvertFrom (Chain String) SubjectTag where
    convertFrom = mkChainDecode (const $ Just SubjectTag)


instance ConvertTo (Chain String) ItemTag where
    convertTo = mkChainEncode $ const "item"


instance ConvertFrom (Chain String) ItemTag where
    convertFrom = mkChainDecode (const $ Just ItemTag)


instance IsTag ItemTag where
    tagColors _ = Tag.altDefaultColors
    tagContent _ = End "item"


instance IsSortable Unit ItemTag where
    kindOf = const unit


instance IsGroupable Group ItemTag where
    t_group ItemTag = Nothing


instance WriteForeign SubjectTag where
    writeImpl = defaultWriteImpl
instance WriteForeign ItemTag where
    writeImpl = defaultWriteImpl
instance ReadForeign SubjectTag where
    readImpl = defaultReadImpl
instance ReadForeign ItemTag where
    readImpl = defaultReadImpl



type MySubject = Subject SubjectId SubjectTag
type MyGroup = Group
type MyItem = Item ItemTag


type MyReportT = Report MySubject MyGroup MyItem
newtype MyReport = MyReport MyReportT


derive newtype instance Report.Is SubjectId SubjectTag Unit ItemTag MySubject MyGroup MyItem MyReport
derive newtype instance Report.Has SubjectTag ItemTag MySubject MyGroup MyItem MyReport
derive newtype instance Report.Modify ItemTag MyGroup MyItem MyReport
derive newtype instance Report.ToReport MySubject MyGroup MyItem MyReport
instance Report.ToExport SubjectId SubjectTag ItemTag MySubject MyGroup MyItem MyReport


_report  = Proxy :: _ "report"


spec :: Spec Unit
spec =
    describe "use" do
      it "report impl" do
        let myReport :: MyReport
            myReport = MyReport Report.empty
            component :: H.Component _ MyReport _ Effect
            component = Report.component @MyReport @String @SubjectTag @Unit @ItemTag @MySubject @MyGroup @MyItem RepComponent.defaultConfig
            slot = HH.slot_ _report unit component myReport
        true `shouldEqual` true