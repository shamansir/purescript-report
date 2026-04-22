module Report.Modify where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:))
import Data.Array (index, catMaybes, snoc, updateAt, concat, any, filter, concat) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, build, node, leaf) as Tree

import Report (Report)
import Report.Builder (TreeNode(..))
import Report.Builder as RBuilder
import Report.Builder.Navigate as Nav
import Report.Builder.InlinePos as Nav
import Report (toBuilder, fromBuilder, withGroup, withItem, toTree) as Report
import Report.Class
import Report.Chain (Chain)
import Report.Chain as Chain
import Report.Core.Logic (EncodedValue(..))
import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep, startsWithNotEq, pathFromArray, startsWith, encode) as GPath
import Report.Decorator as Decorator
import Report.Decorator (Key(..)) as Dec
import Report.Decorator (Decorator, Decorators)
import Report.Decorators.Stats (Stats)
import Report.Decorators.Tags (Tags, RawTag)
import Report.Decorators.Tags (fromArray) as Tags
import Report.Decorators.Class.ValueModify (fromEditable)
import Report.Decorators.Stats.Collect (collectStats, CollectWhat)
import Report.Decorators.Progress as P
import Report.Convert.Text.Decorators.Tags (decodeTags) as Tags


data What
    = Subj
    | GroupName
    -- | GroupStat -- TODO
    | ItemName Int
    | ItemDecorator Int Decorator.Key
    | ItemTag Int Int
    | ItemTabular Int Int
    -- | AddDecorator -- TODO
    -- | AddItem -- TODO
    -- | AddGroup -- TODO


data WhatKey
    = WKSubj
    | WKGroupName
    -- | WKGroupStat -- TODO
    | WKItemName
    | WKItemDecorator
    | WKItemTags
    | WKItemTabular
    -- | WKAddDecorator -- TODO
    -- | WKAddItem -- TODO
    -- | WKAddGroup -- TODO


derive instance Eq WhatKey


data Location subj_id
    = Nowhere
    | AtSubj      subj_id
    | AtGroup     subj_id GroupPath
    | AtItem      subj_id GroupPath Int
    | AtDecorator subj_id GroupPath Int Decorator.Key
    | AtTag       subj_id GroupPath Int Int
    | AtTabular   subj_id GroupPath Int Int


derive instance Eq subj_id => Eq (Location subj_id)


type Modification subj_id =
    { subjId :: subj_id
    , path :: GroupPath
    , what :: What
    , newValue :: EncodedValue
    }


class GroupModify group where
    setGroupName :: String -> group -> group


class StatsModify a where
    setStats :: Stats -> a -> a


class DecoratorsModify a where
    updateDecorators :: Decorators -> a -> a


class TagsModify t a where
    updateTags :: Tags t -> a -> a


class ItemModify a where
    setItemName :: String -> a -> a


modifyAt
    :: forall @item_tag subj_id subj group item
     . Eq subj_id
    => IsSubjectId subj_id subj
    => ConvertFrom (Chain String) item_tag
    => IsGroup group
    => HasDecorators item
    => HasTags item_tag item
    => GroupModify group
    => ItemModify item
    => TagsModify item_tag item
    => DecoratorsModify item
    => Modification subj_id
    -> Report subj group item
    -> Report subj group item
modifyAt { subjId, what, newValue, path } report = case what of
    Subj ->
        report
    GroupName -> do
        Report.withGroup subjId path (setGroupName $ unwrapEditable newValue) report
    -- GroupStat -> do
    --     Report.withGroup subj path (\group -> setGroupStats (groupStatsFromEditable newValue group) group) report
    ItemName itemIdx -> do
        Report.withItem subjId path itemIdx (setItemName $ unwrapEditable newValue) report
    ItemDecorator itemIdx deckey -> do
        Report.withItem subjId path itemIdx (setDecorator deckey) report
    ItemTag itemIdx tagIdx ->
        Report.withItem subjId path itemIdx setTags report
    ItemTabular itemIdx tabularIdx ->
        report -- FIXME: Implement
    where
        -- setDecorator :: Decorator.Key -> item -> item
        -- setDecorator pkey item =
        --     let
        --         (prefixes :: Prefixes) = i_prefixes item
        --         (mbDecodedValue :: Maybe Prefix) = fromEditable pkey newValue
        --         nextPrefixes = fromMaybe prefixes $ (\pfx -> Prefix.put pfx prefixes) <$> mbDecodedValue
        --     in updatePrefixes nextPrefixes item
        setDecorator :: Decorator.Key -> item -> item
        setDecorator skey item =
            let
                (decorators :: Decorators) = i_decorators item
                (mbDecodedValue :: Maybe Decorator) = fromEditable skey newValue
                nextDecorators = fromMaybe decorators $ (\dec -> Decorator.put dec decorators) <$> mbDecodedValue
            in updateDecorators nextDecorators item
        setTags :: item -> item
        setTags item =
            let
                -- (currentTags :: Tags item_tag) = Tags.fromArray $ i_tags item
                -- (mbDecodedValue :: Maybe (Tags RawTag)) = fromEditable unit newValue
                (nextTags :: Tags item_tag) = Tags.decodeTags newValue
                -- nextTags = fromMaybe currentTags $ (\tags -> ?wh <$> tags) <$> mbDecodedValue
            in updateTags nextTags item
        unwrapEditable (EncodedValue string) = string
        -- groupStatsFromEditable :: Editable -> group -> Stats
        -- groupStatsFromEditable editable group = fromMaybe (g_stats group) $ fromEditable editable


data RecalculateInclude
    = OnlyDirect
    | AllNested


whatOfLoc :: forall subj_id. Location subj_id -> Maybe What
whatOfLoc = case _ of
    Nowhere -> Nothing
    AtSubj _ -> Just Subj
    AtGroup _ _ -> Just GroupName
    AtItem _ _ itemIdx -> Just $ ItemName itemIdx
    AtDecorator _ _ itemIdx decKey -> Just $ ItemDecorator itemIdx decKey
    AtTag _ _ itemIdx tagIdx -> Just $ ItemTag itemIdx tagIdx
    AtTabular _ _ itemIdx tabularIdx -> Just $ ItemTabular itemIdx tabularIdx


whatKeyOf :: What -> WhatKey
whatKeyOf = case _ of
    Subj -> WKSubj
    GroupName -> WKGroupName
    -- GroupStat -> WKGroupStat
    ItemName _ -> WKItemName
    ItemDecorator _ _ -> WKItemDecorator
    ItemTag _ _ -> WKItemTags
    ItemTabular _ _ -> WKItemTabular
    -- AddDecorator -> WKAddDecorator
    -- AddItem -> WKAddItem
    -- AddGroup -> WKAddGroup


whatKeyOfLoc :: forall subj_id. Location subj_id -> Maybe WhatKey
whatKeyOfLoc location = case location of
    Nowhere -> Nothing
    AtSubj _ -> Just WKSubj
    AtGroup _ _ -> Just WKGroupName
    AtItem _ _ _ -> Just WKItemName
    AtDecorator _ _ _ _ -> Just WKItemDecorator
    AtTag _ _ _ _ -> Just WKItemTags
    AtTabular _ _ _ _ -> Just WKItemTabular


{-
recalculate
    :: forall @tag subj group item
     . IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => Report subj group item
    -> Report subj group item
recalculate =
    recalculate_ @tag AllNested ItemsProgress


recalculateDirect
    :: forall @tag subj group item
     . IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => Report subj group item
    -> Report subj group item
recalculateDirect =
    recalculate_ @tag OnlyDirect ItemsProgress
-}


type RecalculateConfig =
    { include :: RecalculateInclude
    , collect :: CollectWhat
    }


{-
recalculateAlt
    :: forall @tag subj group item
     . IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => RecalculateConfig
    -> Report subj group item
    -> Report subj group item
recalculateAlt cfg =
    Report.toBuilder >>> RBuilder.unfoldC >>> (map $ map updateGroups) >>> RBuilder.toBuilderC >>> Report.fromBuilder   -- FIXME: TODO!
    where
        belongsTo :: Chain group -> Chain group -> Boolean
        belongsTo grpCA grpCB = Debug.spy "startsWith" $ GPath.startsWith (g_path $ Chain.last $ spyGroupC "grpCA" $ grpCA) (g_path $ Chain.last $ spyGroupC "grpCB" $ grpCB)
        collectAllItems :: Chain group -> Array (Chain group /\ Array item) -> Array item
        collectAllItems grpC = Array.filter (Tuple.fst >>> belongsTo grpC) >>> map Tuple.snd >>> Array.concat
        spyGroup :: String -> group -> group
        spyGroup label = Debug.spyWith label (g_path >>> GPath.encode)
        spyGroupC :: String -> Chain group -> Chain group
        spyGroupC label = Debug.spyWith label (map (g_path >>> GPath.encode) >>> Chain.toString)
        updateGroup :: Array item -> group -> group
        updateGroup itemsCollected group = setStats (collectStats @tag cfg.collect itemsCollected # Debug.spy "stats") $ spyGroup "group" group
        updateGroups :: Array (Chain group /\ Array item) -> Array (Chain group /\ Array item)
        updateGroups groupsArr =
            groupsArr <#> \(groupC /\ items) ->
                case cfg.include of
                    AllNested ->
                        updateGroup (collectAllItems groupC groupsArr) <$> groupC
                    OnlyDirect ->
                        updateGroup items <$> groupC
                /\ items
-}

recalculate
    :: forall subj group item
     . Ord group
    => IsGroup group
    => HasDecorators item
    => StatsModify group
    => RecalculateConfig
    -> Report subj group item
    -> Report subj group item
recalculate cfg =
    -- FIXME: check if group chains properly go back!
    Report.toBuilder >>> RBuilder.unfoldAll >>> (map $ map updateGroups) >>> RBuilder.toBuilderG >>> Report.fromBuilder
    where
        belongsTo :: group -> group -> Boolean
        belongsTo grpA grpB = GPath.startsWith (g_path grpA) (g_path grpB)
        collectAllItems :: group -> Array (group /\ Array item) -> Array item
        collectAllItems grp = Array.filter (Tuple.fst >>> belongsTo grp) >>> map Tuple.snd >>> Array.concat
        updateGroup :: Array item -> group -> group
        updateGroup itemsCollected group = setStats (collectStats cfg.collect itemsCollected) group
        updateGroups :: Array (group /\ Array item) -> Array (group /\ Array item)
        updateGroups groupsArr =
            groupsArr <#> \(group /\ items) ->
                case cfg.include of
                    AllNested ->
                        updateGroup (collectAllItems group groupsArr) group
                    OnlyDirect ->
                        updateGroup items group
                /\ items


-- loadDecoratorKey :: What -> Maybe Decorator.Key
-- loadDecoratorKey = case _ of
--     ItemDecorator _ deckey -> Just deckey
--     _ -> Nothing


data Direction
    = Up
    | Down
    | Right
    | Left


move
    :: forall subj_id @item_tag subj group item
     . Eq subj_id
    => IsSubjectId subj_id subj
    => IsGroup group
    => HasDecorators item
    => HasTags item_tag item
    => HasTabular item
    => Report subj group item
    -> Location subj_id
    -> Direction
    -> Location subj_id
move report loc dir =
    let
        builder = Report.toBuilder report
    in case dir of

        Up ->

            case loc of
                Nowhere -> Nowhere
                AtSubj subjId ->
                    -- previous subj or stay there if there are no subjects before
                    case Nav.previousSubject subjId builder of
                        Just prevSubjId -> AtSubj prevSubjId
                        Nothing -> AtSubj subjId
                AtGroup subjId groupPath ->
                    -- previous group or parent subj if there are no groups before
                    case Nav.previousGroup subjId groupPath builder of
                        Just nextGroupPath -> AtGroup subjId nextGroupPath
                        Nothing -> AtSubj subjId
                AtItem subjId groupPath itemIdx ->
                    -- last tabular of the previous item or previous item or parent group
                    case Nav.previousItem subjId groupPath itemIdx builder of
                        Just prevItemIdx ->
                            case Nav.lastTabularInItem subjId groupPath prevItemIdx builder of
                                Just lastTabIdx -> AtTabular subjId groupPath prevItemIdx lastTabIdx
                                Nothing -> AtItem subjId groupPath itemIdx
                        Nothing ->
                            AtGroup subjId groupPath
                AtDecorator subjId groupPath itemIdx decKey ->
                    -- previous item inside this group or previous group if there are no items before this one in this group
                    -- ...or the last decorator inside the previous item
                    case Nav.previousItem subjId groupPath itemIdx builder of
                        Just prevItemIdx ->
                            AtItem subjId groupPath itemIdx
                        Nothing ->
                            AtGroup subjId groupPath
                AtTag subjId groupPath itemIdx tagIndex ->
                    -- previous item inside this group or next group if there are no items before this one in this group
                    -- ...or the first tag inside the previous item
                    case Nav.previousItem subjId groupPath itemIdx builder of
                        Just prevItemIdx ->
                            AtItem subjId groupPath itemIdx
                        Nothing ->
                            AtGroup subjId groupPath
                AtTabular subjId groupPath itemIdx tabularIdx ->
                    -- ...previous tabular inside this item or parent item
                    case Nav.previousTabular subjId groupPath itemIdx tabularIdx builder of
                        Just prevTabularIdx ->
                            AtTabular subjId groupPath itemIdx prevTabularIdx
                        Nothing ->
                            AtItem subjId groupPath itemIdx

        Down ->

            case loc of
                Nowhere ->
                    -- first subj in the report
                    case Nav.firstSubject builder of
                        Just firstSubjId -> AtSubj firstSubjId
                        Nothing -> Nowhere
                AtSubj subjId ->
                    -- first group inside this subj or next subj if there are no groups in this subj
                    case Nav.firstGroupInSubj subjId builder of
                        Just fistGroupPath -> AtGroup subjId fistGroupPath
                        Nothing ->
                            case Nav.nextSubject subjId builder of
                                Just nextSubjId -> AtSubj nextSubjId
                                Nothing -> AtSubj subjId
                AtGroup subjId groupPath ->
                     -- first item inside this group or next group if there are no items in this group
                    case Nav.firstItemInGroup subjId groupPath builder of
                        Just firstItemIdx -> AtItem subjId groupPath firstItemIdx
                        Nothing -> case Nav.nextGroup subjId groupPath builder of
                            Just nextGroupPath -> AtGroup subjId nextGroupPath
                            Nothing -> AtGroup subjId groupPath
                AtItem subjId groupPath itemIdx ->
                    -- first tabular, if there are tabular values inside this item, else
                    -- next item inside this group or next group if there are no items after this one in this group
                    case Nav.firstTabularInItem subjId groupPath itemIdx builder of
                        Just firstTabularIdx -> AtTabular subjId groupPath itemIdx firstTabularIdx
                        Nothing ->
                            case Nav.nextItem subjId groupPath itemIdx builder of
                                Just nextItemIdx -> AtItem subjId groupPath nextItemIdx
                                Nothing -> case Nav.nextGroup subjId groupPath builder of
                                    Just nextGroupPath -> AtGroup subjId nextGroupPath
                                    Nothing -> case Nav.nextSubject subjId builder of
                                        Just nextSubjId -> AtSubj nextSubjId
                                        Nothing -> AtItem subjId groupPath itemIdx
                AtDecorator subjId groupPath itemIdx decKey ->
                    -- next item inside this group or next group if there are no items after this one in this group
                    -- ...or the first decorator inside the next item
                    case Nav.firstTabularInItem subjId groupPath itemIdx builder of
                        Just fistTabularIdx -> AtTabular subjId groupPath itemIdx fistTabularIdx
                        Nothing ->
                            case Nav.nextItem subjId groupPath itemIdx builder of
                                Just nextItemIdx -> AtItem subjId groupPath nextItemIdx
                                Nothing -> case Nav.nextGroup subjId groupPath builder of
                                    Just nextGroupPath -> AtGroup subjId nextGroupPath
                                    Nothing -> case Nav.nextSubject subjId builder of
                                        Just nextSubjId -> AtSubj nextSubjId
                                        Nothing -> AtDecorator subjId groupPath itemIdx decKey
                AtTag subjId groupPath itemIdx tagIdx ->
                    -- next item inside this group or next group if there are no items after this one in this group
                    -- ...or the first tag inside the next item
                    case Nav.firstTabularInItem subjId groupPath itemIdx builder of
                        Just fistTabularIdx -> AtTabular subjId groupPath itemIdx fistTabularIdx
                        Nothing ->
                            case Nav.nextItem subjId groupPath itemIdx builder of
                                Just nextItemIdx -> AtItem subjId groupPath nextItemIdx
                                Nothing -> case Nav.nextGroup subjId groupPath builder of
                                    Just nextGroupPath -> AtGroup subjId nextGroupPath
                                    Nothing -> case Nav.nextSubject subjId builder of
                                        Just nextSubjId -> AtSubj nextSubjId
                                        Nothing -> AtTag subjId groupPath itemIdx tagIdx
                AtTabular subjId groupPath itemIdx tabularIdx ->
                    -- ...next tabular inside this item or next item
                    -- or the first tabular inside the next item
                    case Nav.nextTabular subjId groupPath itemIdx tabularIdx builder of
                        Just nextTabularIdx -> AtTabular subjId groupPath itemIdx nextTabularIdx
                        Nothing ->
                            case Nav.nextItem subjId groupPath itemIdx builder of
                                Just nextItemIdx -> AtItem subjId groupPath nextItemIdx
                                Nothing -> case Nav.nextGroup subjId groupPath builder of
                                    Just nextGroupPath -> AtGroup subjId nextGroupPath
                                    Nothing -> case Nav.nextSubject subjId builder of
                                        Just nextSubjId -> AtSubj nextSubjId
                                        Nothing -> AtItem subjId groupPath itemIdx

        Left ->

            case loc of
                Nowhere -> Nowhere
                AtSubj subjId -> AtSubj subjId
                AtGroup subjId groupPath -> AtGroup subjId groupPath
                AtItem subjId groupPath itemIdx ->
                    -- first suffix decorator inside this item or just stay in the item
                    case Nav.nextInlinePos @item_tag subjId groupPath itemIdx Nav.PKItemName builder of
                        Just nextPosKey -> posKeyToLocation subjId groupPath itemIdx nextPosKey
                        Nothing -> AtItem subjId groupPath itemIdx
                AtDecorator subjId groupPath itemIdx decKey ->
                    -- next decorator inside this item, or item name, if it's the last prefix, or just stay in the item
                    case Nav.nextInlinePos @item_tag subjId groupPath itemIdx (Nav.toPosKey decKey) builder of
                        Just nextPosKey -> posKeyToLocation subjId groupPath itemIdx nextPosKey
                        Nothing -> AtItem subjId groupPath itemIdx
                AtTag subjId groupPath itemIdx tagIdx ->
                    -- next tag inside this item, or next decorator, or just stay in the item
                    case Nav.nextInlinePos @item_tag subjId groupPath itemIdx Nav.PKTags builder of
                        Just nextPosKey -> posKeyToLocation subjId groupPath itemIdx nextPosKey
                        Nothing -> AtItem subjId groupPath itemIdx
                AtTabular subjId groupId itemIdx tabIdx ->
                    AtTabular subjId groupId itemIdx tabIdx

        Right ->

            case loc of
                Nowhere -> Nowhere
                AtSubj subjId -> AtSubj subjId
                AtGroup subjId groupId -> AtGroup subjId groupId
                AtItem subjId groupPath itemIdx ->
                    -- last prefix decorator inside this item or just stay in the item
                    case Nav.previousInlinePos @item_tag subjId groupPath itemIdx Nav.PKItemName builder of
                        Just prevPosKey -> posKeyToLocation subjId groupPath itemIdx prevPosKey
                        Nothing -> AtItem subjId groupPath itemIdx
                AtDecorator subjId groupPath itemIdx decKey ->
                    -- previous decorator inside this item, or item name, if it's the first suffix, or just stay in the item
                    case Nav.previousInlinePos @item_tag subjId groupPath itemIdx (Nav.toPosKey decKey) builder of
                        Just prevPosKey -> posKeyToLocation subjId groupPath itemIdx prevPosKey
                        Nothing -> AtItem subjId groupPath itemIdx
                AtTag subjId groupPath itemIdx tagIdx ->
                    -- prev tag inside this item, or prev decorator, or just stay in the item
                    case Nav.previousInlinePos @item_tag subjId groupPath itemIdx Nav.PKTags builder of
                        Just prevPosKey -> posKeyToLocation subjId groupPath itemIdx prevPosKey
                        Nothing -> AtItem subjId groupPath itemIdx
                AtTabular subjId groupId itemIdx tabIdx -> AtTabular subjId groupId itemIdx tabIdx

        where
            posKeyToLocation subjId groupPath itemIdx = case _ of
                Nav.PKRating ->           AtDecorator subjId groupPath itemIdx Dec.KRating
                Nav.PKPriority ->         AtDecorator subjId groupPath itemIdx Dec.KPriority
                Nav.PKTask ->             AtDecorator subjId groupPath itemIdx Dec.KTask
                Nav.PKItemName ->         AtItem subjId groupPath itemIdx
                Nav.PKProgress mbPvTag -> AtDecorator subjId groupPath itemIdx $ Dec.KProgress $ fromMaybe (P.PValueTag "UNK") mbPvTag
                Nav.PKEarnedAt ->         AtDecorator subjId groupPath itemIdx Dec.KEarnedAt
                Nav.PKDescription ->      AtDecorator subjId groupPath itemIdx Dec.KDescription
                Nav.PKReference ->        AtDecorator subjId groupPath itemIdx Dec.KReference
                Nav.PKTags ->             AtTag subjId groupPath itemIdx 0