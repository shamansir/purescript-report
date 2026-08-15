module Report.Web.CSS where

import Prelude
import Report.Class (TagColors)

lineHeight_ :: Number
lineHeight_ = 1.9

levelsMargin_ :: Number
levelsMargin_ = 30.0


-- ── Root layout ───────────────────────────────────────────────────────────────

rootLayout :: String
rootLayout = "font-family: \"JetBrains Mono\", sans-serif; display: flex; flex-direction: row;"

reportArea :: String
reportArea = "height: 100vh; min-width: 75%; overflow-y: scroll;outline:none;"

mainContent :: String
mainContent = "margin: 0 auto; max-width: 900px; padding: 20px 20px 50px 20px;"


-- ── Import / export panel ─────────────────────────────────────────────────────

importExportPanel :: String
importExportPanel = "position: absolute; right: 25%; top: 43px; width: 25%; max-height: 50%; overflow-y: scroll; background-color: aliceblue; border-radius: 5px; padding: 5px;"

importExportTextarea :: String
importExportTextarea = "white-space: pre-wrap; background: white; border: none; font-size: 0.7em;"

importExportCloseArea :: String
importExportCloseArea = "position: absolute; right: 0; top: 0; padding: 6px 0px;"


-- ── Toolbar ───────────────────────────────────────────────────────────────────

toolbar :: String
toolbar = "position: fixed; right: 25%; top: 0; border-radius: 5px; background: aliceblue; padding: 5px;"

toolbarButton :: String
toolbarButton = "margin: 0 5px; padding: 3px 8px; cursor: pointer; border-radius: 10px;"


-- ── Subjects navigation panel ─────────────────────────────────────────────────

subjectsNav :: String
subjectsNav = "position: fixed;right: 25%;top: 3em;border-radius: 5px;background: beige;padding: 5px;flex-direction: column;display: flex;text-align: end;font-size: 0.9em;"

subjectsNavLink :: String
subjectsNavLink = "color: darkgoldenrod; text-decoration: none;"

subjectsNavBadge :: String
subjectsNavBadge = "color: black; margin-right: 5px; font-size: 0.7em; position: relative; top: -1px; margin-left: 4px; "


-- ── Groups navigation panel ───────────────────────────────────────────────────

groupsNavExpanded :: String
groupsNavExpanded = "position: fixed;right: 25%;bottom:0;border-radius: 5px;background: beige;padding: 5px;flex-direction: column;display: flex;text-align: end;font-size: 0.6em; width: 15%; opacity: 0.8;max-height:400px;overflow:scroll;padding:9px;"

groupsNavCollapseBtn :: String
groupsNavCollapseBtn = "position:absolute;right:0;width:40px;"

groupsNavCollapsed :: String
groupsNavCollapsed = "position: fixed;right: 25%;bottom:0;border-radius: 5px;background: beige;padding: 5px;flex-direction: column;display: flex;text-align: end;font-size: 0.6em; width: 15%; opacity: 0.5;max-height:2.4em;overflow:scroll;min-height: 2.4em;"

groupsNavLabel :: String
groupsNavLabel = "position:absolute;right:40px;top:10px;"

groupsNavBtn :: String
groupsNavBtn = "display: inline-block;position:absolute;right:0;width:40px;"

-- | The `isFirst` flag picks the top-padding variant (no top-padding for the first subject).
groupsNavSubjectName :: Boolean -> String
groupsNavSubjectName isFirst =
    "min-width:100%;text-align:start;font-weight:bold;"
    <> if isFirst then "padding:0 0 5px 0;" else "padding:15px 0 5px 0;"

groupsNavGroupRow :: String
groupsNavGroupRow = "min-width:100%;text-align:start;"

groupsNavGroupLink :: String
groupsNavGroupLink = "padding-left: 5px;text-decoration:none;"

groupsNavUnselectedIndicator :: String
groupsNavUnselectedIndicator = "display: inline-block; width: 20px;opacity:0.3;"


-- ── Tag filter / options panel ────────────────────────────────────────────────

tagFilterPanel :: String
tagFilterPanel = "padding: 9px 9px 0 0; width: 400px;"

tagFilterScrollable :: String
tagFilterScrollable = "overflow-y: scroll; height: 100%; position: absolute;"

tagFilterSearchInput :: String
tagFilterSearchInput = "border: 1px solid lightgray; border-radius: 5px; padding: 5px 6px; margin-bottom: 15px; min-width: 250px;"

tagFilterOptionsBtn :: String
tagFilterOptionsBtn = "padding: 1px 3px 0 5px; cursor: pointer;"

-- | Dims the button when tag display is off.
tagFilterTagsBtn :: Boolean -> String
tagFilterTagsBtn showItemsTags =
    "padding: 1px 3px 0 5px; cursor: pointer;"
    <> if showItemsTags then "" else "opacity: 0.3;"

tagFilterSortBtn :: String
tagFilterSortBtn = "padding: 1px 3px; cursor: pointer;"

tagFilterTagsList :: String
tagFilterTagsList = "display: block; padding: 5px 0; max-height: 300px; overflow-y: scroll;"


-- ── Processing / filter buttons ───────────────────────────────────────────────

processingButtonsRow :: String
processingButtonsRow = "display: block; margin: 7px 4px;"

processButton :: String
processButton = "background-color: rgb(139, 121, 182); color: white; border-radius: 5px; padding: 3px 5px; margin: 0 3px; cursor: pointer;"

-- | Dims the tag button when it is not active in the current filter.
subjectTagButton :: Boolean -> String
subjectTagButton tagEnabled =
    "display: inline-block; cursor: pointer; padding: 3px 0; opacity: "
    <> show (if tagEnabled then 1.0 else 0.5) <> ";"


-- ── Subject TOC rows ──────────────────────────────────────────────────────────

subjectTocRow :: String
subjectTocRow = "margin: 5px 0;"

subjectTocSelectBtn :: String
subjectTocSelectBtn = "cursor: pointer;"

-- | Highlights the row when the subject is in the current selection.
subjectTocNameBtn :: Boolean -> String
subjectTocNameBtn isSelected =
    "background-color: " <> (if isSelected then "bisque" else "transparent")
    <> "; cursor: pointer; padding: 5px; margin-left: 5px;"


-- ── Subject rendering ─────────────────────────────────────────────────────────

subjectWrapper :: String
subjectWrapper = "padding: 10px 0 10px 20px;"

-- | Highlights the subject header when it is the current navigation target.
subjectHeader :: Boolean -> String
subjectHeader isNavigated =
    "margin: 15px 0 30px 0; max-width: 60%; border-bottom: 1px solid gray; padding-bottom: 5px; font-size: 1.2em;"
    <> if isNavigated then "background-color: cornsilk;" else ""

subjectTagsSpan :: String
subjectTagsSpan = "font-size: 0.8em; margin-left: 5px;"


-- ── Group rendering ───────────────────────────────────────────────────────────

-- | Applies the group's nesting depth as a left margin.
groupWrapper :: Number -> String
groupWrapper marginPx =
    "padding-bottom: 10px; line-height: " <> show lineHeight_
    <> "em; margin-left: " <> show marginPx <> "px;"

groupRowFlex :: String
groupRowFlex = "display: flex; flex-direction: row;"

-- | Bold group header, highlighted when navigated to.
groupHeader :: Boolean -> String
groupHeader isNavigated =
    "font-weight: bold;"
    <> if isNavigated
        then "border: 1px dashed #95bad8ff; background-color: #f0f8ff;"
        else "border: 1px dashed transparent;"

-- | Collapse/expand triangle; faded when the group is expanded.
groupCollapseToggle :: Boolean -> String
groupCollapseToggle isCollapsed =
    "cursor: pointer; user-select: none; margin-right: 5px;"
    <> if isCollapsed then "" else "opacity: 0.25;"

groupItemsContainer :: String
groupItemsContainer = "border-left: 4px solid #eee; padding-left: 5px; margin-top: 10px; margin-bottom: 15px;"


-- ── Item rendering ────────────────────────────────────────────────────────────

-- | Background highlight when the item is the current navigation target.
itemWrapper :: Boolean -> String
itemWrapper isSelected =
    if isSelected
        then "background-color: #f0f8ff; border-radius: 3px;"
        else "background-color: transparent;"

-- | Background highlight when the item's title specifically is selected.
itemTitleWrapper :: Boolean -> String
itemTitleWrapper isTitleSelected =
    if isTitleSelected
        then "background-color: #fbd6fb; border-radius: 3px;"
        else ""


-- ── Navigation hint (debug overlay) ──────────────────────────────────────────

navigationHint :: String
navigationHint = "position: fixed; border: 1px solid black; background-color: #ffffe0ff; padding: 5px 10px; bottom: -5px; left: 60px; max-width: 70%; font-size: 0.7em; box-shadow: 2px 2px 5px gray; border-radius: 5px; overflow: hidden;"


-- ── Progress bar ──────────────────────────────────────────────────────────────

progressBarWrapper :: String
progressBarWrapper = "display: inline-block; position: relative; top: 2px; margin-right: 3px;"

-- | Filled (achieved) segment of the progress bar.
progressBarFilled :: String -> Number -> String
progressBarFilled color widthPx =
    "display: inline-block; background-color: " <> color
    <> "; height: 1em; width: " <> show widthPx <> "px;"

-- | Empty (remaining) segment of the progress bar.
progressBarEmpty :: String -> Number -> String
progressBarEmpty color widthPx =
    "display: inline-block; background-color: " <> color
    <> "; height: 1em; width: " <> show widthPx <> "px;"

levelRow :: String
levelRow = "margin-left: " <> show levelsMargin_ <> "px;"


-- ── Group stats ───────────────────────────────────────────────────────────────

groupStatsDefinedWrapper :: String
groupStatsDefinedWrapper = "opacity: 0.55;"

groupStatsCountWrapper :: String
groupStatsCountWrapper = "display: inline-block; position: relative; top: 1px;"

groupStatsBadge :: String
groupStatsBadge = "font-size: 0.8em; opacity: 0.8; margin: 5px 0;"

progressPlatesSvg :: String
progressPlatesSvg = "display: inline-block; position: relative; align-self: center; margin-left: 12px;border:1px solid lightblue;border-radius:3px;background-color:ghostwhite;"


-- ── Tag badges ────────────────────────────────────────────────────────────────

subjectTagWrapStyle :: String
subjectTagWrapStyle = "display: inline-block; cursor: pointer; padding: 3px 0; user-select: none;"

tagBadgeStyle :: String
tagBadgeStyle = "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; white-space: nowrap;"

-- | Single-segment tag chip — border, background, and text come from the tag's colour scheme.
tagChainEndStyle :: TagColors -> String
tagChainEndStyle c =
    "padding: 1px 3px; border-radius: 4px; border: 1px solid " <> c.border
    <> "; opacity: 0.8; background-color: " <> c.background
    <> "; color: " <> c.text <> "; user-select: none;"

-- | Outer wrapper for a chained (multi-segment) tag — left border only.
tagChainMoreWrapStyle :: TagColors -> String
tagChainMoreWrapStyle c =
    "border-radius: 4px; border-color: " <> c.border
    <> "; border-style: solid; border-width: 1px 0px 1px 1px; padding: 1px 0px 1px 4px;"

-- | Inner padding for the text part of a chained tag segment.
tagChainMoreInnerStyle :: String
tagChainMoreInnerStyle = "padding-right: 4px;"


-- ── Checkbox widget ───────────────────────────────────────────────────────────

-- | Background colour is set dynamically (e.g. green for done, silver for incomplete).
checkboxStyle :: String -> String
checkboxStyle bgColor =
    "display: inline-block;"
    <> "border: 1px solid darkgray;"
    <> "border-radius: 3px;"
    <> "width: 1em;"
    <> "height: 1em;"
    <> "color: white;"
    <> "position: relative; top: 2px; left: -2px;"
    <> "background-color: " <> bgColor <> ";"


-- ── Tabular values ────────────────────────────────────────────────────────────

subjectTabularValues :: String
subjectTabularValues = "display: block; margin: 0 0 14px 2px; color: royalblue; font-size: 0.8em; position: relative; top: -19px;"

tabularBlock :: String
tabularBlock = "display: block; margin: 0 0 0 25px; color: royalblue; font-size: 0.8em;"

tabularRow :: String
tabularRow = "display: block;"

tabularNest :: String
tabularNest = "border-left: 2px solid color-mix(in srgb, royalblue 20%, transparent); padding-left: 5px;"

tabularInnerNest :: String
tabularInnerNest = "border-left: 2px solid color-mix(in srgb, darkgreen 20%, transparent); padding-left: 5px; margin: 10px 0;"


-- ── Group path ────────────────────────────────────────────────────────────────

groupPathWrapper :: String
groupPathWrapper = "font-size: 0.8em;"

pathSegment :: String
pathSegment = "border: 1px solid #ddd; border-radius: 3px; padding: 2px 3px; color: coral;"

groupRefLink :: String
groupRefLink = "color: cadetblue; text-decoration: none;"
