# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Build
spago build

# Run tests
spago test

# Bundle web demo (output to ./web/app.js)
spago bundle --module Demo.WebApp --outfile ./web/app.js --platform browser --bundle-type app

# Serve demo locally on port 1238 (bundles + serves via Parcel)
./serve-demo-nix.sh

# Enter Nix dev shell (provides purs, spago, esbuild, dhall, nodejs)
nix develop
```

## Architecture

`Report` is a PureScript library for building typed hierarchical reports with three levels:

- **Subject** — top-level entity (e.g., a platform or category)
- **Group** — mid-level grouping, addressed via `GroupPath`; groups can be chained (`Chain Group`) to represent nested paths
- **Item** — leaf-level data

The core type `Report subj group item` wraps `Builder subj group item`, which holds the actual data. `Report.purs` re-exports much of `Builder` for consumers.

### Key modules

| Module | Role |
|--------|------|
| `Report` | Main API: `build`, `unfold`, `toTree`, tag/filter helpers |
| `Report.Builder` | Core data structure and all traversal/transformation functions (`mapItems`, `filterGroups`, `regroup`, etc.) |
| `Report.Chain` | Linked list (`End a \| More a (Chain a)`) for representing nested group paths |
| `Report.GroupPath` | Newtype wrapping `Array PathSegment`; encodes/decodes with `::` separator; helpers: `startsWith`, `startsWithNotEq` |
| `Report.Group` | `Group` record (title, path, stats); factory functions `mkGroup`, `rootGroup`, `quickChain`, `quickChain'` |
| `Report.Class` | Type classes: `IsItem`, `IsGroup`, `IsSubject`/`IsSubjectId`, `HasTags`, `HasDecorators`, `HasTabular`, `HasStats`, `IsTag`, `IsGroupable`, `IsSortable`, `ConvertTo`, `ConvertFrom`, `ToExport` |
| `Report.Core` | Date/time types (`SDate`, `SMonth`, `Language`, `LanguageLevel`) and formatting utilities |
| `Report.Core.Logic` | `ViewOrEdit a` for UI state: `View a \| Edit EncodedValue a` |
| `Report.Decorator` | `Decorators` newtype holding `Map Key Decorator`; keys: `KRating`, `KPriority`, `KTask`, `KProgress`, `KEarnedAt`, `KDescription`, `KReference` |
| `Report.Decorators.*` | Concrete decorator implementations: `Stats`, `Tags`, `Progress`, `Rating`, `Task`, `Priority` |
| `Report.Decorators.Q.*` | Quasi-quotation factory helpers for common decorators (`task_todo`, `task_done`, `qrating`, etc.) |
| `Report.Impl.*` | Default implementations of `Item`, `Group`, `Subject`, `Tag` |
| `Report.Modify` | Modification types: `What` (GroupName, ItemName, ItemDecorator, etc.), `Location`, `Modification` |
| `Report.Tabular` | `Tabular v` wrapping `Array (Item v)`; keyed access |
| `Report.Convert.*` | Format converters: `Json`, `Dhall`, `Org`, `Text`, `Generic`; plus `Types` for shared export types, `Keyed` for typed key serialization |
| `Report.Web.Component` | Main Halogen interactive component for web UI |
| `Report.Web.Navigation` / `Navigation2` | Navigation state management (Navigation2 is an alternative implementation with better data storage) |
| `Report.Web.Helpers.*` | URL config parsing, visual state, inline/block layout mode |
| `Utils.Report.*` | Domain-agnostic grouping, pagination helpers |

### Type class conventions

- `i_*` prefix — methods on items (`i_title`, `i_tags`, `i_decorators`)
- `g_*` prefix — methods on groups (`g_title`, `g_path`)
- `s_*` prefix — methods on subjects (`s_id`, `s_name`, `s_unique`)
- `t_*` prefix — methods on tags (`t_group`)

### Conversion type classes

- `ConvertTo trg src` — encode/serialize (`convertTo :: src -> trg`)
- `ConvertFrom trg src` — decode/deserialize (`convertFrom :: trg -> Maybe src`)
- `ToExport subj_id subj_tag item_tag subj group item x` — generic export framework combining all relevant constraints; used by all format converters

### Tags vs Decorators

Tags (`Report.Decorators.Tags`) and Decorators (`Report.Decorator`) are separate systems. Tags are typed via `IsTag t` and attached via `HasTags t a`. Decorators are a generic extensible bag of named properties stored in the `Decorators` map with typed keys (`KRating`, `KPriority`, `KTask`, `KProgress`, `KEarnedAt`, `KDescription`, `KReference`).

### Tabular extension

`HasTabular a` / `Tabular v` provides structured key-value metadata for domain-specific data (analogous to a table schema). Distinct from Decorators — use Tabular for schema-like structured data, Decorators for cross-cutting item properties.

### Stats aggregation

`Report.Decorators.Stats.Collect` provides `collectStats` with `CollectWhat` (ItemsCount / ItemsProgress) for aggregating stats from items up to groups/subjects. Stats ADT: `SGotTotal`, `SWithProgress`, `SFromProgress`, `SCount`, `SNotRelevant`, `SYetUnknown`.

### Test structure

Tests live in `test/Test/` and use the `spec`/`spec-node` framework. The demo app lives in `test/Demo/` and uses a game collection dataset in `test/games-samples/`.

Custom git dependencies (from `shamansir`'s GitHub repos): `yoga-tree-utils`, `play`, `yoga-tree-svg`, `dotlang`. The `diff-compare` package is expected at `../purescript-diff-compare` (local path).
