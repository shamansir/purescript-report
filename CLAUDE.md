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
| `Report.Chain` | Linked list used to represent a path of groups from root to current node |
| `Report.GroupPath` | Newtype wrapping a string path identifier for groups |
| `Report.Class` | Type classes: `IsItem`, `IsGroup`, `IsSubject`/`IsSubjectId`, `HasTags`, `HasDecorators`, `HasTabular`, `HasStats`, `IsTag`, `IsGroupable`, `IsSortable` |
| `Report.Decorator` | `Decorators` newtype holding the extensible decorator map |
| `Report.Decorators.*` | Concrete decorator implementations: `Stats`, `Tags`, `Progress`, `Rating`, `Task`, `Tabular` |
| `Report.Impl.*` | Default implementations of `Item`, `Group`, `Subject`, `Tag` |
| `Report.Convert.*` | Format converters: `Json`, `Dhall`, `Org`, `Text`, `Generic` |
| `Report.Web.Component` | Main Halogen interactive component for web UI |

### Type class conventions

- `i_*` prefix — methods on items (`i_title`, `i_tags`, `i_decorators`)
- `g_*` prefix — methods on groups (`g_title`, `g_path`)
- `s_*` prefix — methods on subjects (`s_id`, `s_name`, `s_unique`)
- `t_*` prefix — methods on tags (`t_group`)

### Tags vs Decorators

Tags (`Report.Decorators.Tags`) and Decorators (`Report.Decorator`) are separate systems. Tags are typed via `IsTag t` and attached via `HasTags t a`. Decorators are a generic extensible bag of named properties stored in the `Decorators` map.

### Test structure

Tests live in `test/Test/` and use the `spec`/`spec-node` framework. The demo app lives in `test/Demo/` and uses a game collection dataset in `test/games-samples/`.

Custom git dependencies (from `shamansir`'s GitHub repos): `yoga-tree-utils`, `play`, `yoga-tree-svg`, `dotlang`. The `diff-compare` package is expected at `../purescript-diff-compare` (local path).