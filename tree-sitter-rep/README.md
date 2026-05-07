# tree-sitter-rep

Tree-sitter grammar for the **REP** hierarchical report format.

## REP format overview

```
SBJ. Subject Name // subject-id
# tag-one
# tag-two
- Tabular Label // tabular-id
; TXT. tabular value
    GRP. Group Title // path-id
    - Path // path
    ; TXT. some/path
    - Index // index
    ; INT. 0
        Item Title
        : MARKER. decorator value
        >>> continuation line
        : MARKER. another decorator
        # item-tag
        - Tabular Label // id
        ; INT. 42
        GRP. Nested Group // nested-id
        - Index // index
        ; INT. 1
            Nested Item
            : GTI. 1 5
```

### Line types

| Prefix | Construct | Indent |
|--------|-----------|--------|
| `SBJ. ` | Subject header | 0 |
| `# ` | Tag | any |
| `- ` | Tabular header | any |
| `; ` | Tabular value | any |
| `GRP. ` | Group header | indented |
| `: ` | Item decorator | indented |
| `>>> ` | Continuation | indented |
| *(none)* | Item title | indented |

### Markers (tri-markers)

3–4 letter uppercase codes used after `;` (tabular) and `:` (decorator):

| Marker | Type |
|--------|------|
| `TXT` | Text string |
| `INT` | Integer |
| `NUM` | Number |
| `BOL` | Boolean |
| `DAT` | Date `<YYYY-MM-DD>` |
| `TIM` | Time `HH:MM:SS` |
| `GTI` | Get-to integer `got total` |
| `GTN` | Get-to number `got total` |
| `PCN` | Percent number |
| `PCI` | Percent integer |
| `MSI` | Measured integer `amount unit` |
| `RGI` | Range integer `from to` |
| `LVI` | Levels integer (with `>>>` continuations) |
| `CMP` | Completion `TODO\|DONE` |
| `UNK` | Unknown `.` |
| ... | *(see Keys.purs for full list)* |

## Build

```bash
npm install
npm run build      # runs tree-sitter generate + node-gyp rebuild
npm test           # runs corpus tests
```

Requires `tree-sitter-cli` (installed as devDependency).

## NeoVim setup

### With nvim-treesitter

Add the parser to your nvim-treesitter config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.rep = {
  install_info = {
    url = "path/to/tree-sitter-rep",  -- local path or GitHub URL
    files = { "src/parser.c" },
    branch = "main",
  },
  filetype = "rep",
}
```

Then add `*.rep` filetype detection (e.g. in `~/.config/nvim/ftdetect/rep.vim`):

```vim
autocmd BufRead,BufNewFile *.rep set filetype=rep
```

Copy `queries/nvim-highlights.scm` to:
`~/.config/nvim/queries/rep/highlights.scm`

Copy `queries/folds.scm` to:
`~/.config/nvim/queries/rep/folds.scm`

Copy `queries/tags.scm` to:
`~/.config/nvim/queries/rep/tags.scm`

### With lazy.nvim (full example)

```lua
{
  "nvim-treesitter/nvim-treesitter",
  opts = function(_, opts)
    local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
    parser_config.rep = {
      install_info = {
        url = "~/path/to/tree-sitter-rep",
        files = { "src/parser.c" },
      },
      filetype = "rep",
    }
    vim.filetype.add({ extension = { rep = "rep" } })
  end,
}
```

## VSCode setup

### With vscode-anygrammar or vscode-tree-sitter

1. Build the WASM target:
   ```bash
   npx tree-sitter build-wasm
   ```
   This produces `tree-sitter-rep.wasm`.

2. Use with [vscode-tree-sitter](https://github.com/georgewfraser/vscode-tree-sitter) or create a VS Code extension.

### TextMate grammar (fallback)

For basic highlighting without a full tree-sitter integration you can use
the TextMate-compatible scope names already in `queries/highlights.scm`.
The capture names map to standard TextMate scopes:

| Capture | TextMate scope |
|---------|----------------|
| `@keyword` | `keyword` |
| `@type.builtin` | `storage.type` |
| `@namespace` | `entity.name.namespace` |
| `@module` | `entity.name.module` |
| `@string` | `string` |
| `@comment` | `comment` |
| `@attribute` | `entity.other.attribute-name` |
| `@operator` | `keyword.operator` |
| `@property` / `@field` | `variable.other.property` |

## Grammar notes

- Indentation is significant but not strictly enforced at the grammar level.
  Nesting is inferred by `tree-sitter` from the `_indent` token (any leading
  whitespace before a `GRP.` or item line).
- The grammar is left-recursive–safe; `prec.right` is used where needed.
- Continuation lines (`>>> `) are children of the immediately preceding
  `decorator` or `tabular_value_line` node.
