; NeoVim-specific highlights (nvim-treesitter capture names)
; Copy or symlink this to your nvim-treesitter queries/rep/highlights.scm

; ── Keywords ─────────────────────────────────────────────────────────────────

(subject_header "SBJ. " @keyword)

(subject_tag "# " @comment.line)

; ── Tri-markers ───────────────────────────────────────────────────────────────

(tri_marker) @type.builtin

; ── Subject ───────────────────────────────────────────────────────────────────

(subject_header name: (subject_name) @module)

; ── Group ─────────────────────────────────────────────────────────────────────

(group_header title: (group_title) @namespace)

; ── Item ──────────────────────────────────────────────────────────────────────

(item_title text: (item_title_text) @variable)

; ── IDs ───────────────────────────────────────────────────────────────────────

(id_clause " // " @operator)
(id_clause value: (identifier) @attribute)

; ── Labels ────────────────────────────────────────────────────────────────────

(subject_tabular_header label: (label_text) @field)
(tabular_header         label: (label_text) @field)

; ── Values ────────────────────────────────────────────────────────────────────

(tabular_value_line value: (value_text) @string)
(decorator          value: (value_text) @string)
(continuation_line  content: (continuation_text) @string)

; ── Tags ──────────────────────────────────────────────────────────────────────

(subject_tag content: (tag_content) @comment)
(item_tag    content: (tag_content) @comment)
