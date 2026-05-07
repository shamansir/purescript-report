; REP format syntax highlighting queries

; ── Keywords ────────────────────────────────────────────────────────────────

(subject_header
  "SBJ. " @keyword)

(subject_tag
  "# " @comment)

; ── Markers ──────────────────────────────────────────────────────────────────

(tri_marker) @type.builtin

; ── Subject ──────────────────────────────────────────────────────────────────

(subject_header
  name: (subject_name) @string.special)

; ── Group ────────────────────────────────────────────────────────────────────

(group_header
  title: (group_title) @namespace)

; ── Items ────────────────────────────────────────────────────────────────────

(item_title
  text: (item_title_text) @variable)

; ── Identifiers / path IDs ───────────────────────────────────────────────────

(id_clause
  " // " @operator
  value: (identifier) @attribute)

; ── Tabular labels ───────────────────────────────────────────────────────────

(subject_tabular_header
  label: (label_text) @property)

(tabular_header
  label: (label_text) @property)

; ── Values ───────────────────────────────────────────────────────────────────

(tabular_value_line
  value: (value_text) @string)

(decorator
  value: (value_text) @string)

(continuation_line
  content: (continuation_text) @string)

; ── Tags ─────────────────────────────────────────────────────────────────────

(subject_tag
  content: (tag_content) @comment)

(item_tag
  content: (tag_content) @comment)
