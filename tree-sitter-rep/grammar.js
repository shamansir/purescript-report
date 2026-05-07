/**
 * Tree-sitter grammar for the REP report format.
 *
 * REP is a hierarchical text format: Subject → Groups → Items
 *
 * Line prefixes determine node type:
 *   SBJ. <name> [// <id>]          subject header    (column 0)
 *   # <content>                     tag               (any indent)
 *   - <label> [// <id>]            tabular header    (any indent)
 *   ; <MARKER>. <value>             tabular value     (any indent)
 *   [indent]GRP. <title> [// <id>] group header      (indented)
 *   [indent]: <MARKER>. <value>    item decorator    (indented)
 *   [indent]>>> <content>          continuation line (indented)
 *   [indent]<text>                 item title        (indented, catch-all)
 *
 * Disambiguation strategy: every keyword-prefixed indented line uses a
 * COMBINED TOKEN (indent + keyword prefix as one regex) so that the lexer's
 * longest-match rule unambiguously resolves conflicts rather than relying on
 * parser-level precedence alone.
 *
 * Token hierarchy (longest match wins):
 *   _group_h_start       /[ \t]+GRP\. /  → group header
 *   _tabular_h_start     /[ \t]+-[ ]/    → group/item tabular header
 *   _decorator_start     /[ \t]+:[ ]/    → item decorator
 *   _item_tag_start      /[ \t]+#[ ]/    → item tag
 *   _continuation_start  /[ \t]+>>>[ ]/  → continuation line
 *   _item_indent         /[ \t]+/        → item title (shortest, catch-all)
 *
 * group_tabular and item_tabular are unified as `tabular_entry`.
 * group body is split into two phases:
 *   Phase 1: repeat(tabular_entry)           — tabulars before items
 *   Phase 2+3: repeat(choice(item, group))   — items and nested groups
 */

module.exports = grammar({
  name: 'rep',

  extras: $ => [],

  conflicts: $ => [],

  rules: {

    // ── Top level ──────────────────────────────────────────────────────────

    source_file: $ => repeat(choice(
      $.subject,
      $._newline,
    )),

    // ── Subject ────────────────────────────────────────────────────────────

    subject: $ => prec.right(seq(
      $.subject_header,
      repeat(choice(
        $.subject_tag,
        $.subject_tabular,
      )),
      repeat($.group),
    )),

    subject_header: $ => seq(
      'SBJ. ',
      field('name', $.subject_name),
      optional(field('id', $.id_clause)),
      $._newline,
    ),

    subject_name: $ => /[^/\n]*[^/ \n]/,

    subject_tag: $ => seq(
      '# ',
      field('content', $.tag_content),
      $._newline,
    ),

    subject_tabular: $ => seq(
      field('header', $.subject_tabular_header),
      field('value',  $.tabular_value_line),
    ),

    subject_tabular_header: $ => seq(
      '- ',
      field('label', $.label_text),
      optional(field('id', $.id_clause)),
      $._newline,
    ),

    // ── Group ──────────────────────────────────────────────────────────────
    //
    // Phase 1: zero-or-more tabular entries (tabulars before items)
    // Phase 2+3: zero-or-more items and nested groups (mixed)
    //
    // The split avoids the shift-reduce conflict that a single
    // repeat(choice(tabular|item|group)) creates on plain _indent tokens.

    group: $ => prec.right(seq(
      field('header', $.group_header),
      repeat(field('tabular', $.tabular_entry)),
      repeat(choice(
        field('item',  $.item),
        field('group', $.group),
      )),
    )),

    // Combined token: /[ \t]+GRP\. / beats _item_indent by longer match.
    _group_h_start: $ => /[ \t]+GRP\. /,

    group_header: $ => seq(
      $._group_h_start,
      field('title', $.group_title),
      optional(field('path_id', $.id_clause)),
      $._newline,
    ),

    // ── Tabular entry (group-level or item-level, unified) ─────────────────

    tabular_entry: $ => seq(
      field('header', $.tabular_header),
      field('value',  $.tabular_value_line),
    ),

    // Combined token: /[ \t]+-[ ]/ — indent + "- " as one token.
    _tabular_h_start: $ => /[ \t]+-[ ]/,

    tabular_header: $ => seq(
      $._tabular_h_start,
      field('label', $.label_text),
      optional(field('id', $.id_clause)),
      $._newline,
    ),

    tabular_value_line: $ => prec.right(seq(
      optional($._indent),
      '; ',
      field('marker', $.tri_marker),
      '. ',
      field('value', $.value_text),
      $._newline,
      repeat(field('continuation', $.continuation_line)),
    )),

    // ── Item ───────────────────────────────────────────────────────────────

    item: $ => prec.right(seq(
      field('title', $.item_title),
      repeat(choice(
        field('decorator', $.decorator),
        field('tag',       $.item_tag),
        field('tabular',   $.tabular_entry),
      )),
    )),

    // _item_indent is plain /[ \t]+/ — used only for item_title.
    // All keyword-prefixed rules use longer combined tokens, so the
    // lexer's longest-match unambiguously picks the right token.
    _item_indent: $ => /[ \t]+/,

    item_title: $ => seq(
      $._item_indent,
      field('text', $.item_title_text),
      $._newline,
    ),

    // Must NOT start with '-' (tabular), '#' (tag), ':' (decorator), '>' (continuation).
    item_title_text: $ => /[^-#:>\n][^\n]*/,

    // Combined token: /[ \t]+#[ ]/ — indent + "# " as one token.
    _item_tag_start: $ => /[ \t]+#[ ]/,

    item_tag: $ => seq(
      $._item_tag_start,
      field('content', $.tag_content),
      $._newline,
    ),

    // Combined token: /[ \t]+:[ ]/ — indent + ": " as one token.
    _decorator_start: $ => /[ \t]+:[ ]/,

    decorator: $ => prec.right(seq(
      $._decorator_start,
      field('marker', $.tri_marker),
      '. ',
      field('value', $.value_text),
      $._newline,
      repeat(field('continuation', $.continuation_line)),
    )),

    // ── Continuation ───────────────────────────────────────────────────────

    // Combined token: /[ \t]+>>>[ ]/ — indent + ">>> " as one token.
    _continuation_start: $ => /[ \t]+>>>[ ]/,

    continuation_line: $ => seq(
      $._continuation_start,
      field('content', $.continuation_text),
      $._newline,
    ),

    // ── Shared tokens ──────────────────────────────────────────────────────

    id_clause: $ => seq(
      ' // ',
      field('value', $.identifier),
    ),

    tri_marker: $ => /[A-Z]{2,4}/,

    identifier: $ => /[^\s\n]+/,

    // Stops before " // id" by requiring last char to be non-space/non-slash
    label_text: $ => /[^/\n]*[^/ \n]/,

    group_title: $ => /[^/\n]*[^/ \n]/,

    tag_content: $ => /[^\n]+/,

    value_text: $ => /[^\n]*/,

    continuation_text: $ => /[^\n]*/,

    // Used only by tabular_value_line (optional leading indent for value lines).
    _indent: $ => /[ \t]+/,

    _newline: $ => /\n/,
  },
})
