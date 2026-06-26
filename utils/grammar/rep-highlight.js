// CodeMirror 6 highlight tags for the REP Lezer grammar.
// Referenced by rep.lezer as:  @external propSource repHighlighting from "./rep-highlight"
//
// After building the grammar (npx lezer-generator rep.lezer -o rep.js),
// import both together:
//
//   import { parser }          from './rep.js'
//   import { repHighlighting } from './rep-highlight.js'
//   import { LRLanguage }      from '@codemirror/language'
//
//   export const repLanguage = LRLanguage.define({
//     parser: parser.configure({ props: [repHighlighting] }),
//   })

import { styleTags, tags as t } from '@lezer/highlight'

export const repHighlighting = styleTags({
  // Structural keywords
  'subjectKW':          t.keyword,
  'groupHStart':        t.keyword,

  // Names
  'subjectName':        t.heading1,
  'GroupTitle':         t.heading2,
  'LabelText':          t.propertyName,
  'ItemTitleText':      t.variableName,
  'Identifier':         t.labelName,
  'TagContent':         t.lineComment,

  // Type markers
  'TypeMarker':         t.typeName,
  'UnknownMarker':      t.invalid,

  // Values
  'ValueText':          t.string,
  'ContinuationText':   t.string,

  // Sigils / punctuation
  'tagSigil':           t.lineComment,
  'idSep':              t.operator,
  'tabularSigil':       t.punctuation,
  'decoratorStart':     t.punctuation,
  'continuationStart':  t.operator,
  'dot':                t.punctuation,
})
