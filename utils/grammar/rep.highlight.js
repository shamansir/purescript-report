/**
 * Highlight.js language definition for the REP report format.
 *
 * Usage:
 *   import hljs from 'highlight.js/lib/core';
 *   import rep from './rep.highlight.js';
 *   hljs.registerLanguage('rep', rep);
 *   hljs.highlightAll();
 *
 * Or in a browser bundle:
 *   hljs.registerLanguage('rep', rep);
 */

// Type codes grouped by semantic category — each gets a distinct CSS class
const TYPE_DATE    = { className: 'built_in',  match: /\b(DAT|YER|DTR|DTT|DMR)\b/ };
const TYPE_TIME    = { className: 'symbol',    match: /\b(TIM|TMR)\b/ };
const TYPE_TEXT    = { className: 'string',    match: /\b(TXT|DSC|REF|UID|TAG)\b/ };
const TYPE_NUMBER  = { className: 'number',    match: /\b(INT|NUM|GTI|GTN|PPI|PPN|RGI|RGN)\b/ };
const TYPE_MEASURE = { className: 'attribute', match: /\b(MSI|MSN|MSX|PCI|PCN|PCX|ERN)\b/ };
const TYPE_PROGRESS= { className: 'literal',   match: /\b(PRG|CMP|BOL)\b/ };
const TYPE_RATING  = { className: 'keyword',   match: /\b(RAT|PRI|TSK)\b/ };
const TYPE_LEVEL   = { className: 'type',      match: /\b(LVI|LVN|LVO|LVS|LVE|LVP|LVC|REL)\b/ };
const TYPE_UNKNOWN = { className: 'comment',   match: /\b(NON|UNK|XXX)\b/ };

const TYPE_MODES = [
  TYPE_DATE, TYPE_TIME, TYPE_TEXT, TYPE_NUMBER, TYPE_MEASURE,
  TYPE_PROGRESS, TYPE_RATING, TYPE_LEVEL, TYPE_UNKNOWN,
];

// Unknown type marker — invalid
const TYPE_INVALID = {
  className: 'meta', // hljs has no 'error' class by default; 'meta' is distinctive
  match: /\b[A-Z]+\b/,
};

// Sub-modes used inside value positions
const DATE_LITERAL = {
  className: 'built_in',      // <2025-08-12>
  match: /<[^>]+>/,
};

const TIME_LITERAL = {
  className: 'symbol',        // 07:54:00 or 40:00
  match: /\b\d{1,4}:\d{2}(:\d{2})?\b/,
};

const PROGRESS_LITERAL = {
  className: 'literal',       // DONE / TODO / DOING
  match: /\b(DONE|TODO|DOING)\b/,
};

const BOOLEAN_LITERAL = {
  className: 'number',        // TRUE / FALSE / YES / NO
  match: /\b(TRUE|FALSE|YES|NO|true|false)\b/,
};

const REL_OPERATOR = {
  className: 'operator',      // leading > = < in REL values
  match: /^[><=](?= )/,
};

const PATH_SEP = {
  className: 'punctuation',   // :: in DSC / REF values
  match: /::/,
};

const NUMBER_LITERAL = {
  className: 'number',
  match: /-?\b\d+(\.\d+)?%?\b/,
};

const TEXT_LITERAL = {
  className: 'string',        // catch-all for unmatched value tokens
  match: /\S+/,
};

const VALUE_MODES = [
  DATE_LITERAL, TIME_LITERAL, PROGRESS_LITERAL, BOOLEAN_LITERAL,
  REL_OPERATOR, NUMBER_LITERAL, PATH_SEP, TEXT_LITERAL,
];

// id clause shared by subject, group, tabular header
const ID_CLAUSE = {
  begin: / \/\//,
  end: /$/,
  className: 'comment',
};

/** @param {string} sigil  single char: ';' or ':' */
function markerLine(sigil) {
  return {
    begin: new RegExp(`^[ \\t]*${sigil === ':' ? ':' : ';'} `),
    end: /$/,
    contains: [
      ...TYPE_MODES,
      TYPE_INVALID,
      ...VALUE_MODES,
    ],
  };
}

export default function rep(_hljs) {
  return {
    name: 'REP',
    aliases: ['rep'],
    case_insensitive: false,
    contains: [

      // Subject header: SBJ. <name> [// id]
      {
        begin: /^SBJ\. /,
        end: /$/,
        returnBegin: true,
        contains: [
          { match: /^SBJ/, className: 'keyword' },
          ID_CLAUSE,
        ],
        className: 'title',
      },

      // Group header: [indent]GRP. <title> [// id]
      {
        begin: /^[ \t]+GRP\. /,
        end: /$/,
        returnBegin: true,
        contains: [
          { match: /GRP/, className: 'keyword' },
          ID_CLAUSE,
        ],
        className: 'section',
      },

      // Tag line: [indent]# <content>
      {
        begin: /^[ \t]*# /,
        end: /$/,
        className: 'comment',
      },

      // Tabular header: [indent]- <label> [// id]
      {
        begin: /^[ \t]*- /,
        end: /$/,
        returnBegin: true,
        contains: [
          { match: /-/, className: 'punctuation' },
          ID_CLAUSE,
        ],
        className: 'variable',
      },

      // Tabular value: [indent]; <TYPE>. <value>
      markerLine(';'),

      // Item decorator: [indent]: <TYPE>. <value>
      markerLine(':'),

      // Continuation line: [indent]>>> <content>
      {
        begin: /^[ \t]*>>> /,
        end: /$/,
        returnBegin: true,
        contains: [
          { match: />>>/, className: 'operator' },
          ...VALUE_MODES,
        ],
      },

      // Item title: indented line not starting with a known sigil
      {
        match: /^[ \t]+(?!GRP\. |[-;:#]|>>>)[^\n]+/,
        className: 'string',
      },
    ],
  };
}
