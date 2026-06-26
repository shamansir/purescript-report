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

const VALID_TYPES =
  'NON|UNK|INT|NUM|TXT|CMP|PCI|PCN|PCX|' +
  'GTI|GTN|TIM|DAT|PPI|PPN|MSI|MSN|MSX|' +
  'RGI|RGN|PRG|LVI|LVN|LVO|LVS|LVE|LVP|LVC|REL|XXX|' +
  'RAT|PRI|TSK|ERN|DSC|REF|' +
  'UID|YER|BOL|DTR|DTT|TMR|DMR|TAG';

// Sub-modes used inside value positions
const DATE_LITERAL = {
  className: 'literal',       // <2025-08-12>
  match: /<[^>]+>/,
};

const TIME_LITERAL = {
  className: 'number',        // 07:54:00 or 40:00
  match: /\b\d{1,4}:\d{2}(:\d{2})?\b/,
};

const BOOLEAN_LITERAL = {
  className: 'literal',       // DONE / TODO / DOING
  match: /\b(DONE|TODO|DOING)\b/,
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

const VALUE_MODES = [DATE_LITERAL, TIME_LITERAL, BOOLEAN_LITERAL, REL_OPERATOR, PATH_SEP, NUMBER_LITERAL];

// Valid type marker (keyword-like, coloured as built-in type)
const TYPE_VALID = {
  className: 'type',
  match: new RegExp(`\\b(${VALID_TYPES})\\b`),
};

// Unknown type marker — invalid
const TYPE_INVALID = {
  className: 'meta', // hljs has no 'error' class by default; 'meta' is distinctive
  match: /\b[A-Z]+\b/,
};

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
      TYPE_VALID,
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
