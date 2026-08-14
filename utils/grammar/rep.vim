" Vim syntax file for the REP report format
" Place in ~/.vim/syntax/rep.vim  (or ~/.config/nvim/syntax/rep.vim)
" Add to ~/.vim/filetype.vim:  au BufRead,BufNewFile *.rep setfiletype rep

if exists("b:current_syntax")
  finish
endif

" ── Keywords ──────────────────────────────────────────────────────────────────

syntax keyword repSubjectKW  SBJ contained
syntax keyword repGroupKW    GRP contained

" ── Valid type markers by semantic category ───────────────────────────────────

syntax keyword repTypeDateValid   DAT YER DTR DTT DMR               contained
syntax keyword repTypeTimeValid   TIM TMR                            contained
syntax keyword repTypeTextValid   TXT DSC REF UID TAG                contained
syntax keyword repTypeNumValid    INT NUM GTI GTN PPI PPN RGI RGN    contained
syntax keyword repTypeMsrValid    MSI MSN MSX PCI PCN PCX ERN        contained
syntax keyword repTypeProgValid   PRG CMP BOL                        contained
syntax keyword repTypeRatValid    RAT PRI TSK                        contained
syntax keyword repTypeLvlValid    LVI LVN LVO LVS LVE LVP LVC REL   contained
syntax keyword repTypeUnkValid    NON UNK XXX                        contained

" Anything else that looks like a type code — flags as error
syntax match repTypeInvalid /\<[A-Z]\+\>/ contained

" ── Value sub-tokens (used inside value regions) ───────────────────────────────

syntax keyword repProgress DONE TODO DOING              contained
syntax keyword repBoolLit  TRUE FALSE YES NO true false  contained
syntax match   repDate     /<[^>]\+>/                   contained
syntax match   repTime     /\d\{1,4}:\d\{2}\(:\d\{2}\)\?/ contained
syntax match   repNumber   /-\?\d\+\(\.\d\+\)\?%\?/     contained
syntax match   repRelOp    /^[><=]\ze /                  contained
syntax match   repPathSep  /::/                          contained
syntax match   repValueTxt /\S\+/                        contained

" ── Subject line:  SBJ. <name> [// id] ───────────────────────────────────────

syntax region repSubjectLine start=/^SBJ\. / end=/$/ oneline keepend
  \ contains=repSubjectKW,repIdClause

syntax match  repSubjectName /[^/\n]*[^/ \n]/ contained containedin=repSubjectLine

" ── Group line:  [indent]GRP. <title> [// id] ────────────────────────────────

syntax region repGroupLine start=/^\s\+GRP\. / end=/$/ oneline keepend
  \ contains=repGroupKW,repIdClause

syntax match  repGroupTitle /[^/\n]*[^/ \n]/ contained containedin=repGroupLine

" ── Id clause:  // <identifier> ───────────────────────────────────────────────

syntax match repIdClause / \/\/.*$/ contained contains=repIdSep,repIdValue
syntax match repIdSep    / \/\//    contained
syntax match repIdValue  /[^\s\n]\+/ contained

" ── Tag line:  [indent]# <tag-id> [// <tag-content>] ────────────────────────

syntax region repTagLine start=/^\s*# / end=/$/ oneline keepend
  \ contains=repTagHash,repTagComment
syntax match repTagHash    /#/          contained
syntax match repTagComment / \/\/.*$/   contained

" ── Tabular header:  [indent]- <label> [// id] ───────────────────────────────

syntax region repTabularHeader start=/^\s*- / end=/$/ oneline keepend
  \ contains=repTabDash,repTabLabel,repIdClause

syntax match repTabDash  /-/  contained
syntax match repTabLabel /[^/\n]*[^/ \n]/ contained

" ── Tabular value:  [indent]; <TYPE>. <value> ─────────────────────────────────

syntax region repTabularValue start=/^\s*; / end=/$/ oneline keepend
  \ contains=repTabSemi,repTypeValid,repTypeInvalid,repTypeDot,repValueContent

syntax match repTabSemi  /;/ contained
syntax match repTypeDot  /\./  contained

" ── Item decorator:  [indent]: <TYPE>. <value> ───────────────────────────────

syntax region repDecoratorLine start=/^\s*: / end=/$/ oneline keepend
  \ contains=repDecColon,repTypeValid,repTypeInvalid,repTypeDot,repValueContent

syntax match repDecColon /:/ contained

" ── Continuation line:  [indent]>>> <content> ────────────────────────────────

syntax region repContinuation start=/^\s*>>> / end=/$/ oneline keepend
  \ contains=repContMark,repValueContent

syntax match repContMark />>>/ contained

" ── Item title (catch-all: indented line not starting with known sigils) ───────

syntax match repItemTitle /^\s\+[^-#:>][^\n]*/

" ── Value content cluster ─────────────────────────────────────────────────────

syntax cluster repValueContent contains=repDate,repTime,repProgress,repBoolLit,repRelOp,repNumber,repPathSep,repValueTxt

" ── Highlighting links ────────────────────────────────────────────────────────

highlight default link repSubjectKW   Keyword
highlight default link repGroupKW     Keyword
highlight default link repSubjectName Title
highlight default link repGroupTitle  Type
highlight default link repIdSep       Operator
highlight default link repIdValue     Comment
highlight default link repTagLine     Identifier
highlight default link repTagHash     Delimiter
highlight default link repTagComment  Comment
highlight default link repTabDash     Delimiter
highlight default link repTabLabel    Identifier
highlight default link repTabSemi     Delimiter
highlight default link repDecColon    Delimiter
highlight default link repContMark    Operator
highlight default link repTypeDot       Delimiter
highlight default link repTypeDateValid Constant
highlight default link repTypeTimeValid Special
highlight default link repTypeTextValid String
highlight default link repTypeNumValid  Number
highlight default link repTypeMsrValid  Float
highlight default link repTypeProgValid Boolean
highlight default link repTypeRatValid  Error
highlight default link repTypeLvlValid  Type
highlight default link repTypeUnkValid  Comment
highlight default link repTypeInvalid   Error
highlight default link repProgress      Boolean
highlight default link repBoolLit       Boolean
highlight default link repDate          Constant
highlight default link repTime          Special
highlight default link repNumber        Number
highlight default link repRelOp         Operator
highlight default link repPathSep       Delimiter
highlight default link repValueTxt      String
highlight default link repItemTitle     Normal

let b:current_syntax = "rep"
