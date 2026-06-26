" Vim syntax file for the REP report format
" Place in ~/.vim/syntax/rep.vim  (or ~/.config/nvim/syntax/rep.vim)
" Add to ~/.vim/filetype.vim:  au BufRead,BufNewFile *.rep setfiletype rep

if exists("b:current_syntax")
  finish
endif

" ── Keywords ──────────────────────────────────────────────────────────────────

syntax keyword repSubjectKW  SBJ contained
syntax keyword repGroupKW    GRP contained

" ── Valid type markers ────────────────────────────────────────────────────────

" Progress types
syntax keyword repTypeValid
  \ NON UNK INT NUM TXT CMP PCI PCN PCX
  \ GTI GTN TIM DAT PPI PPN MSI MSN MSX
  \ RGI RGN PRG LVI LVN LVO LVS LVE LVP LVC REL XXX
  \ contained

" Decorator types
syntax keyword repTypeValid
  \ RAT PRI TSK ERN DSC REF
  \ contained

" Tabular-specific types
syntax keyword repTypeValid
  \ UID YER BOL DTR DTT TMR DMR TAG
  \ contained

" Anything else that looks like a type code — flags as error
syntax match repTypeInvalid /\<[A-Z]\+\>/ contained

" ── Value sub-tokens (used inside value regions) ───────────────────────────────

syntax keyword repBoolean DONE TODO DOING       contained
syntax match   repDate    /<[^>]\+>/            contained
syntax match   repTime    /\d\{1,4}:\d\{2}\(:\d\{2}\)\?/ contained
syntax match   repNumber  /-\?\d\+\(\.\d\+\)\?%\?/ contained
syntax match   repRelOp   /^[><=]\ze /          contained
syntax match   repPathSep /::/                  contained

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

" ── Tag line:  [indent]# <content> ───────────────────────────────────────────

syntax match repTag /^\s*#.\+$/

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

syntax cluster repValueContent contains=repBoolean,repDate,repTime,repRelOp,repPathSep,repNumber

" ── Highlighting links ────────────────────────────────────────────────────────

highlight default link repSubjectKW   Keyword
highlight default link repGroupKW     Keyword
highlight default link repSubjectName Title
highlight default link repGroupTitle  Type
highlight default link repIdSep       Operator
highlight default link repIdValue     Comment
highlight default link repTag         Comment
highlight default link repTabDash     Delimiter
highlight default link repTabLabel    Identifier
highlight default link repTabSemi     Delimiter
highlight default link repDecColon    Delimiter
highlight default link repContMark    Operator
highlight default link repTypeDot     Delimiter
highlight default link repTypeValid   Special
highlight default link repTypeInvalid Error
highlight default link repBoolean     Boolean
highlight default link repDate        Constant
highlight default link repTime        Number
highlight default link repNumber      Number
highlight default link repRelOp       Operator
highlight default link repPathSep     Delimiter
highlight default link repItemTitle   Normal

let b:current_syntax = "rep"
