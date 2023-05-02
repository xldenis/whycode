" Vim syntax file
" Language:     Why3 Info File
" Filenames:    *.whyinfo
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded

if exists('b:current_syntax') && b:current_syntax == "whyinfo"
  finish
endif

" Why3 is case sensitive.
syn case match

syn match    whyinfoDel     "\["
syn match    whyinfoDel     "\]"
syn match    whyinfoLoc     "\d\(\d\)\?:\d\(\d\)\?"

syn match    whyinfoKeyword_0 "Verification Conditions"
syn keyword  whyinfoKeyword_1 goal
syn keyword  whyinfoKeyword_2 from to

" Synchronization
syn sync minlines=50
syn sync maxlines=500

hi def link whyinfoKeyword_0 Identifier
hi def link whyinfoKeyword_1 Identifier
hi def link whyinfoKeyword_2 Comment
hi def link whyinfoDel       Comment
hi def link whyinfoLoc       Constant

let b:current_syntax = "whyinfo"

" vim: ts=8
