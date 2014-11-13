set term=builtin_ansi

syntax on
filetype plugin indent on

" Remove trailing whitespace on save
" and maintain cursor position
"fun! <SID>StripTrailingWhitespaces()
"    let l = line(".")
"    let c = col(".")
"    %s/\s\+$//e
"    call cursor(l, c)
"endfun
"autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

"plugins
execute pathogen#infect()
let g:syntastic_enable_haskell_checker = 1
let jshint2_command = 'jshint'
let jshint2_read = 1
hi ghcmodType ctermbg=blue
let g:ghcmod_type_highlight = 'ghcmodType'
let g:ghcmod_hlint_options = ["--ignore=Redundant $"]
let g:newghc_enable_detailed_browse = 1
let g:acp_enableAtStartup = 0
let g:syntastic_haskell_checkers=['hlint']

let g:org_todo_keywords=['TODO', 'PENDING', 'VERIFY', '|', 'DONE', 'DELEGATED']

"settings
set expandtab
set tabstop=2
set number
set showmatch
set incsearch

match Todo /\s\+$/

nmap <TAB> :tabnext<cr>
