set term=builtin_ansi

syntax on

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

set expandtab
set cindent
set tabstop=2
set number
set showmatch

nmap <TAB> :tabnext<cr>
