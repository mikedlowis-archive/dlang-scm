"------------------------------------------------------------------------------
" Global Config Variables
"------------------------------------------------------------------------------
let g:ProjectPath = getcwd()
let g:TestSrcPath = g:ProjectPath . "/tests"
let g:CTagsFile   = g:ProjectPath . "/tags"

"------------------------------------------------------------------------------
" Global Function Definitions
"------------------------------------------------------------------------------
" Connect to the Correct CTags File
function! ConnectCTags()
    execute("set tags=" . g:CTagsFile)
endfunction

" Update the CTags File
function! RefreshCTags()
     if strlen(g:CTagsFile)
        execute('silent ! ctags -R *')
     endif
endfunction

"------------------------------------------------------------------------------
" Project Specific Key Mappings
"------------------------------------------------------------------------------
map <silent> <F2>  <F3>
map <silent> <F3>  <ESC>:call RefreshCTags()<CR>
map <silent> <F6>  <ESC>:execute("find " . g:TestSrcPath . "/**/test_" . expand("%:t:r") . ".scm")<CR>
map <silent> <F10> <ESC>:make<CR>

"------------------------------------------------------------------------------
" Connect To CTags
"------------------------------------------------------------------------------
call ConnectCTags()

