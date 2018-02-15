"---------------------------------------------
"             NEOVIM THEME STUFF
"---------------------------------------------
set background=dark
set termguicolors

let g:ayucolor = 'dark'
colors jellybeans
let g:badwolf_darkgutter = 1

source $HOME/.config/nvim/color.vim

"---------------------------------------------
"       NO IDEA WHAT IS GOING ON HERE
"---------------------------------------------
" Disable a lot of stuff
let g:loaded_2html_plugin = 1
let g:loaded_gzip = 1
let g:loaded_tarPlugin = 1
let g:loaded_zipPlugin = 1
let g:loaded_netrwPlugin = 1

" XML folding config
let g:xml_syntax_folding=1
au FileType xml setlocal foldmethod=syntax
