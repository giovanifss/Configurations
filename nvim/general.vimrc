"---------------------------------------------
"        GENERAL VIM CONFIGURATIONS
"---------------------------------------------
set noreadonly
set encoding=utf-8

if !&scrolloff
    set scrolloff=3       " Show next 3 lines while scrolling.
endif
if !&sidescrolloff
    set sidescrolloff=5   " Show next 5 columns while side-scrolling.
endif

" Avoid two spaces after join
set nojoinspaces

" Better nav
set number
set cursorline
set relativenumber
set hidden

" Indent
set autoindent
set smartindent

" vi:syntax=vim

" Base sanity stuff
set laststatus=2
set noswapfile
set autowriteall

" Also, let me have settings per project
set exrc
set secure

" Global taboptions
set tabstop=2
set shiftwidth=2
set expandtab

" Option complete
set wildmode=full

" Live substitution
set inccommand=split

" Make whitespaces visible
set list listchars=tab:▷⋅,trail:⋅,nbsp:⋅
set fillchars=vert:\│,fold:\─

let g:terminal_scrollback_buffer_size=9999999

"---------------------------------------------
"          BROUGTH FROM VIM:
"---------------------------------------------
" Remove any distro stuff
set nocompatible

" Open windows in the right and below
set splitbelow
set splitright

" Show part of command in last line
set showcmd

" Search case insensitive by default
set ignorecase
set smartcase

" Ask to save before quiting
set confirm

" Set command window to have 2 lines
set cmdheight=2
