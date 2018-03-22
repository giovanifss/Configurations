"---------------------------------------------
"       PLUGIN SPECIFIC CONFIGURAITONS
"---------------------------------------------
source $HOME/.config/nvim/plugins/deoplete.vimrc
source $HOME/.config/nvim/plugins/async-clj-omni.vimrc
source $HOME/.config/nvim/plugins/vim-sexp.vimrc
source $HOME/.config/nvim/plugins/fzf-proj.vimrc
source $HOME/.config/nvim/plugins/iron.vimrc
source $HOME/.config/nvim/plugins/haskell-vim.vimrc

"---------------------------------------------
"           PLUGIN MANAGER STUFF 
"---------------------------------------------
call plug#begin('~/.config/nvim/plugged')
" Haskell
Plug 'neovimhaskell/haskell-vim'

" Theme
Plug 'nanotech/jellybeans.vim'

" Code Completion
Plug 'shougo/deoplete.nvim'

" Filesystem tinkering
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'
Plug 'Shougo/denite.nvim'

" Project
Plug 'hkupty/fzf-proj.vim'

" Snippets
Plug 'shougo/neosnippet.vim'
Plug 'shougo/neosnippet-snippets'

" Split and join
Plug 'hkupty/timeshift.vim'

" Repls
Plug 'hkupty/iron.nvim'

" Lint Engine
Plug 'w0rp/ale'

" Cool search stuff
Plug 'romainl/vim-cool'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Code commenting
Plug 'scrooloose/nerdcommenter'

" Git stuff
Plug 'mhinz/vim-signify'
Plug 'jreybert/vimagit', { 'branch': 'next' }
" Plug 'lambdalisue/gina.vim'

Plug 'vim-scripts/AnsiEsc.vim'

" Tpope stuff
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-eunuch'

" Web requests
Plug 'mattn/webapi-vim'

" Gist
Plug 'mattn/gist-vim'

" Align
Plug 'tommcdo/vim-lion'

"-- Syntax specific and some more stuff
" Docker
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }

" Clojure
Plug 'clojure-vim/acid.nvim', { 'branch': 'testing' }
Plug 'clojure-vim/async-clj-omni'
Plug 'fholiveira/vim-clojure-static',  { 'for': 'clojure', 'branch': 'hack-update'}
Plug 'hkupty/async-clj-highlight',  { 'for': 'clojure', 'branch': 'acid-autocmd' }

" Python
Plug 'hynek/vim-python-pep8-indent', { 'for': 'python' }
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'zchee/deoplete-jedi', { 'for': 'python' }

" Vim - Vader
Plug 'Shougo/neco-vim', { 'for': 'vim' }
Plug 'junegunn/vader.vim'

" Vim numbers
Plug 'myusuf3/numbers.vim'

" Increment numbers in column
Plug 'triglav/vim-visual-increment'

" Splunk syntax
Plug 'vim-scripts/splunk.vim'

" Tasks
" Plug 'blindFS/vim-taskwarrior', { 'on': 'TW' }

" Eyecandy
Plug 'ryanoasis/vim-devicons'

call plug#end()
