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

" Dark colors
Plug 'nanotech/jellybeans.vim'
Plug 'sjl/badwolf'
Plug 'joshdick/onedark.vim'
Plug 'ayu-theme/ayu-vim', { 'frozen': 1 }

" Code Completion
Plug 'shougo/deoplete.nvim'
Plug 'Shougo/denite.nvim'

" Let on modeline
Plug 'vim-scripts/let-modeline.vim'

" Ctags
" Plug 'ludovicchabant/vim-gutentags'

" Snippets
Plug 'shougo/neosnippet.vim'
Plug 'shougo/neosnippet-snippets'

" Tmux substitute
Plug 'hkupty/nvimux'

" Split and join
Plug 'hkupty/timeshift.vim'

" Repls
Plug 'hkupty/iron.nvim'

" Lint Engine
Plug 'w0rp/ale'

" Make
" Plug 'SevereOverfl0w/clojure-check', {'do': './install'}
Plug 'hkupty/clojure-check', {'do': './install', 'branch': 'testing'}

" Twitter
Plug 'vmchale/vim-twitter'

" Cool search stuff
Plug 'romainl/vim-cool'

" Filesystem tinkering
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'

" Project
Plug 'hkupty/fzf-proj.vim'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Code commenting
Plug 'scrooloose/nerdcommenter'

" Git stuff
Plug 'mhinz/vim-signify'
Plug 'jreybert/vimagit', { 'branch': 'next' }
Plug 'lambdalisue/gina.vim'

Plug 'vim-scripts/AnsiEsc.vim'

" Tpope stuff
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-eunuch'

" Web requests
Plug 'mattn/webapi-vim'

" Gist
Plug 'mattn/gist-vim'

" Code navigation
Plug 'unblevable/quick-scope', { 'on': 'QuickScopeToggle' }

" Eyecandy
Plug 'ryanoasis/vim-devicons'

" Align
Plug 'tommcdo/vim-lion'

"-- Syntax specific and some more stuff
" Docker
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }

" Scala
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'mdreves/vim-scaladoc', { 'for': 'scala' }

" Scala Build Tool - SBT
Plug 'derekwyatt/vim-sbt', { 'for': 'sbt.scala' }

" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }

" Clojure
Plug 'guns/vim-sexp', { 'for': 'clojure' }
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

"Notes and stuff
Plug 'xolox/vim-misc'
Plug 'xolox/vim-notes'

Plug 'vim-scripts/splunk.vim'

"Ledger
Plug 'ledger/vim-ledger'

" Tasks
Plug 'blindFS/vim-taskwarrior', { 'on': 'TW' }

call plug#end()