"---------------------------------------------
"        GENERAL VIM CONFIGURATIONS
"---------------------------------------------
set noreadonly

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
"           Trouxe do vim normal:
"---------------------------------------------
" Retira qualquer coisa que a distro tenha imposto
set nocompatible

" Faz com que novas janelas sejam abertas em baixo ou a direita
set splitbelow
set splitright

" Mostra parte de comandos na ultima linha da tela
set showcmd

" Busca em case insensitive menos quando se busca em letras maiusculas
set ignorecase
set smartcase

" Ao inves de falhar o comando pq tem mudancas nao salvas, mostra um dialogo perguntando se deseja salvar
set confirm

" Seta a janela de comando para ter 2 linhas
set cmdheight=2
