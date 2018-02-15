"---------------------------------------------
"                 MAPPING KEYS
"---------------------------------------------
" Leader key
let mapleader="\\"

" Local leader key
nnoremap <SPACE> <Nop>
""let maplocalleader="\<Space>"
let maplocalleader="\<Space>"

"---------------------------------------------
"           NORMAL MODE BINDINGS
"---------------------------------------------
nnoremap <C-h> <C-W>H
nnoremap <C-j> <C-W>J
nnoremap <C-k> <C-W>K
nnoremap <C-l> <C-W>L

" Centralize stuff
nnoremap n nzz
nnoremap N Nzz
" nnoremap <C-o> <C-o>zz
" nnoremap <C-i> <C-i>zz

"-------------------------
" Local Leader Bindings
"-------------------------
" File navigation
nnoremap <LocalLeader>gb 25%
nnoremap <LocalLeader>gm 50%
nnoremap <LocalLeader>gt 75%

"-------------------------
"    Leader bindings
"-------------------------
" Splits command binds
nnoremap <Leader>vv :vsp<CR>
nnoremap <Leader>vf :vsp<CR>:Files<CR>
nnoremap <Leader>vb :vsp<CR>:Buffers<CR>
nnoremap <Leader>v: :vsp<CR>:
nnoremap <Leader>vt :vsp<CR>:lcd %:p:h<CR>:terminal<CR>i
nnoremap <Leader>vs :sp<CR>
nnoremap <Leader>sf :sp<CR>:Files<CR>
nnoremap <Leader>sb :sp<CR>:Buffers<CR>
nnoremap <Leader>s: :sp<CR>:
nnoremap <Leader>st :sp<CR>:lcd %:p:h<CR>:terminal<CR>i

" Project find binds
nnoremap <Leader>proj :Projects<CR>

" Explorer command binds
nnoremap <Leader>eh :e .<CR>
nnoremap <Leader>oH :!ls ~/<CR>:e ~/

" Tab command binds
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>th :tabnew %<CR>
nnoremap <Leader>tb :tabnew<CR>:Buffers<CR>
nnoremap <Leader>tf :tabnew<CR>:Files<CR>
nnoremap <Leader>tt :tabnew<CR>:terminal<CR>i
nnoremap <Leader>t: :tabnew<CR>:
nnoremap <Leader>tl :Windows<CR>
nnoremap <Leader>ont :tabnew<CR>:e ~/

" Change root directory
nnoremap <Leader>cwd :cd %:p:h<CR>:pwd<CR>

" Open binds file
nnoremap <Leader>binds :tabe $HOME/.config/nvim/keybindings.vimrc

" Files command binds
nnoremap <Leader>fl :Files<CR>

" Lines command binds
nnoremap <Leader>la :Lines<CR>
nnoremap <Leader>lb :BLines<CR>

" Buffers command binds
nnoremap <Leader>bl :Buffers<CR>
nnoremap <Leader>bs :buffers<CR>

" Indent command binds
nnoremap <Leader>if gg=G
nnoremap <Leader>ib {=}

" History comamnd binds
nnoremap <Leader>hf :History<CR>
nnoremap <Leader>hc :History:<CR>
nnoremap <Leader>hs :History/<CR>

" Extra stuff for development
nnoremap <Leader>rs :IronRepl<CR>
nnoremap <Leader>term :botright vertical 80 split new <Bar> set wfw <Bar> call IronStartRepl('sh', 0, 1)
" nnoremap <Leader>config :tabe $MYVIMRC
" nnoremap <Leader>config :!ls $HOME/.config/nvim/<CR>:tabe $HOME/.config/nvim/
nnoremap <Leader>config :tabe $HOME/.config/nvim/
nnoremap <Leader>rconfig :source $MYVIMRC

" Clear window
nnoremap <Leader>cw :nohl<CR>

" Create files
nnoremap <Leader>cfh :e %:h/
nnoremap <Leader>cfn :e
nnoremap <Leader>cfd :call mkdir(expand('%:h'), 'p')<CR>

" Paste/Copy to/from Clipboard
nnoremap <Leader>ph "+p
nnoremap <Leader>P "+P
" Yield to the end of the line\"
nnoremap <Leader>Y "+yg_
nnoremap <Leader>y "+y
nnoremap <Leader>yy "+yy
nnoremap <Leader>yf gg"+yG

" Delete another windows binds
nnoremap <Leader>kn <C-W><C-W>:q<CR>1<C-G>

" To visual mode stuff
nnoremap V <C-v>

" Git stuffs
nnoremap <Leader>git :Magit<CR>

"---------------------------------------------
"            TERMINAL MODE BINDINGS
"---------------------------------------------
" tnoremap <Esc> <C-\><C-N>
tnoremap <C-v> <C-\><C-N>"+pA

"---------------------------------------------
"             INSERT MODE BINDINGS
"---------------------------------------------
" Configs from traditional vim
:inoremap ( ()<Esc>i
:inoremap { {}<Esc>i
:inoremap < <><Esc>i
:inoremap " ""<Esc>i
:inoremap [ []<Esc>i
:inoremap ' ''<Esc>i

" Paste stuff
:inoremap <C-v> <C-r>+

" Escape pra quando tiver dentro dos parenteses
:inoremap <C-l> <Esc>/[)}"'\]>]<CR>a

" Deoplete tab-complete
" inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

"---------------------------------------------
"            VISUAL MODE BINDINGS
"---------------------------------------------
" Pra poder colocar texto selecionado entre chaves
:vnoremap { <Esc>`>a}<Esc>`<i{<Esc>
:vnoremap ( <Esc>`>a)<Esc>`<i(<Esc>
:vnoremap " <Esc>`>a"<Esc>`<i"<Esc>
:vnoremap [ <Esc>`>a]<Esc>`<i[<Esc>
:vnoremap ' <Esc>`>a'<Esc>`<i'<Esc>

" Paste/Copy to/from Clipboard
vnoremap <Leader>y "+y
vnoremap <C-c> "+y
"vnoremap <Leader>p "+p
"vnoremap <Leader>P "+P

"---------------------------------------------
"       VALE A PENA DAR UMA OLHADA
"---------------------------------------------
" Nada mais por enquanto
