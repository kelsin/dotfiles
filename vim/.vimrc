"  _   __       _       _
" | | / /      | |     (_)
" | |/ /   ___ | | ___  _  _ __
" |    \  / _ \| |/ __|| || '_ \
" | |\  \|  __/| |\__ \| || | | |
" \_| \_/ \___||_||___/|_||_| |_|
"
" .vimrc file

" Use Vim defaults instead of trying for Vi compatibility
set nocompatible

" Plug {{{
" Vim Plug: https://github.com/junegunn/vim-plug

" Install Plug if needed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

" Plugins {{{
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-shell'

Plug 'editorconfig/editorconfig-vim'
Plug 'airblade/vim-rooter'
Plug 'scrooloose/nerdtree'
Plug 'bronson/vim-trailing-whitespace'
Plug 'mattn/gist-vim'
Plug 'ap/vim-css-color'
Plug 'rizzatti/dash.vim'
Plug 'w0rp/ale'

" Syntax
Plug 'chrisbra/csv.vim'
Plug 'tpope/vim-markdown'
Plug 'timcharper/textile.vim'
Plug 'evanmiller/nginx-vim-syntax'

Plug 'digitaltoad/vim-pug'
Plug 'groenewege/vim-less'
Plug 'pangloss/vim-javascript'
Plug 'moll/vim-node'

Plug 'guns/vim-clojure-static'
Plug 'jimenezrick/vimerl'
Plug 'jnwhiteh/vim-golang'
Plug 'kchmck/vim-coffee-script'
Plug 'mustache/vim-mustache-handlebars'
Plug 'wlangstroth/vim-haskell'

Plug 'vim-ruby/vim-ruby'
Plug 'slim-template/vim-slim'
Plug 'skwp/vim-rspec'
Plug 'tpope/vim-haml'

" Style
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons'
" }}}

call plug#end()
" }}}

if filereadable($HOME.'/blizzard/src/blizzard-colors/vim/blizzard.vim')
  so ~/blizzard/src/blizzard-colors/vim/blizzard.vim
endif

" Basics {{{
" Read the first 5 lines for modelines on file opening
set modeline
set modelines=5

" Don't format text automatically
set formatoptions-=t

" Set .viminfo as the viminfo file (the rest of this line is default)
set viminfo='1000,<500,s100,f1,h,n~/.viminfo

" Default vim completion, will edit once I'm used to default
set wildmode=full

" Ignore some folders
set wildignore=*/overlays/**,*/.idea/**,*/build/**,*/target/**,*.o,*.obj,*.class,*.swp,*.swo

" Indent C-style functions correction
set cino+=(0

" Allow files to be loaded in background
set hidden

" Do not wrap lines in the window
set nowrap

" Show line numbers
set number

" Show relative line numbers
set relativenumber

" Set default textwidth and add a column showing this
set textwidth=80
if exists('+colorcolumn')
  set colorcolumn=81
endif

" Allow setting xterm titles
set title

" Large History
set undolevels=1000

" No beeping or flashing of any kind
set visualbell
set noerrorbells
set vb t_vb=
if has('autocmd')
  autocmd GUIEnter * set visualbell t_vb=
endif

" Show current command in the bottom right of the screen
set showcmd

" Jump to matching ([{ when inserting )]}
set showmatch

" When using search and other patterns act like Emacs
" If pattern is fully lowercase, ignore case, otherwise match with it
set ignorecase
set smartcase

" Hilight search matches in the file
set hlsearch

" Backups while working, but not after
set nobackup
set writebackup

" Hide the mouse while typing text
set mousehide

" No tabs
set showtabline=0
" }}}

" Tabs {{{
" Default to indents of 2 spaces
set tabstop=2
set softtabstop=2
set shiftwidth=2

" Prefer spaces instead of tabs
set expandtab

" Round to multiples of shiftwidth when using > and <
set shiftround

" Edit some defaults for java and javascript files
if has('autocmd')
  autocmd Filetype javascript setlocal ts=4 sts=4 sw=4   expandtab
  autocmd Filetype java       setlocal ts=4 sts=4 sw=4 noexpandtab
  autocmd Filetype xml        setlocal ts=2 sts=2 sw=2 noexpandtab
  autocmd Filetype gitconfig  setlocal ts=4 sts=4 sw=4   expandtab
endif

" Show tabs, trailing whitespace, and nbsp's
set list listchars=tab:→\ ,trail:∙,nbsp:␣,extends:>,precedes:<
autocmd FileType nerdtree setlocal nolist
" }}}

" Tags {{{
" Tags filenames are relative to the directory they are in
set tagrelative

" Tags files should be sorted for speed
set tagbsearch
" }}}

" Functions {{{
function! <SID>StripTrailingWhitespaces()
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  %s/\s\+$//e
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction
" }}}

" Bindings {{{
" Easier : and leader
nnoremap ; :
vnoremap ; :

" Make Y behave like D and C
nmap Y y$

" No more forgetting to sudo!
cmap w!! w !sudo tee % >/dev/null

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>ez :e ~/.zshrc<CR>
nmap <silent> <leader>et :e ~/.tmux.conf<CR>

" Format paragraph quickly
vmap Q gq
nmap Q gqap

" Clear search
nnoremap <leader><space> :noh<cr>

" Bad arrow keys! Make sure I can't use them in any mode
noremap  <up>    <nop>
vnoremap <up>    <nop>
noremap  <down>  <nop>
vnoremap <down>  <nop>
noremap  <left>  <nop>
vnoremap <right> <nop>
vnoremap <left>  <nop>
noremap  <right> <nop>
" B-A-<start>

" Split window movement, these are normally just the same as hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Open split window
nnoremap <leader>w <C-w>v<C-w>l

" Close split window
nnoremap <leader>k :bd<CR>

" Bindings for emacs keys in insert mode
inoremap <C-e> <esc>A
inoremap <C-a> <esc>I

" Make holding shift not a big deal
command! -bar -bang Q q<bang>
command! -bar -bang W w<bang>
command! -bar -bang WQ wq<bang>

" NERDTree Bindings
noremap <leader>n :NERDTreeToggle<CR>

" Remap F1 to use Dash to lookup from the cursor
inoremap <F1> :Dash<CR>
nnoremap <F1> :Dash<CR>
vnoremap <F1> :Dash<CR>

" Paste toggle
set pastetoggle=<F2>

" Java Mappings
if has("autocmd")
  autocmd FileType java nnoremap <F3> :ImportsOrganize<CR>
  autocmd FileType java nnoremap <F4> :ImportsFind<CR>
  autocmd FileType java nnoremap <F5> :ImportsOpenJavadoc<CR>
endif

" Cleaning Whitespace
nnoremap <silent> <F8> :call <SID>StripTrailingWhitespaces()<CR>
" }}}

" Autocmd {{{
" Jump to the last position when reopening a file
if has('autocmd')
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

  " Show relative numbers only when focusing the current window in normal mode
  autocmd WinEnter    * setlocal number   relativenumber
  autocmd WinLeave    * setlocal number norelativenumber
  autocmd FocusGained * setlocal number   relativenumber
  autocmd FocusLost   * setlocal number norelativenumber
  autocmd InsertEnter * setlocal number norelativenumber
  autocmd InsertLeave * setlocal number   relativenumber

  " Use markers to fold in vimrc
  autocmd FileType vim setlocal foldmethod=marker

  " No list characters in Quickfix buffer
  autocmd BufWinEnter Quickfix setlocal nolist

  " Read .classtags as a dict in java files
  autocmd FileType java setlocal dict+=.classtags
  autocmd FileType java setlocal makeprg=mvn\ compile\ -q
  autocmd FileType java setlocal errorformat=\[ERROR]\ %f:[%l\\,%v]\ %m
endif
" }}}

" Plugins {{{
" Airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme='murmur'
let g:airline#extensions#ale#enabled = 1
" }}}

" Signify {{{
" We want Signify to use git and svn checking in that order
let g:signify_vcs_list = [ 'git', 'svn' ]

" Update signify on buffer enter
let g:signify_update_on_bufenter = 1

" Update signify on focus gained
let g:signify_update_on_focusgained = 1

" No stupid colors in signcolumn
highlight clear SignColumn
" }}}

" NERDTree {{{
" NERDTree no show hidden by default
let NERDTreeShowHidden = 0

" Don't let NERDTree cover up Netrw
let NERDTreeHijackNetrw = 0

" Show bookmarks
let NERDTreeShowBookmarks = 1

" Close after opening a file
let NERDTreeQuitOnOpen=1

" Use arrows in NERDTree
if has("gui_running")
  let NERDTreeDirArrows = 1
endif

" Open single directories automatically
let NERDTreeCasadeOpenSingleChildDir = 1

" Ignore some files in NERDTree
let NERDTreeIgnore = ['^\.vagrant$', '^\.vagrant$', '^\.bundle$', '^\.bzr$',
      \'^\.git$', '^\.hg$', '^\.sass-cache$', '^\.svn$', '^\.$', '^\.\.$',
      \'^Thumbs\.db$', '\.sw[op]$']
" }}}

" Editorconfig {{{
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
" }}}

" Trailing Whitepace {{{
let loaded_trailing_whitespace_plugin = 1
" }}}
" }}}

" Gui {{{
" Gorgeous fullscreen with transparency on MacOSX!
if has("fullscreen")
  set fuopt=maxvert,maxhorz
endif
if has("transparency")
  set transp=5
endif

" No gui elements (toolbars, menus, tabs, scrollbars)
set go=
" }}}

" Fonts {{{
set guifont=Input\ Mono:h16
" }}}
