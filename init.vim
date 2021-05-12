set nocompatible
filetype off

call plug#begin()
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim' 
Plug 'neoclide/coc.nvim', {'branch': 'release'} 
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript' 
Plug 'joshdick/onedark.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'https://github.com/Mizux/vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'
Plug 'NLKNguyen/papercolor-theme' 
call plug#end()

" UI

"" Enable theming support
" if (has("termguicolors"))
"  set termguicolors
"  hi LineNr ctermbg=NONE guibg=NONE
" endif

syntax enable

set t_Co=256
colorscheme PaperColor

let mapleader=","

"" Saving
nnoremap <leader>w :w<CR>

"" Emacs like splits
map <C-x>2 :split<CR>
map <C-x>3 :vsplit<CR>

"" Tabs
map <C-x>n :tabnext<CR>
map <C-x>c :tabnew<CR>
map <C-x>N :tabprevious<CR>
map <C-x>x :tabclose<CR>

" NerdTree Config

"" Auto close nvim if NERDTree is the only thing left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:NERDTreeShowHidden = 1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeIgnore = []
let g:NERDTreeStatusline = '' 

nnoremap <silent> <C-n> :NERDTreeToggle<CR>

" FZF Config
nnoremap <C-x>p :FZF<CR>
nnoremap <C-x>b :Buffers<CR>
nnoremap <C-x>/ :Ag<CR>

let $FZF_DEFAULT_COMMAND = 'ag -g ""' 

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \} 

" Coc Config
let g:coc_global_extensions = ['coc-emmet', 'coc-css', 'coc-html', 'coc-json', 'coc-prettier', 'coc-tsserver', 'coc-pairs', 'coc-vimlsp'] 

nnoremap <C-]> :call CocActionAsync('jumpDefinition')<CR>

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references) 

" Status line
set laststatus=2 
set noshowmode "Removes the --INSERT from the command menu
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ } 

" Random editor config 
set shiftwidth=2
set tabstop=2
set autoindent
set expandtab
set backspace=indent,eol,start
set path=.,,**
set smartindent
set smartcase
set showcmd
set number relativenumber
set nu rnu
set cursorline
set hlsearch
set mouse=a
set nobackup
set nowritebackup
set encoding=utf-8
set updatetime=300
set shortmess+=c

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <c-space> coc#refresh()

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>" 

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction
