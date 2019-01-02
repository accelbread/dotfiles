set copyindent
set expandtab
set tabstop=4
set shiftwidth=4

set ignorecase
set smartcase

set hidden
set pastetoggle=<F2>
set showbreak=>\ 
set visualbell

let mapleader = " "

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

tnoremap <C-a> <C-\><C-n>

set termguicolors
set background=dark
set colorcolumn=80,100

source $HOME/.config/nvim/colorscheme/default.vim

hi EoLSpace ctermbg=238 guibg=#333333
match EoLSpace /\s\+$/

set grepprg=rg\ --vimgrep
autocmd BufEnter * silent! lcd %:p:h

