local cmd = vim.cmd
local opt = vim.opt
local g = vim.g

opt.expandtab = true
opt.copyindent = true
opt.tabstop = 4
opt.shiftwidth = 4

opt.ignorecase = true
opt.smartcase = true
opt.hidden = true
opt.pastetoggle = '<F2>'
opt.inccommand = 'split'
opt.spelloptions = 'camel'
opt.equalalways = false
opt.joinspaces = false
opt.grepprg = 'rg --vimgrep'
opt.completeopt = {'menuone', 'noinsert', 'noselect'}
opt.shortmess = opt.shortmess + 'c'
opt.ttimeoutlen = 0
opt.timeoutlen = 200
opt.list = true
opt.lazyredraw = true
opt.listchars = 'trail:▒,tab:> |,nbsp:␣'
g.mapleader = ' '
g.maplocalleader = ' '
g.c_syntax_for_h = 1

opt.mouse = 'nv'
opt.visualbell = true
opt.showbreak = '> '
opt.termguicolors = true
opt.colorcolumn = {80, 100}

cmd[[
  augroup spell_enable
    autocmd!
    autocmd BufEnter * lua if(pcall(vim.treesitter.get_parser)) then vim.wo.spell = true end
    autocmd FileType mail,markdown setlocal spell
  augroup END
]]
