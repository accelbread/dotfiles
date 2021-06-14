local cmd = vim.cmd
local opt = vim.opt
local g   = vim.g

local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

opt.expandtab = true
opt.copyindent = true
opt.tabstop = 4
opt.shiftwidth = 4

opt.ignorecase = true
opt.smartcase = true
opt.hidden = true
opt.pastetoggle = '<F2>'
opt.inccommand = 'split'
opt.equalalways = false
opt.grepprg = 'rg --vimgrep'
opt.completeopt = {'menuone', 'noinsert', 'noselect'}
opt.shortmess = opt.shortmess + 'c'
opt.ttimeoutlen = 0
opt.list = true
opt.listchars = 'trail:▒,tab:> |,nbsp:␣'
g.mapleader = " "
g.maplocalleader = " "

opt.mouse = 'nv'
map('v', '<LeftRelease>', '"*ygv<LeftRelease>')

map('n', '<C-a>', '<nop>')
map('t', '<C-a>', '<C-\\><C-n>')

opt.visualbell = true
opt.showbreak = '> '
opt.termguicolors = true
opt.background = 'dark'
opt.colorcolumn = {80, 100}

cmd('autocmd FileType mail set spell')
