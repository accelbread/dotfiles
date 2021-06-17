-- Install packer
local install_path = vim.fn.stdpath'data'..'/site/pack/packer/opt/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.system{'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path}
end

-- Load packer
vim.cmd'packadd packer.nvim'

-- packer config
return require('packer').startup(function(use)
  use {'wbthomason/packer.nvim', config = [[dofile(vim.fn.stdpath'config'..'/plugins.lua')]], cmd = 'PackerSync'}
  use {'neovim/nvim-lspconfig',
    ft = {'rust','c','cpp','python'},
    config = function()
      local lsp = require 'lspconfig'
      lsp.rls.setup{
        settings = {
          rust = {
            unstable_features = true,
            clippy_preference = 'on'}}}
      lsp.clangd.setup{}
      lsp.pyls.setup{}
    end}
  use {'ahmedkhalf/lsp-rooter.nvim', after = 'nvim-lspconfig'}
  use {'folke/trouble.nvim', after = 'nvim-lspconfig',
    config = function()
      require'trouble'.setup {
        icons = false,
        auto_preview = false,
        use_lsp_diagnostic_signs = true}
    end}
  use {'kosayoda/nvim-lightbulb', after = 'nvim-lspconfig',
    config = function()
      vim.fn.sign_define('LightBulbSign', {text = "A", texthl = 'LspDiagnosticsSignHint'})
      vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]
    end}
  use {'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require'nvim-treesitter.configs'.setup{
        ensure_installed = {"json", "jsonc", "yaml", "toml", "bash", "latex", "html", "javascript",
            "css", "lua", "python", "c", "cpp", "rust", "zig", "comment", "regex"},
        highlight = {enable = true},
        rainbow = {enable = true, extended_mode = true}}
    end}
  use {'p00f/nvim-ts-rainbow', after = 'nvim-treesitter'}
  use {'nvim-telescope/telescope.nvim', requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}}
  use {'hrsh7th/nvim-compe',
    config = function()
      require'compe'.setup {
        source = {
          path = true,
          buffer = true,
          nvim_lsp = true}}
    end}
  use {'ray-x/lsp_signature.nvim', after = 'nvim-lspconfig',
    config = function()
      require'lsp_signature'.on_attach{hint_enable = false}
    end}
  use 'tversteeg/registers.nvim'
  use 'phaazon/hop.nvim'
  use {'lewis6991/gitsigns.nvim', requires = 'nvim-lua/plenary.nvim',
    config = function()
      require'gitsigns'.setup()
    end}
  use {'folke/tokyonight.nvim',
    config = function()
      vim.g.tokyonight_style = 'night'
      vim.g.tokyonight_transparent = true
      vim.cmd[[colorscheme tokyonight]]
      dofile(vim.fn.stdpath'config'..'/colors.lua')
    end}
  use {'sindrets/diffview.nvim', cmd = 'DiffviewOpen', module = 'diffview',
    config = function()
      require'diffview'.setup()
    end}
  use {'TimUntersberger/neogit', cmd = 'Neogit', requires = 'nvim-lua/plenary.nvim',
    config = function()
      require'diffview'
      require'neogit'.setup{
        disable_commit_confirmation = true,
        integrations = {diffview = true}}
    end}
  use {'hoob3rt/lualine.nvim',
    config = function()
      require'lualine'.setup {
        options = {
          theme = 'tokyonight',
          icons_enabled = false,
          component_separators = '',
          section_separators = ''},
        sections = {
          lualine_c = {{'filename', path = 1}},
          lualine_x = {'encoding', 'fileformat'},
          lualine_y = {'filetype'},
          lualine_z = {'location', 'progress'}}}
    end}
  use {'norcalli/nvim-colorizer.lua',
    cmd = 'ColorizerToggle',
    ft = {'css', 'html'},
    config = function()
      require 'colorizer'.setup({'css', 'html'}, {RRGGBBAA = true, css = true})
    end}
  use 'ziglang/zig.vim'
end)
