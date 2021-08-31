-- vim:sw=2
-- Reload this file on edit
vim.cmd[[
  augroup packer_config
    autocmd!
    autocmd BufWritePost */nvim/cfg/packer.lua lua load_config('packer')
  augroup END
]]

-- Install packer
local install_path = vim.fn.stdpath'data' .. '/site/pack/packer/opt/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.system{
    'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim',
    install_path
  }
end

-- Load packer
vim.cmd'packadd packer.nvim'

-- packer config
return require('packer').startup(function(use)
  use{
    'wbthomason/packer.nvim',
    cmd = 'PackerSync',
    config = [[load_config('packer')]]
  }
  use{
    'neovim/nvim-lspconfig',
    ft = {'rust', 'c', 'cpp', 'python'},
    config = [[load_config('nvim-lspconfig')]]
  }
  use{'ahmedkhalf/lsp-rooter.nvim', after = 'nvim-lspconfig'}
  use{
    'folke/trouble.nvim',
    after = 'nvim-lspconfig',
    config = [[load_config('trouble')]]
  }
  use{
    'kosayoda/nvim-lightbulb',
    after = 'nvim-lspconfig',
    config = [[load_config('nvim-lightbulb')]]
  }
  use{'stevearc/aerial.nvim', module = 'aerial'}
  use{
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = [[load_config('nvim-treesitter')]]
  }
  use'p00f/nvim-ts-rainbow'
  use{
    'mfussenegger/nvim-ts-hint-textobject',
    config = [[load_config('nvim-ts-hint-textobject')]]
  }
  use{'lewis6991/spellsitter.nvim', config = [[load_config('spellsitter')]]}
  use{'nvim-telescope/telescope.nvim', requires = {'nvim-lua/plenary.nvim'}}
  use'L3MON4D3/LuaSnip'
  use{
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp', 'saadparwaiz1/cmp_luasnip', 'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer', 'f3fora/cmp-spell'
    },
    config = [[load_config('nvim-cmp')]]
  }
  use{
    'ray-x/lsp_signature.nvim',
    after = 'nvim-lspconfig',
    config = [[load_config('lsp_signature')]]
  }
  use{'folke/which-key.nvim', config = [[load_config('which-key')]]}
  use'tpope/vim-repeat'
  use{'ggandor/lightspeed.nvim', after = 'which-key.nvim'}
  use{
    'lewis6991/gitsigns.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = [[load_config('gitsigns')]]
  }
  use{'folke/tokyonight.nvim', config = [[load_config('tokyonight')]]}
  use{
    'sindrets/diffview.nvim',
    cmd = 'DiffviewOpen',
    module = 'diffview',
    config = [[load_config('diffview')]]
  }
  use{
    'TimUntersberger/neogit',
    cmd = 'Neogit',
    requires = 'nvim-lua/plenary.nvim',
    config = [[load_config('neogit')]]
  }
  use{'hoob3rt/lualine.nvim', config = [[load_config('lualine')]]}
  use{'terrortylor/nvim-comment', config = [[load_config('nvim-comment')]]}
  use{
    'norcalli/nvim-colorizer.lua',
    cmd = 'ColorizerToggle',
    ft = {'css', 'html'},
    config = [[load_config('nvim-colorizer')]]
  }
  use'ConradIrwin/vim-bracketed-paste'
  use'kyazdani42/nvim-web-devicons'
  use'ziglang/zig.vim'
  use'lewis6991/impatient.nvim'
end)
