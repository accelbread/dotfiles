vim.g.tokyonight_style = 'night'
vim.g.tokyonight_italic_keywords = false
vim.g.tokyonight_lualine_bold = true
vim.g.tokyonight_sidebars = {'packer', 'Trouble', 'aerial'}
vim.cmd[[colorscheme tokyonight]]

local c = require'tokyonight/colors'.setup()
