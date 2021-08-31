require'nvim-treesitter.configs'.setup{
  ensure_installed = {
    'json', 'jsonc', 'yaml', 'toml', 'bash', 'latex', 'html', 'javascript',
    'css', 'lua', 'python', 'c', 'cpp', 'rust', 'zig', 'comment', 'regex',
    'query'
  },
  highlight = {enable = true, disable = {'rust'}},
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = nil,
      node_incremental = ' i',
      scope_incremental = ' s',
      node_decremental = ' d'
    }
  },
  rainbow = {enable = true, extended_mode = true},
  textsubjects = {enable = true, keymaps = {['.'] = 'textsubjects-smart'}}
}
