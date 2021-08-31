require'gitsigns'.setup{
  keymaps = {
    noremap = true,
    ['n ]c'] = {
      expr = true,
      '&diff ? \']c\' : \'<cmd>lua require"gitsigns".next_hunk()<CR>\''
    },
    ['n [c'] = {
      expr = true,
      '&diff ? \'[c\' : \'<cmd>lua require"gitsigns".prev_hunk()<CR>\''
    },
    ['o ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',

    ['n ghs'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['v ghs'] = '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n ghu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n ghr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['v ghr'] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n ghR'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n ghp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n ghb'] = '<cmd>lua require"gitsigns".blame_line(true)<CR>',
    ['n ghS'] = '<cmd>lua require"gitsigns".stage_buffer()<CR>',
    ['n ghU'] = '<cmd>lua require"gitsigns".reset_buffer_index()<CR>'
  },
  word_diff = true
}
