local wk = require('which-key')

local function t(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

wk.setup{plugins = {spelling = {enabled = true, suggestions = 50}}}

wk.register{
  gc = {'<Cmd>set operatorfunc=CommentOperator<CR>g@', 'Comment'},
  s = {'<Plug>Lightspeed_s', 'Lightspeed forwards'},
  S = {'<Plug>Lightspeed_S', 'Lightspeed backwards'},
  ['<C-L>'] = {'Reset screen, highlight, diff'},
  ['<M-a>'] = {'<C-a>', 'Increment'},
  ['<C-a>'] = {'<nop>', 'which_key_ignore'},
  ['<F28>'] = {'which_key_ignore'}
}

wk.register({['<F28>'] = {'which_key_ignore'}}, {mode = 'i'})

wk.register({
  gc = {':<C-u>call CommentOperator(visualmode())<CR>', 'Comment'},
  ['<LeftRelease>'] = {'"*ygv<LeftRelease>', 'which_key_ignore'},
  ['<F28>'] = {'which_key_ignore'}
}, {mode = 'v'})

wk.register({
  s = {'<Plug>Lightspeed_s', 'Lightspeed forwards'},
  S = {'<Plug>Lightspeed_S', 'Lightspeed backwards'},
  z = {'<Esc>', 'which_key_ignore'},
  Z = {'<Esc>', 'which_key_ignore'}
}, {mode = 'o'})

wk.register({['<C-a>'] = {t('<C-\\><C-n>'), 'Escape'}}, {mode = 't'})
