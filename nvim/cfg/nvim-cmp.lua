require'cmp'.setup{
  snippet = {expand = function(args) require'luasnip'.lsp_expand(args.body) end},
  mapping = {
    ['<C-p>'] = require'cmp'.mapping.select_prev_item(),
    ['<C-n>'] = require'cmp'.mapping.select_next_item(),
    ['<C-u>'] = require'cmp'.mapping.scroll_docs(-4),
    ['<C-d>'] = require'cmp'.mapping.scroll_docs(4),
    ['<C-Space>'] = require'cmp'.mapping.complete(),
    ['<C-e>'] = require'cmp'.mapping.close()
  },
  sources = {
    {name = 'nvim_lsp'}, {name = 'luasnip'}, {name = 'path'}, {name = 'buffer'},
    {name = 'spell'}
  }
}
