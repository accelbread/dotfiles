local lsp = require'lspconfig'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require'cmp_nvim_lsp'.update_capabilities(capabilities)

local on_attach = function(client) require'aerial'.on_attach(client) end

lsp.rls.setup{
  settings = {rust = {unstable_features = true, clippy_preference = 'on'}},
  on_attach = on_attach
}
lsp.zls.setup{}
lsp.clangd.setup{capabilities = capabilities}
lsp.pylsp.setup{}
