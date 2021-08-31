vim.fn.sign_define('LightBulbSign',
                   {text = 'A', texthl = 'LspDiagnosticsSignHint'})
vim.cmd[[
  augroup lightbulb_config
    autocmd!
    autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
  augroup END
]]
