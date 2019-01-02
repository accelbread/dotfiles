let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls'],
    \ }
let g:LanguageClient_rootMarkers = {
    \ 'rust': ['Cargo.toml'],
    \ }
let g:LanguageClient_completionPreferTextEdit = 1
let g:LanguageClient_settingsPath = expand("<sfile>:p:h") . "/lsp_config.json"
set completeopt=noinsert,menuone,noselect

function LC_maps()
  if has_key(g:LanguageClient_serverCommands, &filetype)
    call ncm2#enable_for_buffer()
    set formatexpr=LanguageClient#textDocument_rangeFormatting_sync()
    nnoremap <buffer> <silent> K :call LanguageClient#textDocument_hover()<CR>
    nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
    nnoremap <buffer> <silent> <F5> :call LanguageClient_contextMenu()<CR>
  endif
endfunction

autocmd FileType * call LC_maps()
