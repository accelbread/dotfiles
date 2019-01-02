packadd! onedark.vim
let g:onedark_terminal_italics = 1

augroup onedarkoverrides
  autocmd!
  let s:white = { "gui": "#ABB2BF", "cterm": "145", "cterm16" : "7" }
  autocmd ColorScheme * call onedark#extend_highlight("ColorColumn", { "bg": { "cterm": 234, "gui": "#121212" } })
  autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white })
augroup END

colorscheme onedark
