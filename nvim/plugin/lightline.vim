set noshowmode

let g:bufferline_echo = 0
let g:bufferline_active_buffer_left = ''
let g:bufferline_active_buffer_right = ''

"  \   'colorscheme': 'onedark',
let g:lightline = {
  \   'active': {
  \     'left': [ [ 'mode', 'paste' ], [ 'filename' ], [ 'bufferline' ] ],
  \     'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'gitbranch' ] ],
  \   },
  \   'component': {
  \     'bufferline': '%<%{LightLineBufferline()}',
  \   },
  \   'component_function': {
  \     'filename': 'LightlineFilename',
  \     'gitbranch': 'fugitive#head',
  \   }
  \ }

function! LightlineFilename()
  let filename = expand('%:t') !=# '' ? expand('%:t') : '[No Name]'
  let modified = &modified ? '+' : ''
  return filename . modified
endfunction

function! LightLineBufferline()
  call bufferline#refresh_status()
  let line = g:bufferline_status_info.before . g:bufferline_status_info.after
  let line = substitute(line, '\V\^ \*', '', '')
  let line = substitute(line, '   ', '  ', 'g')
  let line = substitute(line, '+  ', '+ ', 'g')
  return line
endfunction

