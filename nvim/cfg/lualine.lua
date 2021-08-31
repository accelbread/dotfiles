require'lualine'.setup{
  options = {
    theme = 'tokyonight',
    icons_enabled = false,
    component_separators = '',
    section_separators = ''
  },
  sections = {lualine_c = {{'filename', path = 1}}}
}
