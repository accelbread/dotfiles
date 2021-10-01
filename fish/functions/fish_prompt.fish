function fish_prompt
    set_color $fish_color_cwd
    echo -n (basename $PWD)
    set_color normal
    echo -n '❭ '
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
