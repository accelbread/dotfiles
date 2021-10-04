function e
    set -q argv[1]; or set argv[1] "."
    for f in $argv
        vterm_cmd find-file (realpath $f)
    end
end
