dircolors -b | source /dev/stdin

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch
unsetopt notify
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle :compinstall filename '/home/fate/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

PROMPT='%B%F{9}%(?..%? )%b%(!..%f%F{6})%1/>%f '

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias diff='diff --color=auto'

preexec(){
    print -Pn "\e]0;[%n@%m] $1\a"
}
precmd(){
    print -Pn "\e]0;[%n@%m] zsh: %~\a"
}

export EDITOR=vim
