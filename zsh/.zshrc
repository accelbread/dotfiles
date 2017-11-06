# Set $LS_COLORS
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

export EDITOR=vim

# zsh-git-prompt 
source /usr/lib/zsh-git-prompt/zshrc.sh
ZSH_THEME_GIT_PROMPT_PREFIX="["
ZSH_THEME_GIT_PROMPT_SUFFIX="] "
reset_color="\e[0;36m"

# prompt
modecolor=6
PROMPT='%F{6}$(git_super_status)%(?..%B%F{1}%? %b%f)%F{$modecolor}%1/❭ %f'

zle-keymap-select() {
  case "$KEYMAP" in
    "vicmd") modecolor=5;;
    *)       modecolor=6;;
  esac
  zle reset-prompt
}
zle -N zle-keymap-select

# Stay in previous mode
accept-line() { 
  prev_mode=$KEYMAP
  zle .accept-line
}
zle-line-init() {
  zle -K ${prev_mode:-viins}
}
zle -N accept-line
zle -N zle-line-init

# Lower delay in switching vi mode
export KEYTIMEOUT=1

# Set window title
precmd(){
    print -Pn "\e]0;[%n@%m] zsh: %~\a"
}
preexec(){
    print -Pn "\e]0;[%n@%m] $1\a"
}

# Traverse history matching part of line left of cursor
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey -a "k" history-beginning-search-backward
bindkey -a 'j' history-beginning-search-forward

# Vi mode: allow delete before position insert mode entered in
bindkey "^?" backward-delete-char
bindkey "^W" backward-kill-word
bindkey "^H" backward-delete-char
bindkey "^U" backward-kill-line

# Set tools to use colors
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias diff='diff --color=auto'

