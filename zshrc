export ZSH="/home/dustin/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

bindkey -v

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH=$PATH:~/.local/bin/ 
export PATH=$PATH:~/.local/bin/scripts
export PATH=$PATH:~/.local/bin/scripts/i3cmds
export PATH=$PATH:~/.nimble/bin/
export PATH=$PATH:~/.gem/ruby/2.7.0/bin/
export PATH=$PATH:~/Repos/CX16/EMU/

export EDITOR="nvim"

alias gis="git status"
alias gic="git commit"
alias gia="git add ."
alias gib="git checkout"
alias ls="lsd"
alias ll="lsd -lah"
alias cat="bat"

man() {
    env \
    LESS_TERMCAP_mb="$(printf "\e[1;31m")" \
    LESS_TERMCAP_md="$(printf "\e[1;31m")" \
    LESS_TERMCAP_me="$(printf "\e[0m")" \
    LESS_TERMCAP_se="$(printf "\e[0m")" \
    LESS_TERMCAP_so="$(printf "\e[1;44;33m")" \
    LESS_TERMCAP_ue="$(printf "\e[0m")" \
    LESS_TERMCAP_us="$(printf "\e[1;32m")" \
    man "${@}"
}

bindkey "^R" history-incremental-search-backward
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search

# opam configuration
test -r /home/dustin/.opam/opam-init/init.zsh && . /home/dustin/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

eval "$(starship init zsh)"

# if [ "$TMUX" = "" ]; then tmux; fi
