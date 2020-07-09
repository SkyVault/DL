export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH="/home/dustin/.oh-my-zsh"

ZSH_THEME="robbyrussell"

# ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

export ARCHFLAGS="-arch x86_64"

source ~/.cache/wal/colors-tty.sh

alias zshconfig="mate ~/.zshrc"
alias ohmyzsh="mate ~/.oh-my-zsh"
alias fm="~/.config/vifm/scripts/vifmrun"
alias yt2mp3="youtube-dl -4 -f bestaudio --extract-audio --audio-format mp3 --audio-quality 0"
alias gw3m="w3m -4 google.com"
alias web="w3m -4 'https://sjmulder.nl/en/textonly.html'"
alias v="nvim"
alias vi="nvim"
alias vim="nvim"
alias sbcl="rlwrap sbcl"
alias lisp="rlwrap sbcl"

alias edwm="cd $HOME/.repos/DL/WM/dwm/"

bindkey -v
bindkey "^R" history-incremental-search-backward
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search

# PATH
export PATH=$PATH:/home/dustin/.yarn/bin/
export PATH=$PATH:~/.scripts/
export PATH=$PATH:~/.config/scripts/
export PATH=$PATH:~/.local/bin/
export PATH=$PATH:$HOME/.cargo/bin/
export PATH=$PATH:$HOME/Android/Sdk/tools/bin
export PATH=$PATH:$HOME/Android/Sdk/build-tools/29.0.2
export PATH=$PATH:$HOME/.nimble/bin

export DISCORD_BOT_TOKEN="Njg2MDM4NzI2Mjk5NzQ2NTI4.XmUdDw.B3SPLqCrmpd2W8HXpAbHh8iJvoY"
export DISCORD_SERVER_NAME="458330762538909708"

# opam configuration
test -r /home/dustin/.opam/opam-init/init.zsh && . /home/dustin/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
