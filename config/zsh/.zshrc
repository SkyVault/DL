# Load autocompletions
autoload -U compinit; compinit

_comp_options+=(globdots)
source "$ZDOTDIR/autocomplete.zsh"

# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# Zsh history of directories
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT

alias d="dirs -v"
for index ({1..9}) alias "$index"="cd +${index}"; unset index

# Vi mode
bindkey -v
export KEYTIMEOUT=1

## Vi mode for history navigation 
zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

bindkey '^P' vi-up-line-or-history
bindkey '^N' vi-down-line-or-history

## Makes the cursor act like it does in vim
cursor_mode() {
    # See https://ttssh2.osdn.jp/manual/4/en/usage/tips/vim.html for cursor shapes
    cursor_block='\e[2 q'
    cursor_beam='\e[6 q'

    function zle-keymap-select {
        if [[ ${KEYMAP} == vicmd ]] ||
            [[ $1 = 'block' ]]; then
            echo -ne $cursor_block
        elif [[ ${KEYMAP} == main ]] ||
            [[ ${KEYMAP} == viins ]] ||
            [[ ${KEYMAP} = '' ]] ||
            [[ $1 = 'beam' ]]; then
            echo -ne $cursor_beam
        fi
    }

    zle-line-init() { echo -ne $cursor_beam }

    zle -N zle-keymap-select
    zle -N zle-line-init
} 
cursor_mode

## Edit commands with vim
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# Zsh suggestions
source "$ZDOTDIR/zsh-autosuggestions/zsh-autosuggestions.zsh"
