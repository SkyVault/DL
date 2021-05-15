#!/bin/bash

[[ $- != *i* ]] && return

source "$HOME/.config/bash/config"
source "$HOME/.config/bash/term"
source "$HOME/.config/bash/aliases" 

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
