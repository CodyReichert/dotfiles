#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History
HISTCONTROL=ignoreboth
HISTSIZE=100000
HISTFILESIZE=100000
PROMPT_COMMAND="history -a; $PROMP_COMMAND"

shopt -s histappend
shopt -s histreedit
shopt -s histverify
shopt -s autocd
shopt -s checkwinsize

source ~/.bash_git

export GIT_PS1_SHOWDIRTYSTATE=1

# Set the prompt style to:
#
#  cody@desktop ~/dir (master *)
#  λ
export PS1="\[\e[00;37m\]\n\u@\h \w\[\033[32m\]\$(__git_ps1)\n\[\e[0m\]\[\e[00;31m\]λ \[\e[0m\]"

if [ "$PS1" ];
then
  complete -cf sudo
fi

if [ -f /usr/share/bash-completion/bash_completion ]; then
  . /usr/share/bash-completion/bash_completion
fi

# Default editor
export EDITOR="emacs"
export ALTERNATE_EDITOR=""
export GIT_EDITOR=/bin/true
alias e='emacsclient -c'

# Default browser
export BROWSER="google-chrome-unstable"

# ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l1='ls -1'

# utility aliases
alias c='clear'
alias grep='grep -i'
alias pacman='sudo pacman'
alias aura='sudo aura --unsuppress'
alias g='git'
alias gl='git lo -n 20'
alias gll='git l'
alias glo='git log'
alias gd='git diff'
alias gdc='git diff --cached'
alias gp='git push -u'
alias gpf='git push -u --force-with-lease'
alias gs='git status'
alias gc='git commit -m'
alias gcb='git checkout -b'
alias gss='git stash push'
alias gsp='git stash pop'
alias gsl='git stash list'
alias tree='tree -I ".git"'
alias untar='tar -zxvf'
alias winpid="xprop _NET_WM_PID | cut -d' ' -f3"
alias dc='docker-compose'
alias mjpg_start='mjpg_streamer -i "input_uvc.so -n -r VGA -f 5 -q 50" -o "output_http.so -w ./www -p 5001" &'

# Path
PATH=$PATH:$HOME/.bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.scripts/
PATH=$PATH:$HOME/.qi/bin/
PATH=$PATH:$HOME/.local/bin/
PATH=$PATH:$HOME/apps/shift/
PATH=$PATH:$HOME/workspace/CodyReichert/pgdevdb

export LC_ALL="en_US.UTF-8"

set -o vi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Automatically load ssh-agent on login
if [ ! -S ~/.ssh/ssh_auth_sock ]; then
  eval `ssh-agent`
  ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock
ssh-add -l > /dev/null || ssh-add

###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

if type complete &>/dev/null; then
  _npm_completion () {
    local words cword
    if type _get_comp_words_by_ref &>/dev/null; then
      _get_comp_words_by_ref -n = -n @ -n : -w words -i cword
    else
      cword="$COMP_CWORD"
      words=("${COMP_WORDS[@]}")
    fi

    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${words[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
    if type __ltrim_colon_completions &>/dev/null; then
      __ltrim_colon_completions "${words[cword]}"
    fi
  }
  complete -o default -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    local si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
fi

zsh
