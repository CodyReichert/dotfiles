# Path to your oh-my-zsh installation.

if [ ! -d "/home/cody/.oh-my-zsh" ]; then
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
    mv /home/cody/.zshrc.pre-oh-my-zsh /home/cody/.zshrc
fi

export ZSH=/home/cody/.oh-my-zsh
ZSH_THEME="steeef"
plugins=(git zshmarks emoji)

source $ZSH/oh-my-zsh.sh

setopt no_share_history

# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# export MANPATH="/usr/local/man:$MANPATH"

# Path
export PATH="$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.scripts/:$HOME/.qi/bin/:$HOME/.local/bin/:$HOME/workspace/CodyReichert/pgdevdb"

bindkey -v

fpath+=$HOME/.zsh/pure

autoload -U promptinit; promptinit
prompt pure

# Default editor
export EDITOR="emacs"
export ALTERNATE_EDITOR=""
export GIT_EDITOR=/bin/true
alias e='emacsclient -c'

# Default browser
export BROWSER="google-chrome-unstable"

# Aliases

alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l1='ls -1'
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

source $HOME/.local/bin/bashmarks.sh

autoload -Uz compinit
compinit

# Completion for kitty
# kitty + complete setup zsh | source /dev/stdin
