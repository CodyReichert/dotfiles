#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History
HISTCONTROL=ignoreboth
HISTSIZE=100000
HISTFILESIZE=100000
shopt -s histappend
shopt -s histreedit
shopt -s histverify
PROMPT_COMMAND="history -a; $PROMP_COMMAND"

shopt -s autocd
shopt -s checkwinsize

source ~/.bash_git

export GIT_PS1_SHOWDIRTYSTATE=1

export PS1="\[\e[00;37m\]\n\u@\h \w\[\033[32m\]\$(__git_ps1)\n\[\e[0m\]\[\e[00;31m\]λ \[\e[0m\]"


if [ "$PS1" ];
then
  complete -cf sudo
fi

if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

# ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
# alias l='ls -CF'
alias l1='ls -1'

# Default editor
export EDITOR="emacs"
export ALTERNATE_EDITOR=""
export GIT_EDITOR=/bin/true
alias e='emacsclient -c'

# Default browser
export BROWSER="google-chrome-beta"


# pacman
alias pacman='sudo pacman'
alias pacdep='sudo pacman -Si'
alias packer='apacman'
alias y='yaourt'
# git
alias gmm='GIT_EDITOR=/bin/true git merge master'
alias gpr='git pull --rebase'
alias stashsave='git stash save'
alias stashpop='git stash pop'
alias sgpr='stashsave && gpr origin master && stashpop'
# cabal
alias cabaldeps='cabal sandbox init; cabal install --only-dep -j'
# other
alias tree='tree -I ".git"'
alias untar='tar -zxvf'
alias myxrandr='xrandr --output VGA-1 --auto --left-of HDMI-1'
alias beetimport='beet import -i /media/cody/ASPEN/music/untagged'
alias aspen='mount /dev/sdc1'
alias winpid="xprop _NET_WM_PID | cut -d' ' -f3"

# applications
alias Shift='~/apps/shift/Shift --no-sandbox'

# Path
PATH=$PATH:$HOME/.bin
PATH=$HOME/.local/bin:$PATH
PATH=$HOME/.scripts/:$PATH
PATH=$HOME/.qi/bin/:$PATH
PATH=$HOME/.local/bin/:$PATH
PATH=$HOME/apps/shift/:$PATH
PATH=$PATH:$HOME/.gem/ruby/2.5.0/bin

export LC_ALL="en_US.UTF-8"

set -o vi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

function retsmd() {
    docker kill retsmd
    docker run --detach --rm --net=host -e 8080:8080 --name=retsmd simplyrets/retsmd:latest
}
