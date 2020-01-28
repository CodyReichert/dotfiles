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

if [ -f /usr/share/bash-completion/bash_completion ]; then
  . /usr/share/bash-completion/bash_completion
fi

# Default editor
export EDITOR="emacs"
export ALTERNATE_EDITOR=""
export GIT_EDITOR=/bin/true
alias e='emacsclient -c'

# Default browser
export BROWSER="google-chrome-beta"

# ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l1='ls -1'

# utility aliases
alias c='clear'

# pacman
alias pacman='sudo pacman'
alias pacdep='sudo pacman -Si'
alias packer='apacman'
alias y='yaourt'

# aura
alias aura='sudo aura --unsuppress'

# git
alias gpr='git pull --rebase'
alias stashsave='git stash save'
alias stashpop='git stash pop'
alias sgpr='stashsave && gpr origin master && stashpop'

# other
alias tree='tree -I ".git"'
alias untar='tar -zxvf'
alias myxrandr='xrandr --output VGA-1 --auto --left-of HDMI-1'
alias beetimport='beet import -i /media/cody/ASPEN/music/untagged'
alias aspen='mount /dev/sdc1'
alias winpid="xprop _NET_WM_PID | cut -d' ' -f3"

# applications
alias dc='docker-compose'
alias cura='~/apps/cura/cura-4.4.1.AppImage &'
alias mjpg_start='mjpg_streamer -i "input_uvc.so -n -r VGA -f 5 -q 50" -o "output_http.so -w ./www -p 5001" &'

# Path
PATH=$PATH:$HOME/.bin
PATH=$HOME/.local/bin:$PATH
PATH=$HOME/.scripts/:$PATH
PATH=$HOME/.qi/bin/:$PATH
PATH=$HOME/.local/bin/:$PATH
PATH=$HOME/apps/shift/:$PATH
PATH=$HOME/apps/cura/:$PATH
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
export PATH=/home/cody/apps/shift/:/home/cody/.local/bin/:/home/cody/.qi/bin/:/home/cody/.scripts/:/home/cody/.local/bin:/home/cody/.nvm/versions/node/v9.7.1/bin:/home/cody/apps/shift/:/home/cody/.local/bin/:/home/cody/.qi/bin/:/home/cody/.scripts/:/home/cody/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/cody/.bin:/home/cody/.gem/ruby/2.5.0/bin:/home/cody/.bin:/home/cody/.gem/ruby/2.5.0/bin:/home/cody/workspace/CodyReichert/pgdevdb

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
fi
