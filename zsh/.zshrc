# Path to your oh-my-zsh installation.

if [ ! -d "/home/cody/.oh-my-zsh" ]; then
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
    mv /home/cody/.zshrc.pre-oh-my-zsh /home/cody/.zshrc
fi

export ZSH=/home/cody/.oh-my-zsh
ZSH_THEME="steeef"
plugins=(git zshmarks cabal python systemd)

source $ZSH/oh-my-zsh.sh

# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# export MANPATH="/usr/local/man:$MANPATH"

export PATH="/home/cody/.bin/ghc-7.8.4/bin:/home/cody/.scripts/:/opt/ghc/7.8.3/bin:/home/cody/.cabal/bin:/home/cody/.bin/ghc-7.8.4/bin:/home/cody/.scripts/:/opt/ghc/7.8.3/bin:/home/cody/.cabal/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/cody/.gem/ruby/2.2.0/bin:/home/cody/.node/bin:/home/cody/.gem/ruby/2.2.0/bin:/home/cody/.node/bin"

bindkey -v

PROMPT=$'
%n%{$reset_color%}@%m%{$reset_color%}: %{$limegreen%}%~%{$reset_color%} $vcs_info_msg_0_$(virtualenv_info)%{$reset_color%}
%{$purple%}λ%{$reset_color%} '

# ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
# alias l='ls -CF'
alias l1='ls -1'

export ALTERNATE_EDITOR="emacs -nw"
alias e='emacsclient -t'
export EDITOR="e"

  # emacs
alias emacs='emacs -nw'
alias emcas='emacs -nw'
  # pacman
alias pacman='sudo pacman'
alias pacdep='sudo pacman -Si'
  # git
alias gpr='git pull --rebase'
alias stashsave='git stash save'
alias stashpop='git stash pop'
alias sgpr='stashsave && gpr origin master && stashpop'
  # cabal
alias cabaldeps='cabal sandbox init; cabal install --only-dep -j'
  # other
alias tree='tree -I ".git"'
alias chromium='chromium-browser'
alias untar='tar -zxvf'
alias myxrandr='xrandr --output VGA-0 --auto --right-of HDMI-1'
alias pgdevdb='bash /home/cody/.scripts/pgdevdb'
alias beetimport='beet import -i /media/cody/ASPEN/music/untagged'
alias aspen='mount /dev/sdc1'

source $HOME/.local/bin/bashmarks.sh
