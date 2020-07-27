#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
eval $(ssh-agent)

rvm_path=/usr/local/rvm
[[ -s "/usr/local/rvm" ]] && source "/usr/local/rvm/scripts/rvm" # Load RVM into a shell session *as a function*
