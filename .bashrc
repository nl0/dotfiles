PATH="$HOME/bin:$HOME/.gem/ruby/1.9.1/bin:$HOME/.gem/ruby/2.0.0/bin:$HOME/.gem/ruby/2.2.0/bin:$PATH"

# Check for an interactive session
[ -z "$PS1" ] && return

PS1='\[\e[0;32m\][\u@\H \[\e[0;33m\]\w\[\e[0;32m\]]\$\[\e[0m\] '

# aliases
alias diff='colordiff'
alias df='df -h'
alias du='du -ch'
alias du1='du -chd1'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias mkdir='mkdir -pv'
alias more='less'
alias ssh='TERM=$COLORTERM ssh'
alias cr='cmus-remote'
alias mc='PAGER=vim mc'
alias kc='eval $(keychain --eval --agents ssh -Q --quiet id_rsa)'
alias sys='sudo systemctl'

shopt -s autocd
#set -o vi

if [ -n "$DISPLAY" ]; then
  export BROWSER=firefox-developer
fi

export EDITOR=vim
