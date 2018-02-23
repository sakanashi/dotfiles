autoload -U compinit
compinit

setopt COMPLETE_IN_WORD

HISTSIZE=50000

alias ls='ls -G'
alias ll='ls -hl'
test -r ~/.zshrc && . ~/.zshrc
eval $(thefuck --alias)
export PATH=$HOME/local/bin:$PATH
export PATH=$PATH:$HOME/.cask/bin
# export PATH=$HOME/local/bin/gradle-2.14.1/bin:$PATH
# export PATH=$HOME/.rbenv/shims:$PATH
# export PATH=$PATH:/opt/yarn-[version]/bin
# export PATH=$PATH:$HOME/local/bin/platform-tools
# source ~/.git-completion.bash

alias grepc='grep -nr --color=always --with-filename'
alias less='less -qR'
if [[ -x `which colordiff` ]]; then
      alias diff='colordiff'
fi
