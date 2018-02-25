autoload -U compinit
compinit

### envs ###
export LANG=ja_JP.UTF-8
export OUTPUT_CHARSET=utf-8
# export LV="-Ou8"
# export EDITOR="env TERM=xterm-256color emacsclient -t"
# export GREP_OPTIONS="--binary-files=without-match --color=auto"

### prompt ###
# ブランチ名を色付きで表示させるメソッド
function rprompt-git-current-branch {
  local branch_name st branch_status

  if [ ! -e  ".git" ]; then
    return
  fi
  branch_name=`git rev-parse --abbrev-ref HEAD 2> /dev/null`
  st=`git status 2> /dev/null`
  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
    branch_status="%F{green}"
  elif [[ -n `echo "$st" | grep "^Untracked files"` ]]; then
    branch_status="%F{red}?"
  elif [[ -n `echo "$st" | grep "^Changes not staged for commit"` ]]; then
    branch_status="%F{red}+"
  elif [[ -n `echo "$st" | grep "^Changes to be committed"` ]]; then
    branch_status="%F{yellow}!"
  elif [[ -n `echo "$st" | grep "^rebase in progress"` ]]; then
    echo "%F{red}!(no branch)"
    return
  else
    branch_status="%F{blue}"
  fi
  echo "${branch_status}[$branch_name]%f"
}

setopt prompt_subst
RPROMPT=''
case "${OSTYPE}" in
    darwin*)
        PROMPT='%F{cyan}%n:%f%F{green}%~%f:`rprompt-git-current-branch` $ '
#         PROMPT="%F{cyan}%n:%f%F{green}$(echo '\w' | sed -e "/^.\{30,\}$/s/^\(.\{15\}\).*\(.\{15\}\)$/\1...\2/")%f $ "
        ;;
    linux*)
        PROMPT='%F{cyan}%n@$(echo $HOSTNAME | cut -d . -f1):%f%F{green}%~%f:`rprompt-git-current-branch` $ '
        ;;
esac


### history ###
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
export PROMPT_COMMAND='history -a; history -r'

### history search ###
autoload history-search-end

#RPROMPT='[%d]'

### color ###
#[[ -f ~/.dir_colors ]] && eval $(dircolors -b ~/.dir_colors)
#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

if [ -f ~/.dir_colors ]; then
    if type dircolors > /dev/null 2>&1; then
        eval $(dircolors ~/.dir_colors)
    elif type gdircolors > /dev/null 2>&1; then
        eval $(gdircolors ~/.dir_colors)
    fi
fi

#setopt COMPLETE_IN_WORD

#HISTSIZE=50000

#alias ls='ls -G' # --color=auto'
#alias ll='ls -hl' # --color=auto'

case "${OSTYPE}" in
    darwin*)
        alias ls="ls -G"
        alias ll="ls -lhG"
        alias la="ls -lhaG"
        ;;
    linux*)
        alias ls='ls --color'
        alias ll='ls -lh --color'
        alias la='ls -lha --color'
        ;;
esac

export PATH=$HOME/local/bin:$PATH
export PATH=$PATH:$HOME/.cask/bin
# export PATH=$HOME/local/bin/gradle-2.14.1/bin:$PATH
# export PATH=$HOME/.rbenv/shims:$PATH
# export PATH=$PATH:/opt/yarn-[version]/bin
# export PATH=$PATH:$HOME/local/bin/platform-tools
# source ~/.git-completion.bash

alias grepc='grep -nr --color=always --with-filename'
alias less='less -qR'
# if [[ -x `which colordiff` ]]; then
#       alias diff='colordiff'
# fi
