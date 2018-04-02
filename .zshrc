autoload -U compinit
compinit
autoload -U colors; colors

### envs ###
export LANG=ja_JP.UTF-8
export OUTPUT_CHARSET=utf-8
# export LV="-Ou8"
# export EDITOR="env TERM=xterm-256color emacsclient -t"
# export GREP_OPTIONS="--binary-files=without-match --color=auto"

### prompt ###
function prompt-git-current-branch {
  local name st color

  if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
      return
  fi
  name=$(basename "`git symbolic-ref HEAD 2> /dev/null`")
  if [[ -z $name ]]; then
      return
  fi
  st=`git status 2> /dev/null`
  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
      color=${fg[green]}
  elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
      color=${fg[yellow]}
  elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
      color=${fg_bold[red]}
  else
      color=${fg[red]}
  fi

  # %{...%} は囲まれた文字列がエスケープシーケンスであることを明示する
  # これをしないと右プロンプトの位置がずれる
  echo "%{$color%}[$name]%{$reset_color%}"
}

setopt prompt_subst
RPROMPT=''
case "${OSTYPE}" in
    darwin*)
        PROMPT='
%F{cyan}%n:%f%F{green}%~%f: $ '
        RPROMPT='`prompt-git-current-branch`'
        ;;
    linux*)
        PROMPT='
%F{cyan}%B[%n@%m$VPC_ENV_TMP]%b%f%F{green}%~%f $ '
        RPROMPT='`prompt-git-current-branch`'
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

# if [ -f ~/.dir_colors ]; then
#     if type dircolors > /dev/null 2>&1; then
#         eval $(dircolors ~/.dir_colors)
#     elif type gdircolors > /dev/null 2>&1; then
#         eval $(gdircolors ~/.dir_colors)
#     fi
# fi
if [ -f ~/.dircolors.ansi-dark ]; then
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
        # alias ls="ls -G"
        # alias ll="ls -lhG"
        # alias la="ls -lhaG"
        alias ls="gls --color=auto"
        alias ll="gls -lh --color=auto"
        alias la="gls -lha --color=auto"
        ;;
    linux*)
        alias ls='ls --color=auto'
        alias ll='ls -lh --color=auto'
        alias la='ls -lha --color=auto'
        ;;
esac

export PATH=$HOME/local/bin:$PATH
export PATH=$PATH:$HOME/.cask/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/local/lib
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

### rbenv ###
[[ -d ~/.rbenv  ]] && \
    export PATH=${HOME}/.rbenv/bin:${PATH} && \
    eval "$(rbenv init -)"
