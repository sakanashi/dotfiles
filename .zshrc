autoload -U compinit
compinit
autoload -Uz colors
colors

### envs ###
export LANG=ja_JP.UTF-8
export OUTPUT_CHARSET=utf-8
# export LV="-Ou8"
# export EDITOR="env TERM=xterm-256color emacsclient -t"
# export GREP_OPTIONS="--binary-files=without-match --color=auto"
export PATH=$HOME/local/bin:$PATH
export PATH=$PATH:$HOME/.cask/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/local/lib
# export PATH=$HOME/local/bin/gradle-2.14.1/bin:$PATH
# export PATH=$HOME/.rbenv/shims:$PATH
# export PATH=$PATH:/opt/yarn-[version]/bin
# export PATH=$PATH:$HOME/local/bin/platform-tools
# source ~/.git-completion.bash

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
setopt hist_ignore_dups
setopt hist_reduce_blanks
setopt share_history
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
autoload history-search-end

### tmux ###
function change_tmux_prefix {
    tmux ls > /dev/null
    if [ $? -eq 1 -a -z "$TMUX" ]; then
    else
        prefix=$(tmux show-options -g prefix)
        if [ "${prefix}" = "prefix C-z" ]; then
            tmux set-option -ag prefix C-o
            tmux set-option -ag status-bg "colour075"
        else
            tmux set-option -ag prefix C-z
            tmux set-option -ag status-bg "green"
        fi
    fi
}
alias prefix='$(change_tmux_prefix)'

### color ###
if [ -f ~/.dircolors.ansi-dark ]; then
    if type dircolors > /dev/null 2>&1; then
        eval $(dircolors ~/.dir_colors)
    elif type gdircolors > /dev/null 2>&1; then
        eval $(gdircolors ~/.dir_colors)
    fi
fi

case "${OSTYPE}" in
    darwin*)
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

alias grepc='grep -nr --color=always --with-filename'
alias less='less -qR'
# if [[ -x `which colordiff` ]]; then
#       alias diff='colordiff'
# fi

### rbenv ###
[[ -d ~/.rbenv  ]] && \
    export PATH=${HOME}/.rbenv/bin:${PATH} && \
    eval "$(rbenv init -)"

### ssh-agent ###
agent="$HOME/.ssh/agent"
if [ -S "$SSH_AUTH_SOCK" ]; then
    case $SSH_AUTH_SOCK in
        /tmp/*/agent.[0-9]*)
            ln -snf "$SSH_AUTH_SOCK" $agent && export SSH_AUTH_SOCK=$agent
    esac
elif [ -S $agent ]; then
    export SSH_AUTH_SOCK=$agent
else
    echo "no ssh-agent"
fi
