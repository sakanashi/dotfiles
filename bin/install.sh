#!/bin/bash
set -ex

SCRIPT_PATH=$(cd $(dirname $0); pwd)

#install emacs
$SCRIPT_PATH/install_emacs24.sh

#install cask
$SCRIPT_PATH/get_cask.sh
cd ~/.emacs.d; cask install

# install tmux
$SCRIPT_PATH/install_tmux.sh

