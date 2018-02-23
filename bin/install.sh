#!/bin/bash
set -ex

SCRIPT_PATH=$(cd $(dirname $0); pwd)

# install emacs
$SCRIPT_PATH/install_emacs24.sh

# install cask
$SCRIPT_PATH/get_cask.sh
