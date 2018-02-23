#! /bin/bash
set -ex

# cd $(cd $(dirname $0); pwd)/..
# EMACS_CONF_DIR=$(pwd)
# ls -a ~/.emacs.d;  rm -r ~/.emacs.d
# cd ~
# ln -s $EMACS_CONF_DIR/.emacs.d

mkdir -p ~/local/src
cd ~/local/src/

ver=24.3

[ -f emacs-${ver}.tar.xz ] && rm emacs-${ver}.tar.xz
curl -O http://core.ring.gr.jp/pub/GNU/emacs/emacs-${ver}.tar.gz
tar -zxf emacs-${ver}.tar.gz

cd emacs-${ver}/
./configure --prefix=$HOME/local --without-x
make
make install

# after step
# 1. path
# export PATH=$HOME/local/bin:$PATH
# export PATH=$PATH:$HOME/.cask/bin
# 2. install cask 
