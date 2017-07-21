#! /bin/bash

set -ex 

ln -s .emacs.d/ ~/.emacs.d

mkdir -p ~/local/src
cd ~/local/src/

# sudo yum install xz -y
ver=24.3

[ -f emacs-${ver}.tar.xz ] && rm emacs-${ver}.tar.xz
#wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-${ver}.tar.xz
curl -O http://core.ring.gr.jp/pub/GNU/emacs/emacs-${ver}.tar.gz
# tar Jxf emacs-${ver}.tar.xz
#xz -df emacs-${ver}.tar.xz
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
