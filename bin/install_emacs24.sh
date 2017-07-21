#! /bin/bash

set -ex 

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
