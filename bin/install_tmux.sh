#!/bin/bash
# set symbolic link
set -u

echo 'Start install tmux'
mkdir -p ~/local/src
cd ~/local/src/

# Get&build libevent
wget https://github.com/libevent/libevent/releases/download/release-2.1.8-stable/libevent-2.1.8-stable.tar.gz
tar zxf libevent-2.1.8-stable.tar.gz
cd libevent-2.1.8-stable
./configure --prefix=${HOME}/local
make
make install
# Get & build ncurses
wget ftp://ftp.gnu.org/gnu/ncurses/ncurses-6.0.tar.gz
tar zxf ncurses-6.0.tar.gz
cd ncurses-6.0
./configure --enable-pc-files --prefix=${HOME}/local --with-pkg-config-libdir=${HOME}/local/lib/pkgconfig --with-termlib
make
make install
# Get tmux
wget https://github.com/tmux/tmux/releases/download/2.6/tmux-2.6.tar.gz
tar zxf tmux-2.6.tar.gz
cd tmux-2.6
wget https://gist.githubusercontent.com/z80oolong/e65baf0d590f62fab8f4f7c358cbcc34/raw/3a3269767c863f3b25dbdf3849ed53150b28306e/tmux-HEAD-fb02df66-fix.diff
patch -p1 < ./tmux-HEAD-fb02df66-fix.diff
PKG_CONFIG_PATH=${HOME}/local/lib/pkgconfig ./configure --prefix=${HOME}/local
make
make install

echo 'tmux install succeeded.'

echo 'Get tpm'
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
echo 'Succeeded.'

# curl -kLO https://github.com/tmux/tmux/releases/download/2.6/tmux-2.6.tar.gz
# tar -zxvf tmux-2.6.tar.gz
# cd tmux-2.6
# ./configure
# make
# sudo make install
