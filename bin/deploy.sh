#!/bin/bash
# set symbolic link
set -u

DOTFILES_DIR=$(cd $(dirname $0)/..; pwd)

echo "start setup..."
for f in .??*; do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitconfig.local.template" ] && continue
    [ "$f" = ".gitmodules" ] && continue
    [ "$f" = .*~ ] && continue
    ln -snfv ~/dotfiles/"$f" ~/
done

# [ -e ~/.gitconfig.local ] || cp ~/dotfiles/.gitconfig.local.template ~/.gitconfig.local

cat << END

succeeded.

END
