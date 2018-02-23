#!/bin/bash
set -ex
cd ~/local/
mkdir -p cask
cd cask
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
