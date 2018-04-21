#!/bin/sh

set -ex
cd $HOME
git clone https://github.com/ucsd-progsys/liquidhaskell.git
cd liquidhaskell
stack install
