#!/bin/sh

set -ex
cd $HOME
git clone git@github.com:ucsd-progsys/liquidhaskell.git
cd liquidhaskell
stack install
