#!/bin/sh

set -ex
cd "${HOME}"
git clone --recurse-submodules https://github.com/ucsd-progsys/liquidhaskell.git
cd liquidhaskell
stack install
