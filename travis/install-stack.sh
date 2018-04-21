#!/bin/sh

set -ex
mkdir -p $HOME/.local/bin
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'
