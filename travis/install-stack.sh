#!/bin/sh

set -ex
curl -L https://www.stackage.org/stack/linux-x86_64 |
  tar xz --wildcards --strip-components=1 -C "${HOME}/.local/bin" '*/stack'
