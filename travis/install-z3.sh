#!/bin/sh

set -ex

z3_release='z3-4.6.0-x64-ubuntu-14.04'

mkdir -p "${HOME}/z3"
cd "${HOME}/z3"
wget "https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/${z3_release}.zip"
unzip "${z3_release}.zip"
mv "${z3_release}/bin/z3" "${HOME}/.local/bin"
