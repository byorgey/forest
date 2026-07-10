#!/bin/sh
mkdir -p _shake
mkdir -p _blocks
cabal build &&\
    cabal exec shake -- "$@"
