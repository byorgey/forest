#!/bin/zsh
while sleep 0.1; do /bin/ls trees-raw/**/*.tree | /bin/entr -d -s './build.sh; /home/brent/.opam/default/bin/forester build forest-out.toml'; done
