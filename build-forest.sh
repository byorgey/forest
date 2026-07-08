#!/bin/sh
while sleep 0.1; do /bin/ls trees-raw/**/*.tree | entr -d -s './build.sh; forester build forest.toml'; done
