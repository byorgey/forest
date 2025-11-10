#!/bin/sh
while sleep 0.1; do /bin/ls trees/**/*.tree | entr -d forester build forest.toml; done
