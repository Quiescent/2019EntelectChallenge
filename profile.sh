#!/usr/bin/env bash

echo "======================================================================"
echo "===Please note that this should be run from the root of the repo!!===="
echo "======================================================================"
set -e

stack install --profile --local-bin-path bin

echo "======================================================================"
echo "==========================Start of Profiling=========================="
echo "======================================================================"
./bin/Profile-exe "../2019-Worms/game-runner/match-logs/2019.07.02.12.47.48/" +RTS -p

# TODO: Store these results in a new directory for this commit or tag
# and machine automatically
