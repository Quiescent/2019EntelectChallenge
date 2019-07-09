#!/usr/bin/env bash

echo "======================================================================"
echo "===Please note that this should be run from the root of the repo!!===="
echo "======================================================================"
set -e

stack install --profile --local-bin-path bin

echo "======================================================================"
echo "==========================Start of Profiling=========================="
echo "======================================================================"
./bin/Profile-exe "matches/round-1/2019.07.07.13.24.43/" +RTS -p

CURRENT_COMMIT=$(git log --oneline | head -1 | cut -d' ' -f1)
CURRENT_MACHINE=$(uname)
OUTPUT_DIRECTORY="profiling/$CURRENT_MACHINE"
mkdir -p $OUTPUT_DIRECTORY
mv Profile-exe.prof "$OUTPUT_DIRECTORY/$CURRENT_COMMIT.prof"

echo "Done.  You can find the results in $OUTPUT_DIRECTORY/$CURRENT_COMMIT.prof"
