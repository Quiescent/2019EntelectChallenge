#!/usr/bin/env bash

# This script will only work for the round which the engine was
# designed for.  If you try to run it for a different round then we
# wont be able to deserialise the state.

echo "======================================================================"
echo "===Please note that this should be run from the root of the repo!!===="
echo "======================================================================"
set -e

stack install --local-bin-path bin

echo "======================================================================"
echo "=======================Start of Benchmarking=========================="
echo "======================================================================"
OUTPUT_FILE="benchmarking.bench"
./bin/Benchmark-exe $* +RTS -N2 -RTS > "$OUTPUT_FILE"

CURRENT_COMMIT=$(git log --oneline | head -1 | cut -d' ' -f1)
CURRENT_MACHINE=$(uname)
OUTPUT_DIRECTORY="benchmarking/$CURRENT_MACHINE"
mkdir -p $OUTPUT_DIRECTORY
MATCH_ARG_STRING=$(echo "$*" | grep -o "[0-9][0-9][0-9][0-9].*" | sed -e 's/ /-/g')
mv "$OUTPUT_FILE" "$OUTPUT_DIRECTORY/$CURRENT_COMMIT-$MATCH_ARG_STRING.bench"

echo "Done.  You can find the results in $OUTPUT_DIRECTORY/$CURRENT_COMMIT.bench"
