#!/usr/bin/env bash

echo "======================================================================"
echo "===Please note that this should be run from the root of the repo!!===="
echo "======================================================================"
set -e

stack install --local-bin-path bin

echo "======================================================================"
echo "=======================Start of Benchmarking=========================="
echo "======================================================================"
OUTPUT_FILE="benchmarking.bench"
./bin/Benchmark-exe "matches/round-1/2019.07.07.13.24.43/" > "$OUTPUT_FILE"

CURRENT_COMMIT=$(git log --oneline | head -1 | cut -d' ' -f1)
CURRENT_MACHINE=$(uname)
OUTPUT_DIRECTORY="benchmarking/$CURRENT_MACHINE"
mkdir -p $OUTPUT_DIRECTORY
mv "$OUTPUT_FILE" "$OUTPUT_DIRECTORY/$CURRENT_COMMIT.bench"

echo "Done.  You can find the results in $OUTPUT_DIRECTORY/$CURRENT_COMMIT.bench"
