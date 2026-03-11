#!/bin/bash

BENCHMARK_DIR="benchmark_tests"
BASE_OUT_DIR="assembly"
UNOPT_DIR="${BASE_OUT_DIR}/unoptimised"
OPT_DIR="${BASE_OUT_DIR}/optimised"

rm -rf "$BASE_OUT_DIR"
mkdir -p "$UNOPT_DIR"
mkdir -p "$OPT_DIR"

echo "========================================"
echo "Generating Benchmark Assembly Files..."
echo "========================================"

for FILE in "$BENCHMARK_DIR"/*.wacc; do
    [ -e "$FILE" ] || { echo "No .wacc files found in $BENCHMARK_DIR!"; exit 1; }
    
    BASENAME=$(basename "$FILE" .wacc)
    echo "Processing: $BASENAME"

    ./compile "$FILE" > /dev/null 2>&1
    
    if [ -f "${BASENAME}.s" ]; then
        mv "${BASENAME}.s" "${UNOPT_DIR}/${BASENAME}.s"
    elif [ -f "${BENCHMARK_DIR}/${BASENAME}.s" ]; then
        mv "${BENCHMARK_DIR}/${BASENAME}.s" "${UNOPT_DIR}/${BASENAME}.s"
    else
        echo "  [!] Could not find unoptimised .s file for $BASENAME"
    fi

    ./compile -O "$FILE" > /dev/null 2>&1
    
    if [ -f "${BASENAME}_O.s" ]; then
        mv "${BASENAME}_O.s" "${OPT_DIR}/${BASENAME}.s"
    elif [ -f "${BENCHMARK_DIR}/${BASENAME}_O.s" ]; then
        mv "${BENCHMARK_DIR}/${BASENAME}_O.s" "${OPT_DIR}/${BASENAME}.s"
    else
        echo "  [!] Could not find optimised .s file for $BASENAME. Looking for ${BASENAME}_O.s"
    fi

done

echo "========================================"
echo "Done!"
echo "========================================"