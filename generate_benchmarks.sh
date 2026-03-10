#!/bin/bash

BENCHMARK_DIR="benchmark_tests"
UNOPT_DIR="assembly_unoptimised"
OPT_DIR="assembly_optimised"

rm -rf "$UNOPT_DIR" "$OPT_DIR"
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
    else
        echo "  [!] Unoptimised compilation failed for $BASENAME"
    fi

    ./compile -O "$FILE" > /dev/null 2>&1
    if [ -f "${BASENAME}.s" ]; then
        mv "${BASENAME}.s" "${OPT_DIR}/${BASENAME}.s"
    else
        echo "  [!] Optimised compilation failed for $BASENAME"
    fi

done

echo "========================================"
echo "Done!"
echo "Unoptimised files: ./${UNOPT_DIR}/"
echo "Optimised files:   ./${OPT_DIR}/"