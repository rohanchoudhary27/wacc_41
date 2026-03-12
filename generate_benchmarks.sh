#!/bin/bash

# 1. This points to where your raw WACC files are
BENCHMARK_DIR="benchmark_tests"

# 2. This creates the 'assembly' directory in your project root!
BASE_OUT_DIR="assembly"

# Define configurations for true ablation testing
# Format: "FolderName|CompilerFlags|ExpectedFileSuffix"
CONFIGS=(
    "unoptimised||"
    "optimised|-o|_o"
    "unoptimised-binary|-oBranches -oMemory -oRegisters|_oBranches_Memory_Registers"
    "unoptimised-branches|-oBinary -oMemory -oRegisters|_oBinary_Memory_Registers"
    "unoptimised-memory|-oBinary -oBranches -oRegisters|_oBinary_Branches_Registers"
    "unoptimised-registers|-oBinary -oBranches -oMemory|_oBinary_Branches_Memory"
)

# Clean and prepare output directories inside the root 'assembly' folder
rm -rf "$BASE_OUT_DIR"
for CONFIG in "${CONFIGS[@]}"; do
    DIR_NAME="${CONFIG%%|*}"
    # This creates: assembly/unoptimised, assembly/unoptimised-branches, etc.
    mkdir -p "$BASE_OUT_DIR/$DIR_NAME"
done

echo "========================================"
echo "Generating Benchmark Assembly Files..."
echo "========================================"

for FILE in "$BENCHMARK_DIR"/*.wacc; do
    [ -e "$FILE" ] || { echo "No .wacc files found in $BENCHMARK_DIR!"; exit 1; }
    
    BASENAME=$(basename "$FILE" .wacc)
    echo "Processing: $BASENAME"

    for CONFIG in "${CONFIGS[@]}"; do
        IFS='|' read -r DIR_NAME FLAGS SUFFIX <<< "$CONFIG"
        
        # Run the compiler
        ./compile $FLAGS "$FILE" > /dev/null 2>&1
        
        EXPECTED_FILE="${BASENAME}${SUFFIX}.s"
        
        # This is where the file is going to end up (e.g., assembly/unoptimised-branches/)
        TARGET_DIR="${BASE_OUT_DIR}/${DIR_NAME}"
        
        # Find the generated file and move it to the correct ablation folder
        if [ -f "$EXPECTED_FILE" ]; then
            mv "$EXPECTED_FILE" "${TARGET_DIR}/${BASENAME}.s"
        elif [ -f "${BENCHMARK_DIR}/${EXPECTED_FILE}" ]; then
            mv "${BENCHMARK_DIR}/${EXPECTED_FILE}" "${TARGET_DIR}/${BASENAME}.s"
        else
            echo "  [!] Could not find generated $EXPECTED_FILE for $DIR_NAME"
        fi
    done

done

echo "========================================"
echo "Done! Check the 'assembly' folder in your root directory."
echo "========================================"