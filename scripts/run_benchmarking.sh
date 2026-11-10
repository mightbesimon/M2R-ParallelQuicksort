#!/bin/bash
set -e

# Create output folder
OUTPUT_DIR="data"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
HOST=$(hostname)
CSV_FILE="$OUTPUT_DIR/measurements_${HOST}_${TIMESTAMP}.csv"

# Experimental Design Parameters
# Expanded sizes to fill the 100k -> 1M gap and go up to 2M
SIZES=(100 1000 10000 50000 100000 250000 500000 1000000 2000000)
REPS=30

# Recompile the C program
make -C src/

# Write CSV header directly
echo "Size,Rep,Type,Time" > "$CSV_FILE"

# Build execution plan and randomize order to avoid CPU caching/thermal bias
PLAN_FILE=$(mktemp)
for s in "${SIZES[@]}"; do
    for r in $(seq 1 $REPS); do
        echo "$s $r" >> "$PLAN_FILE"
    done
done

SHUFFLED_PLAN=$(mktemp)
shuf "$PLAN_FILE" > "$SHUFFLED_PLAN"

TOTAL_RUNS=$(wc -l < "$SHUFFLED_PLAN")
CURRENT=0

echo "Running $TOTAL_RUNS randomized benchmark runs..."

# Execute benchmark loop
while read -r size rep; do
    CURRENT=$((CURRENT + 1))

    # Run binary and capture output
    RAW_OUT=$(./src/parallelQuicksort "$size")

    # Extract times
    SEQ=$(echo "$RAW_OUT" | grep "^Sequential" | grep -oE "[0-9]+\.[0-9]+")
    PAR=$(echo "$RAW_OUT" | grep "^Parallel" | grep -oE "[0-9]+\.[0-9]+")
    LIBC=$(echo "$RAW_OUT" | grep "^Built-in" | grep -oE "[0-9]+\.[0-9]+")

    # Append rows directly to CSV
    echo "$size,$rep,Sequential,$SEQ" >> "$CSV_FILE"
    echo "$size,$rep,Parallel,$PAR" >> "$CSV_FILE"
    echo "$size,$rep,Built-in,$LIBC" >> "$CSV_FILE"

    if (( CURRENT % 30 == 0 || CURRENT == TOTAL_RUNS )); then
        echo "Progress: $CURRENT / $TOTAL_RUNS completed"
    fi
done < "$SHUFFLED_PLAN"

rm -f "$PLAN_FILE" "$SHUFFLED_PLAN"
echo "Done! Clean CSV saved to: $CSV_FILE"
