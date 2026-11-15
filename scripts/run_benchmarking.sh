#!/bin/bash
set -e

OUTPUT_DIR="data"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
HOST=$(hostname)
CSV_FILE="$OUTPUT_DIR/measurements_${HOST}_${TIMESTAMP}.csv"

SIZES=(100 1000 10000 50000 100000 250000 500000 1000000 2000000)
REPS=30

make -C src/
echo "Size,Rep,Type,Time" > "$CSV_FILE"

PLAN_FILE=$(mktemp)
for s in "${SIZES[@]}"; do
    for r in $(seq 1 $REPS); do
        echo "$s $r" >> "$PLAN_FILE"
    done
done

TOTAL_RUNS=$(wc -l < "$PLAN_FILE")
CURRENT=0

echo "Running $TOTAL_RUNS benchmark runs in structured order..."

while read -r size rep; do
    CURRENT=$((CURRENT + 1))

    RAW_OUT=$(./src/parallelQuicksort "$size")

    SEQ=$(echo "$RAW_OUT" | grep "^Sequential" | grep -oE "[0-9]+\.[0-9]+")
    PAR=$(echo "$RAW_OUT" | grep "^Parallel" | grep -oE "[0-9]+\.[0-9]+")
    LIBC=$(echo "$RAW_OUT" | grep "^Built-in" | grep -oE "[0-9]+\.[0-9]+")

    echo "$size,$rep,Sequential,$SEQ" >> "$CSV_FILE"
    echo "$size,$rep,Parallel,$PAR" >> "$CSV_FILE"
    echo "$size,$rep,Built-in,$LIBC" >> "$CSV_FILE"

    if (( CURRENT % 30 == 0 || CURRENT == TOTAL_RUNS )); then
        echo "Progress: $CURRENT / $TOTAL_RUNS completed"
    fi
done < "$PLAN_FILE"

rm -f "$PLAN_FILE"
echo "Done! Clean CSV saved to: $CSV_FILE"
