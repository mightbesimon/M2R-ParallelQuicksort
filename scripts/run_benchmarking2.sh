#!/bin/bash
set -e

OUTPUT_DIR="data"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
HOST=$(hostname)
CSV_FILE="$OUTPUT_DIR/results_exp2_large_${HOST}_${TIMESTAMP}.csv"

# Large array sizes to test parallel speedup at scale
SIZES=(1000000 2000000 5000000 7500000 10000000 15000000 20000000)
REPS=30

make -C src/
echo "Size,Rep,Type,Time" > "$CSV_FILE"

PLAN_FILE=$(mktemp)
for s in "${SIZES[@]}"; do
    for r in $(seq 1 $REPS); do
        echo "$s $r" >> "$PLAN_FILE"
    done
done

SHUFFLED_PLAN=$(mktemp)
sort -R "$PLAN_FILE" > "$SHUFFLED_PLAN"

TOTAL_RUNS=$(wc -l < "$SHUFFLED_PLAN")
CURRENT=0

echo "Running Experiment 2: Large scale ($TOTAL_RUNS runs)..."

while read -r size rep; do
    CURRENT=$((CURRENT + 1))
    RAW_OUT=$(./src/parallelQuicksort "$size")

    SEQ=$(echo "$RAW_OUT" | grep "^Sequential" | grep -oE "[0-9]+\.[0-9]+")
    PAR=$(echo "$RAW_OUT" | grep "^Parallel" | grep -oE "[0-9]+\.[0-9]+")
    LIBC=$(echo "$RAW_OUT" | grep "^Built-in" | grep -oE "[0-9]+\.[0-9]+")

    echo "$size,$rep,Sequential,$SEQ" >> "$CSV_FILE"
    echo "$size,$rep,Parallel,$PAR" >> "$CSV_FILE"
    echo "$size,$rep,Built-in,$LIBC" >> "$CSV_FILE"

    if (( CURRENT % 21 == 0 || CURRENT == TOTAL_RUNS )); then
        echo "Exp 2 Progress: $CURRENT / $TOTAL_RUNS completed"
    fi
done < "$SHUFFLED_PLAN"

rm -f "$PLAN_FILE" "$SHUFFLED_PLAN"
echo "Done! Exp 2 saved to: $CSV_FILE"
