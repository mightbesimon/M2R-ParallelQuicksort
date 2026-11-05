M2R-ParallelQuicksort
=====================

This small project aims at teaching 2nd year master students how to
improve their experimental methodology.

The [journal](journal.org) contains all information and experimental reports.

Repository layout
-----------------

| Path | What it is |
|------|------------|
| `src/parallelQuicksort.c` | the original implementation under study, **unmodified** |
| `src/qsort_bench.c` | instrumented harness: one algorithm per process, monotonic clock, parameterised `THREAD_LEVEL`, reproducible input seed |
| `src/qsort_order.c` | runs all three algorithms in one process in a given order, to measure the order effect |
| `scripts/run_benchmarking.sh` | the original benchmarking script, kept for the record |
| `scripts/run_campaign.py` | experiment driver: randomised run order, blocked replication, provenance metadata |
| `scripts/run_order_experiment.py` | driver for the order-effect experiment |
| `scripts/analysis.R` | statistical analysis and all figures (ggplot2) |
| `scripts/csv_quicksort_extractor*.pl` | the original log parsers, kept for the record |
| `data/<host>_<date>/` | raw measurements, one CSV + one `.meta.json` per campaign |
| `figures/` | generated figures, plus a `summary_*.csv` table view for each |

Reproducing the 2026-11-05 campaign
-----------------------------------

```bash
make -C src/ bench            # qsort_bench (-O2) and qsort_bench_O0
make -C src/ qsort_order
python3 scripts/run_campaign.py --list
for c in E1_replication E2_tlevel E3_cores E4_ablation E5_repair E6_headtohead; do
    python3 scripts/run_campaign.py "$c"
done
python3 scripts/run_order_experiment.py
Rscript scripts/analysis.R
```

Requirements: a C99 compiler with pthreads, Python 3.8+, and R with
`ggplot2`, `dplyr`, `tidyr` and `scales`.
