#!/usr/bin/env python3
"""
run_order_experiment.py -- estimate the order effect that the original
fixed-order design could not distinguish from the algorithm effect.

All six permutations of (sequential, parallel, libc) are run inside a
single process, as the original script did, with the permutation and
the replicate shuffled across the campaign.  The resulting CSV has a
'position' column, so position and algorithm are no longer confounded.
"""
import datetime as dt
import itertools
import os
import platform
import random
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXE = os.path.join(ROOT, "src", "qsort_order")
SIZES = [100_000, 1_000_000]
TLEVEL = 10
BLOCKS = 20
ORDERS = ["".join(p) for p in itertools.permutations("spl")]

def main():
    outdir = os.path.join(ROOT, "data",
                          f"{platform.node()}_{dt.date.today().isoformat()}")
    os.makedirs(outdir, exist_ok=True)
    path = os.path.join(outdir,
                        f"E7_order_{dt.datetime.now():%H%M}.csv")
    jobs = []
    rng = random.Random(777)
    for b in range(BLOCKS):
        block = [(b + 1, s, o) for s in SIZES for o in ORDERS]
        rng.shuffle(block)
        jobs.extend(block)
    print(f"E7_order: {len(jobs)} process launches "
          f"({3*len(jobs)} measurements) -> {path}")
    with open(path, "w") as fh:
        fh.write("campaign,block,algo,size,seed,tlevel,order,position,"
                 "wall_time,sorted\n")
        for i, (blk, size, order) in enumerate(jobs):
            p = subprocess.run([EXE, str(size), str(blk), str(TLEVEL), order],
                               capture_output=True, text=True, timeout=600)
            if p.returncode != 0:
                print("FAIL", p.stderr.strip(), file=sys.stderr)
                continue
            for line in p.stdout.strip().splitlines():
                fh.write(f"E7_order,{blk},{line}\n")
            fh.flush()
            if (i + 1) % 40 == 0:
                print(f"  {i+1}/{len(jobs)}", flush=True)
    print("done")

if __name__ == "__main__":
    main()
