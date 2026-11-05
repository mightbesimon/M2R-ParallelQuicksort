#!/usr/bin/env python3
"""
run_campaign.py -- experiment driver for the parallel-quicksort study.

What this fixes compared to scripts/run_benchmarking.sh
-------------------------------------------------------
1. ORDER.  The old script ran, inside one process, sequential then
   parallel then libc, always in that order, for every size in
   increasing order.  "Which algorithm" was therefore perfectly
   confounded with "which position in the process" and "how warm the
   machine is".  Here every measurement is a separate process and the
   whole list of (factor combination, replicate) jobs is SHUFFLED, so
   any drift in machine state (frequency scaling, thermal, noisy
   neighbour on the hypervisor) adds noise instead of bias.
2. REPLICATION.  5 repetitions, all taken back to back, estimate the
   variability of a 2-second window, not of the experiment.  Here
   replicates are organised in blocks: the complete design is run
   once, reshuffled, run again, and so on, so replicates are spread
   over the whole campaign.
3. PARAMETER RANGE.  The old grid was 1e2..1e6 by decades, and the
   conclusion ("worth it above 400000") was read off a gap in which
   there was not a single data point.  The grids here are dense and
   log-spaced, and THREAD_LEVEL -- hard-coded to 10 in the original
   and never varied -- is a first-class factor.
4. PROVENANCE.  Every campaign writes a .meta.json next to its CSV
   with the git revision, compiler version, CPU model, kernel, and
   the exact design, so a result can be traced back to the machine
   state that produced it.

Usage:  run_campaign.py <campaign> [--out DIR] [--dry-run]
        run_campaign.py --list
"""

import argparse
import datetime as dt
import itertools
import json
import os
import platform
import random
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
BENCH = {
    "O2": os.path.join(ROOT, "src", "qsort_bench"),
    "O0": os.path.join(ROOT, "src", "qsort_bench_O0"),
}

CSV_COLS = ["algo", "size", "seed", "tlevel", "cutoff", "rng", "warmup",
            "nthreads", "ncpu", "wall_time", "cpu_time", "sorted"]


def logspace_sizes(lo, hi, n):
    """n log-spaced integer sizes in [lo, hi], de-duplicated."""
    out = []
    for i in range(n):
        v = int(round(lo * (hi / lo) ** (i / (n - 1))))
        if v not in out:
            out.append(v)
    return out


# --------------------------------------------------------------------
# Campaign definitions.  Each is a dict of factor -> list of levels.
# 'blocks' is the number of times the complete design is replicated;
# each block is independently shuffled.
# --------------------------------------------------------------------

def campaigns():
    c = {}

    # E0: sequential and libc baselines at EXACTLY the sizes used by the
    # tlevel (E2), core (E3) and cutoff (E5) sweeps.  Without this the
    # speedups in those sweeps would have to be interpolated from E1's
    # log grid, which uses different sizes.
    c["E0_baseline"] = dict(
        blocks=15,
        opt=["O2"],
        factors=dict(
            algo=["seq", "libc"],
            size=[10_000, 100_000, 1_000_000, 4_000_000],
            tlevel=[0],
            cutoff=[0],
            rng=["pertask"],
            cores=[0],
        ),
    )

    # E1: honest replication of the original question, at the original
    # settings (tlevel=10) but with a sound design.  Dense size grid
    # that actually covers the 1e5..1e6 region the original conclusion
    # was extrapolated into.
    c["E1_replication"] = dict(
        blocks=15,
        opt=["O2"],
        factors=dict(
            algo=["seq", "par", "libc"],
            size=logspace_sizes(100, 4_000_000, 17),
            tlevel=[10],
            cutoff=[0],
            rng=["pertask"],
            cores=[0],
        ),
    )

    # E2: the hard-coded constant.  THREAD_LEVEL from 0 (= sequential)
    # to 12 (= 8191 threads) across four sizes.
    c["E2_tlevel"] = dict(
        blocks=12,
        opt=["O2"],
        factors=dict(
            algo=["par"],
            size=[10_000, 100_000, 1_000_000, 4_000_000],
            tlevel=list(range(0, 13)),
            cutoff=[0],
            rng=["pertask"],
            cores=[0],
        ),
    )

    # E3: core scaling.  taskset restricts the process to k CPUs, which
    # is a *proxy* for a smaller machine (same cache and memory
    # bandwidth, fewer cores) -- see the journal for the caveat.
    c["E3_cores"] = dict(
        blocks=12,
        opt=["O2"],
        factors=dict(
            algo=["seq", "par", "parcut"],
            size=[100_000, 1_000_000, 4_000_000],
            tlevel=[10],
            cutoff=[0],
            rng=["pertask"],
            cores=[1, 2, 3, 4],
        ),
        # tlevel/cutoff are overridden per algo below
        overrides=dict(
            par=dict(tlevel=[2, 3, 4, 10]),
            parcut=dict(tlevel=[16], cutoff=[50_000]),
            seq=dict(tlevel=[0]),
        ),
    )

    # E4: ablation of the two measurement artefacts baked into the
    # original setup -- compiling the code under test at -O0, and
    # drawing the pivot from the lock-protected libc rand().
    c["E4_ablation"] = dict(
        blocks=12,
        opt=["O0", "O2"],
        factors=dict(
            algo=["seq", "par"],
            size=[100_000, 1_000_000],
            tlevel=[10],
            cutoff=[0],
            rng=["global", "pertask"],
            cores=[0],
        ),
    )

    # E5: the repaired parallel version.  'parcut' keeps the
    # colleague's algorithm but stops spawning threads below a cutoff;
    # tlevel=16 is effectively unbounded so the cutoff alone decides.
    c["E5_repair"] = dict(
        blocks=12,
        opt=["O2"],
        factors=dict(
            algo=["parcut"],
            size=[100_000, 1_000_000, 4_000_000],
            tlevel=[16],
            cutoff=[1_000, 5_000, 20_000, 50_000, 200_000, 1_000_000],
            rng=["pertask"],
            cores=[0],
        ),
    )

    # E6: head-to-head of all four variants over a dense size grid,
    # with the tuned settings found by E2/E5.  This is the figure the
    # colleague actually wants, and the one the break-even analysis
    # and the extrapolation are fitted on.
    c["E6_headtohead"] = dict(
        blocks=15,
        opt=["O2"],
        factors=dict(
            algo=["seq", "par", "libc", "parcut"],
            size=logspace_sizes(1_000, 8_000_000, 15),
            tlevel=[10],
            cutoff=[0],
            rng=["pertask"],
            cores=[0],
        ),
        overrides=dict(
            par=dict(tlevel=[3]),
            parcut=dict(tlevel=[16], cutoff=[50_000]),
            seq=dict(tlevel=[0]),
        ),
    )

    return c


def expand(spec):
    """Expand a campaign spec into a list of job dicts (one block)."""
    jobs = []
    base = spec["factors"]
    overrides = spec.get("overrides", {})
    for opt in spec["opt"]:
        for algo in base["algo"]:
            f = dict(base)
            f.pop("algo")
            f.update(overrides.get(algo, {}))
            keys = sorted(f.keys())
            for combo in itertools.product(*[f[k] for k in keys]):
                job = dict(zip(keys, combo))
                job["algo"] = algo
                job["opt"] = opt
                # libc and seq ignore tlevel/cutoff -> collapse the
                # duplicate cells so we do not waste runs.
                if algo in ("seq", "libc"):
                    job["tlevel"] = 0
                    job["cutoff"] = 0
                if algo == "par":
                    job["cutoff"] = 0
                jobs.append(job)
    # de-duplicate collapsed cells
    seen, uniq = set(), []
    for j in jobs:
        k = tuple(sorted(j.items()))
        if k not in seen:
            seen.add(k)
            uniq.append(j)
    return uniq


def machine_meta():
    def sh(cmd):
        try:
            return subprocess.run(cmd, shell=True, capture_output=True,
                                  text=True, timeout=20).stdout.strip()
        except Exception:
            return ""

    cpu_model = ""
    try:
        with open("/proc/cpuinfo") as fh:
            for line in fh:
                if line.startswith("model name"):
                    cpu_model = line.split(":", 1)[1].strip()
                    break
    except OSError:
        pass

    gov = sh("cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor "
             "2>/dev/null | sort -u | tr '\\n' ' '")
    return {
        "hostname": platform.node(),
        "kernel": platform.release(),
        "platform": platform.platform(),
        "cpu_model": cpu_model,
        "ncpu_online": os.cpu_count(),
        "scaling_governor": gov or "unavailable",
        "virtualisation": sh("systemd-detect-virt 2>/dev/null") or "unknown",
        "mem_total_kb": sh("awk '/MemTotal/{print $2}' /proc/meminfo"),
        "libc": " ".join(platform.libc_ver()),
        "cc_version": sh("cc --version | head -1"),
        "git_rev": sh(f"git -C {ROOT} rev-parse HEAD"),
        "git_dirty": bool(sh(f"git -C {ROOT} status --porcelain")),
        "taskset_available": shutil.which("taskset") is not None,
    }


def run_one(job, timeout=600):
    exe = BENCH[job["opt"]]
    cmd = [exe,
           "--algo", job["algo"],
           "--size", str(job["size"]),
           "--seed", str(job["seed"]),
           "--tlevel", str(job["tlevel"]),
           "--cutoff", str(job["cutoff"]),
           "--rng", job["rng"]]
    cores = job.get("cores", 0)
    if cores:
        cmd = ["taskset", "-c", ",".join(str(i) for i in range(cores))] + cmd
    p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
    if p.returncode != 0 or not p.stdout.strip():
        return None, (p.stderr or "nonzero exit").strip()
    return p.stdout.strip(), None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("campaign", nargs="?")
    ap.add_argument("--list", action="store_true")
    ap.add_argument("--out", default=None)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--seed", type=int, default=20261105,
                    help="seed of the run-order shuffle (reproducible design)")
    args = ap.parse_args()

    cs = campaigns()
    if args.list or not args.campaign:
        for name, spec in cs.items():
            n = len(expand(spec)) * spec["blocks"]
            print(f"{name:20s} {n:6d} measurements  ({spec['blocks']} blocks)")
        return 0

    if args.campaign not in cs:
        print(f"unknown campaign {args.campaign!r}", file=sys.stderr)
        return 2
    spec = cs[args.campaign]

    design = expand(spec)
    rng = random.Random(args.seed)

    # Build the full job list: block by block, each block a fresh
    # shuffle of the complete design.  The replicate index doubles as
    # the input seed, so replicate r of every cell sorts the *same*
    # array -- the comparison between algorithms is paired.
    jobs = []
    for b in range(spec["blocks"]):
        block = [dict(j, seed=b + 1, block=b + 1) for j in design]
        rng.shuffle(block)
        jobs.extend(block)

    outdir = args.out or os.path.join(
        ROOT, "data", f"{platform.node()}_{dt.date.today().isoformat()}")
    os.makedirs(outdir, exist_ok=True)
    stamp = dt.datetime.now().strftime("%H%M")
    csv_path = os.path.join(outdir, f"{args.campaign}_{stamp}.csv")
    meta_path = os.path.join(outdir, f"{args.campaign}_{stamp}.meta.json")

    meta = {
        "campaign": args.campaign,
        "started_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
        "design_shuffle_seed": args.seed,
        "blocks": spec["blocks"],
        "cells_per_block": len(design),
        "total_measurements": len(jobs),
        "spec": {k: v for k, v in spec.items()},
        "machine": machine_meta(),
    }

    print(f"campaign {args.campaign}: {len(design)} cells x "
          f"{spec['blocks']} blocks = {len(jobs)} measurements")
    print(f"  -> {csv_path}")
    if args.dry_run:
        print(json.dumps(meta, indent=2, default=str))
        return 0

    with open(meta_path, "w") as fh:
        json.dump(meta, fh, indent=2, default=str)

    t_start = time.time()
    failures = []
    with open(csv_path, "w") as fh:
        fh.write(",".join(["campaign", "block", "opt", "cores", "run_idx",
                           "t_offset"] + CSV_COLS) + "\n")
        for i, job in enumerate(jobs):
            line, err = run_one(job)
            if err:
                failures.append((job, err))
                continue
            off = time.time() - t_start
            fh.write(f"{args.campaign},{job['block']},{job['opt']},"
                     f"{job.get('cores', 0)},{i},{off:.3f},{line}\n")
            fh.flush()
            if (i + 1) % 50 == 0 or i + 1 == len(jobs):
                el = time.time() - t_start
                print(f"  {i+1}/{len(jobs)}  {el:7.1f}s elapsed  "
                      f"eta {el/(i+1)*(len(jobs)-i-1):7.1f}s", flush=True)

    meta["finished_utc"] = dt.datetime.now(dt.timezone.utc).isoformat()
    meta["wallclock_s"] = round(time.time() - t_start, 1)
    meta["failures"] = [{"job": j, "error": e} for j, e in failures]
    meta["machine_after"] = machine_meta()
    with open(meta_path, "w") as fh:
        json.dump(meta, fh, indent=2, default=str)

    print(f"done in {meta['wallclock_s']}s, {len(failures)} failures")
    return 0


if __name__ == "__main__":
    sys.exit(main())
