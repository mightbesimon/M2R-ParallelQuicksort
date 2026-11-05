/*
  qsort_bench.c -- an instrumented harness around the quicksort
  implementations of parallelQuicksort.c.

  Rationale (see journal.org, 2026-11-05 entry): the original program
  measures the three sorting algorithms one after the other inside a
  single process, which confounds "which algorithm" with "in which
  order it ran".  It also uses gettimeofday() (a settable clock) and
  calls the lock-protected libc rand() from inside partition(), which
  turns the pivot draw into a serialisation point that is charged to
  the parallel version.

  This harness therefore:
    - runs exactly ONE algorithm per process invocation, so the run
      order becomes an experimental factor we can randomise;
    - times with clock_gettime(CLOCK_MONOTONIC) and additionally
      reports process CPU time, so we can see parallel efficiency;
    - replaces rand() inside partition() by a lock-free, per-task
      splitmix64 stream, while keeping the ORIGINAL rand() path
      available (--rng global) so that the cost of the lock can be
      measured instead of assumed;
    - takes the input seed on the command line, so a given
      (size, seed) pair is byte-for-byte the same array for every
      algorithm, which makes the comparison paired;
    - exposes THREAD_LEVEL as a parameter instead of hard-coding 10;
    - counts the threads actually created;
    - emits one CSV row, so no fragile log scraping is needed.

  The original src/parallelQuicksort.c is deliberately left untouched
  and remains the reference implementation.

  The sorting code itself (partition / quicksortHelper /
  parallelQuicksortHelper) is a faithful copy of the colleague's
  algorithm.  The only addition is "parcut", an explicitly labelled
  variant used in the report to show how the parallel version can be
  repaired.
*/

#define _POSIX_C_SOURCE 200809L

#include <pthread.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* ---------------------------------------------------------------- */
/* deterministic, lock-free PRNG (splitmix64)                       */
/* ---------------------------------------------------------------- */

static uint64_t splitmix64(uint64_t *s)
{
  uint64_t z = (*s += 0x9E3779B97F4A7C15ULL);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}

/* uniform double in [0,1) */
static double sm_double(uint64_t *s)
{
  return (double) (splitmix64(s) >> 11) * (1.0 / 9007199254740992.0);
}

/* ---------------------------------------------------------------- */
/* global experiment configuration                                  */
/* ---------------------------------------------------------------- */

enum rng_mode { RNG_PERTASK, RNG_GLOBAL };

static enum rng_mode g_rng = RNG_PERTASK;
static int g_cutoff = 0;        /* parcut: stay parallel only above this */
static atomic_long g_threads = 0;

/* ---------------------------------------------------------------- */
/* sequential core (identical to the colleague's code)              */
/* ---------------------------------------------------------------- */

static void swap(double lyst[], int i, int j)
{
  double temp = lyst[i];
  lyst[i] = lyst[j];
  lyst[j] = temp;
}

static int partition(double lyst[], int lo, int hi, uint64_t *rs)
{
  int b = lo;
  double u = (g_rng == RNG_GLOBAL) ? (1.0 * rand() / RAND_MAX) : sm_double(rs);
  int r = (int) (lo + (hi - lo) * u);
  double pivot = lyst[r];
  swap(lyst, r, hi);
  for (int i = lo; i < hi; i++) {
    if (lyst[i] < pivot) {
      swap(lyst, i, b);
      b++;
    }
  }
  swap(lyst, hi, b);
  return b;
}

static void quicksortHelper(double lyst[], int lo, int hi, uint64_t *rs)
{
  if (lo >= hi)
    return;
  int b = partition(lyst, lo, hi, rs);
  quicksortHelper(lyst, lo, b - 1, rs);
  quicksortHelper(lyst, b + 1, hi, rs);
}

/* ---------------------------------------------------------------- */
/* parallel version (identical structure to the colleague's code)   */
/* ---------------------------------------------------------------- */

struct thread_data {
  double *lyst;
  int low;
  int high;
  int level;
  uint64_t rstate;
  int use_cutoff;
};

static void *parallelQuicksortHelper(void *threadarg)
{
  struct thread_data *my_data = (struct thread_data *) threadarg;
  int n = my_data->high - my_data->low + 1;
  int go_sequential = (my_data->level <= 0 || my_data->low == my_data->high + 1);

  if (my_data->use_cutoff && n < g_cutoff)
    go_sequential = 1;

  if (go_sequential) {
    quicksortHelper(my_data->lyst, my_data->low, my_data->high,
                    &my_data->rstate);
    pthread_exit(NULL);
  }

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

  int mid = partition(my_data->lyst, my_data->low, my_data->high,
                      &my_data->rstate);

  struct thread_data thread_data_array[2];
  for (int t = 0; t < 2; t++) {
    thread_data_array[t].lyst = my_data->lyst;
    thread_data_array[t].level = my_data->level - 1;
    thread_data_array[t].use_cutoff = my_data->use_cutoff;
    /* decorrelated, reproducible substream per task */
    thread_data_array[t].rstate =
        my_data->rstate ^ (0x1234567ULL * (uint64_t) (t + 1));
  }
  thread_data_array[0].low = my_data->low;
  thread_data_array[0].high = mid - 1;
  thread_data_array[1].low = mid + 1;
  thread_data_array[1].high = my_data->high;

  pthread_t threads[2];
  for (int t = 0; t < 2; t++) {
    int rc = pthread_create(&threads[t], &attr, parallelQuicksortHelper,
                            (void *) &thread_data_array[t]);
    if (rc) {
      fprintf(stderr, "ERROR; pthread_create() returned %d\n", rc);
      exit(EXIT_FAILURE);
    }
    atomic_fetch_add(&g_threads, 1);
  }

  pthread_attr_destroy(&attr);
  for (int t = 0; t < 2; t++) {
    void *status;
    int rc = pthread_join(threads[t], &status);
    if (rc) {
      fprintf(stderr, "ERROR; pthread_join() returned %d\n", rc);
      exit(EXIT_FAILURE);
    }
  }
  pthread_exit(NULL);
}

static void parallelQuicksort(double lyst[], int size, int tlevel,
                              uint64_t seed, int use_cutoff)
{
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

  struct thread_data td;
  td.lyst = lyst;
  td.low = 0;
  td.high = size - 1;
  td.level = tlevel;
  td.rstate = seed;
  td.use_cutoff = use_cutoff;

  pthread_t theThread;
  int rc = pthread_create(&theThread, &attr, parallelQuicksortHelper,
                          (void *) &td);
  if (rc) {
    fprintf(stderr, "ERROR; pthread_create() returned %d\n", rc);
    exit(EXIT_FAILURE);
  }
  atomic_fetch_add(&g_threads, 1);

  pthread_attr_destroy(&attr);
  void *status;
  rc = pthread_join(theThread, &status);
  if (rc) {
    fprintf(stderr, "ERROR; pthread_join() returned %d\n", rc);
    exit(EXIT_FAILURE);
  }
}

/* ---------------------------------------------------------------- */
/* helpers                                                          */
/* ---------------------------------------------------------------- */

static int compare_doubles(const void *a, const void *b)
{
  const double da = *(const double *) a;
  const double db = *(const double *) b;
  return (da > db) - (da < db);
}

static int isSorted(const double lyst[], int size)
{
  for (int i = 1; i < size; i++)
    if (lyst[i] < lyst[i - 1])
      return 0;
  return 1;
}

static double now_monotonic(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double) ts.tv_sec + 1e-9 * (double) ts.tv_nsec;
}

static double now_cputime(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
  return (double) ts.tv_sec + 1e-9 * (double) ts.tv_nsec;
}

static void usage(const char *prog)
{
  fprintf(stderr,
          "usage: %s --algo seq|par|libc|parcut --size N [options]\n"
          "  --seed S      input seed (default 1); same (size,seed) => same array\n"
          "  --tlevel L    parallel recursion levels (default 10, as in the original)\n"
          "  --cutoff C    parcut: go sequential below C elements (default 0)\n"
          "  --rng global|pertask   pivot RNG (default pertask)\n"
          "  --warmup W    unmeasured sorts before the measured one (default 0)\n"
          "  --header      print the CSV header and exit\n",
          prog);
}

static const char *CSV_HEADER =
    "algo,size,seed,tlevel,cutoff,rng,warmup,nthreads,ncpu,"
    "wall_time,cpu_time,sorted";

int main(int argc, char *argv[])
{
  const char *algo = NULL;
  long size = -1;
  unsigned long seed = 1;
  int tlevel = 10;
  int warmup = 0;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--header")) {
      printf("%s\n", CSV_HEADER);
      return 0;
    } else if (!strcmp(argv[i], "--algo") && i + 1 < argc) {
      algo = argv[++i];
    } else if (!strcmp(argv[i], "--size") && i + 1 < argc) {
      size = atol(argv[++i]);
    } else if (!strcmp(argv[i], "--seed") && i + 1 < argc) {
      seed = strtoul(argv[++i], NULL, 10);
    } else if (!strcmp(argv[i], "--tlevel") && i + 1 < argc) {
      tlevel = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--cutoff") && i + 1 < argc) {
      g_cutoff = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--warmup") && i + 1 < argc) {
      warmup = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--rng") && i + 1 < argc) {
      const char *m = argv[++i];
      if (!strcmp(m, "global"))
        g_rng = RNG_GLOBAL;
      else if (!strcmp(m, "pertask"))
        g_rng = RNG_PERTASK;
      else {
        usage(argv[0]);
        return 2;
      }
    } else {
      usage(argv[0]);
      return 2;
    }
  }

  if (!algo || size < 0) {
    usage(argv[0]);
    return 2;
  }

  int is_seq = !strcmp(algo, "seq");
  int is_par = !strcmp(algo, "par");
  int is_libc = !strcmp(algo, "libc");
  int is_parcut = !strcmp(algo, "parcut");
  if (!(is_seq || is_par || is_libc || is_parcut)) {
    usage(argv[0]);
    return 2;
  }

  /* The pivot RNG is seeded independently of the input, so that
     changing the algorithm does not change the input array. */
  srand((unsigned) seed);

  double *lystbck = malloc((size_t) size * sizeof(double));
  double *lyst = malloc((size_t) size * sizeof(double));
  if (!lystbck || !lyst) {
    fprintf(stderr, "ERROR: out of memory for size %ld\n", size);
    return 1;
  }

  uint64_t input_state = 0x5DEECE66DULL ^ (uint64_t) seed;
  for (long i = 0; i < size; i++)
    lystbck[i] = sm_double(&input_state);

  /* Touch both buffers so that page-fault cost is not charged to the
     measured region (the original charged it to whatever ran first). */
  memset(lyst, 0, (size_t) size * sizeof(double));

  for (int w = 0; w < warmup; w++) {
    memcpy(lyst, lystbck, (size_t) size * sizeof(double));
    uint64_t rs = (uint64_t) seed * 6364136223846793005ULL + (uint64_t) w;
    if (is_libc)
      qsort(lyst, (size_t) size, sizeof(double), compare_doubles);
    else if (is_seq)
      quicksortHelper(lyst, 0, (int) size - 1, &rs);
    else
      parallelQuicksort(lyst, (int) size, tlevel, rs, is_parcut);
  }
  atomic_store(&g_threads, 0);

  memcpy(lyst, lystbck, (size_t) size * sizeof(double));
  uint64_t rs = 0xA0761D4ULL ^ (uint64_t) seed;

  double c0 = now_cputime();
  double t0 = now_monotonic();
  if (is_libc)
    qsort(lyst, (size_t) size, sizeof(double), compare_doubles);
  else if (is_seq)
    quicksortHelper(lyst, 0, (int) size - 1, &rs);
  else
    parallelQuicksort(lyst, (int) size, tlevel, rs, is_parcut);
  double t1 = now_monotonic();
  double c1 = now_cputime();

  int ok = isSorted(lyst, (int) size);
  long nthreads = atomic_load(&g_threads);
  long ncpu = sysconf(_SC_NPROCESSORS_ONLN);

  printf("%s,%ld,%lu,%d,%d,%s,%d,%ld,%ld,%.9f,%.9f,%d\n",
         algo, size, seed, tlevel, g_cutoff,
         (g_rng == RNG_GLOBAL) ? "global" : "pertask",
         warmup, nthreads, ncpu, t1 - t0, c1 - c0, ok);

  free(lyst);
  free(lystbck);
  return ok ? 0 : 1;
}
