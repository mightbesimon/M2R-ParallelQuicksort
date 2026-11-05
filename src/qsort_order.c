/*
  qsort_order.c -- measures the ORDER EFFECT that the original
  benchmarking script could not see.

  scripts/run_benchmarking.sh ran, inside a single process, the
  sequential sort, then the parallel sort, then libc qsort, always in
  that order.  "Which algorithm" was therefore perfectly confounded
  with "which position in the process".  A position can matter for
  several reasons: the first sort pays the page faults for the array,
  the allocator and the TLB warm up, the CPU ramps its frequency, and
  the 1024-odd threads the parallel version leaves behind have to be
  reaped.

  This program runs all three algorithms in one process, in an order
  given on the command line, and reports the position of each.  With
  all six permutations measured, the effect of position can be
  estimated separately from the effect of algorithm -- which is the
  whole point of randomising the run order.

  Timing and pivot RNG are identical to qsort_bench.c.
*/

#define _POSIX_C_SOURCE 200809L

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static uint64_t splitmix64(uint64_t *s)
{
  uint64_t z = (*s += 0x9E3779B97F4A7C15ULL);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}

static double sm_double(uint64_t *s)
{
  return (double) (splitmix64(s) >> 11) * (1.0 / 9007199254740992.0);
}

static void swap(double l[], int i, int j)
{
  double t = l[i]; l[i] = l[j]; l[j] = t;
}

static int partition(double l[], int lo, int hi, uint64_t *rs)
{
  int b = lo;
  int r = (int) (lo + (hi - lo) * sm_double(rs));
  double pivot = l[r];
  swap(l, r, hi);
  for (int i = lo; i < hi; i++)
    if (l[i] < pivot) { swap(l, i, b); b++; }
  swap(l, hi, b);
  return b;
}

static void qsHelper(double l[], int lo, int hi, uint64_t *rs)
{
  if (lo >= hi) return;
  int b = partition(l, lo, hi, rs);
  qsHelper(l, lo, b - 1, rs);
  qsHelper(l, b + 1, hi, rs);
}

struct td { double *l; int low, high, level; uint64_t rs; };

static void *parHelper(void *arg)
{
  struct td *d = (struct td *) arg;
  if (d->level <= 0 || d->low == d->high + 1) {
    qsHelper(d->l, d->low, d->high, &d->rs);
    pthread_exit(NULL);
  }
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  int mid = partition(d->l, d->low, d->high, &d->rs);
  struct td a[2];
  for (int t = 0; t < 2; t++) {
    a[t].l = d->l; a[t].level = d->level - 1;
    a[t].rs = d->rs ^ (0x1234567ULL * (uint64_t) (t + 1));
  }
  a[0].low = d->low; a[0].high = mid - 1;
  a[1].low = mid + 1; a[1].high = d->high;
  pthread_t th[2];
  for (int t = 0; t < 2; t++)
    if (pthread_create(&th[t], &attr, parHelper, &a[t])) exit(1);
  pthread_attr_destroy(&attr);
  for (int t = 0; t < 2; t++) pthread_join(th[t], NULL);
  pthread_exit(NULL);
}

static void parQuicksort(double l[], int size, int tlevel, uint64_t seed)
{
  struct td d = { l, 0, size - 1, tlevel, seed };
  pthread_t th;
  if (pthread_create(&th, NULL, parHelper, &d)) exit(1);
  pthread_join(th, NULL);
}

static int cmpd(const void *a, const void *b)
{
  const double x = *(const double *) a, y = *(const double *) b;
  return (x > y) - (x < y);
}

static int isSorted(const double l[], long n)
{
  for (long i = 1; i < n; i++)
    if (l[i] < l[i - 1]) return 0;
  return 1;
}

static double mono(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double) ts.tv_sec + 1e-9 * (double) ts.tv_nsec;
}

int main(int argc, char *argv[])
{
  /* usage: qsort_order <size> <seed> <tlevel> <order e.g. spl> */
  if (argc != 5) {
    fprintf(stderr,
            "usage: %s <size> <seed> <tlevel> <order: permutation of spl>\n"
            "  s = sequential, p = parallel, l = libc\n", argv[0]);
    return 2;
  }
  long size = atol(argv[1]);
  unsigned long seed = strtoul(argv[2], NULL, 10);
  int tlevel = atoi(argv[3]);
  const char *order = argv[4];
  if (strlen(order) != 3) return 2;

  double *bck = malloc((size_t) size * sizeof(double));
  double *l = malloc((size_t) size * sizeof(double));
  if (!bck || !l) return 1;
  uint64_t is = 0x5DEECE66DULL ^ (uint64_t) seed;
  for (long i = 0; i < size; i++) bck[i] = sm_double(&is);

  /* Deliberately NOT pre-faulting l: this reproduces the original
     program, where the first algorithm to run pays for the pages. */

  for (int pos = 0; pos < 3; pos++) {
    char c = order[pos];
    memcpy(l, bck, (size_t) size * sizeof(double));
    uint64_t rs = 0xA0761D4ULL ^ (uint64_t) seed;
    const char *name;
    double t0 = mono();
    if (c == 's') { qsHelper(l, 0, (int) size - 1, &rs); name = "seq"; }
    else if (c == 'p') { parQuicksort(l, (int) size, tlevel, rs); name = "par"; }
    else if (c == 'l') { qsort(l, (size_t) size, sizeof(double), cmpd); name = "libc"; }
    else return 2;
    double t1 = mono();
    int ok = isSorted(l, size);
    printf("%s,%ld,%lu,%d,%s,%d,%.9f,%d\n",
           name, size, seed, tlevel, order, pos + 1, t1 - t0, ok);
    if (!ok) fprintf(stderr, "ERROR: %s did not sort\n", name);
  }
  free(l); free(bck);
  return 0;
}
