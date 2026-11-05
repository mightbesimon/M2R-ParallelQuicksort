parallelQuicksort: parallelQuicksort.o

PEDANTIC_PARANOID_FREAK =       -g -Wall -Wshadow -Wcast-align \
				-Waggregate-return -Wmissing-prototypes -Wmissing-declarations \
				-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
				-Wmissing-noreturn  \
				-Wpointer-arith -Wwrite-strings -finline-functions -O0

REASONABLY_CAREFUL_DUDE =	-g -Wall -O2
NO_PRAYER_FOR_THE_WICKED =	-w -O3
WARNINGS = 			$(PEDANTIC_PARANOID_FREAK)
CFLAGS = $(WARNINGS) -pthread -lrt -std=c99 
LIBS = 

%: %.o
	$(CC) $(INCLUDES) $(DEFS) $(CFLAGS) $^ $(LIBS) -o $@ 

%.o: %.c
	$(CC) $(INCLUDES) $(DEFS) $(CFLAGS) -c -o $@ $<

clean:
	rm -f parallelQuicksort *.o *~

# ---------------------------------------------------------------------
# Instrumented harness used for the 2026-11-05 experimental campaign.
# Built at -O2 (REASONABLY_CAREFUL_DUDE) because measuring -O0 code
# tells us about the compiler, not about the algorithm.  The -O0 build
# is kept as qsort_bench_O0 so that the effect of the flag can itself
# be measured.
# ---------------------------------------------------------------------
BENCH_CFLAGS = -g -Wall -Wextra -std=c99 -pthread

qsort_bench: qsort_bench.c
	$(CC) $(BENCH_CFLAGS) -O2 $< -o $@ -lpthread

qsort_bench_O0: qsort_bench.c
	$(CC) $(BENCH_CFLAGS) -O0 $< -o $@ -lpthread

# Built at -O2 like qsort_bench, so that the order experiment differs
# from the main campaign in the run order ALONE.
qsort_order: qsort_order.c
	$(CC) $(BENCH_CFLAGS) -O2 $< -o $@ -lpthread

bench: qsort_bench qsort_bench_O0 qsort_order

clean-bench:
	rm -f qsort_bench qsort_bench_O0 qsort_order

.PHONY: bench clean-bench
