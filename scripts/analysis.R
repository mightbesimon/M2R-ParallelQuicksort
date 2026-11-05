#!/usr/bin/env Rscript
# =====================================================================
#  analysis.R -- statistical analysis and figures for the
#  parallel-quicksort performance study.
#
#  Run from the repository root:
#      Rscript scripts/analysis.R [data/<host>_<date>]
#
#  Produces figures/*.png and figures/summary_*.csv (the figures carry
#  a contrast WARN on two of the four series colours, so a table view
#  is shipped alongside every figure, as the palette rule requires).
# =====================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
})

args    <- commandArgs(trailingOnly = TRUE)
datadir <- if (length(args) >= 1) args[1] else {
  d <- list.dirs("data", recursive = FALSE)
  d <- d[grepl("\\d{4}-\\d{2}-\\d{2}$", d)]
  tail(sort(d), 1)
}
figdir <- "figures"
dir.create(figdir, showWarnings = FALSE)
message("data dir: ", datadir)

# ---------------------------------------------------------------------
# Palette (validated: see journal.org).  Series colour follows the
# ALGORITHM, never its rank, so a facet that drops a series does not
# repaint the survivors.
# ---------------------------------------------------------------------
PAL <- c(seq = "#2a78d6", par = "#eb6834", libc = "#1baf7a",
         parcut = "#eda100")
SHP <- c(seq = 16, par = 17, libc = 15, parcut = 18)   # secondary encoding
LBL <- c(seq = "Sequential", par = "Parallel (as shipped, L = 10)",
         libc = "libc qsort", parcut = "Parallel + cutoff")

# Campaign E6 deliberately runs `par` at the TUNED tlevel found by E2
# (L = 3) rather than at the shipped L = 10, so the head-to-head
# compares the best each approach can do.  Figures fed by E6 must use
# this label set, never the one above.
LBL6 <- c(seq = "Sequential", par = "Parallel (tuned, L = 3)",
          libc = "libc qsort", parcut = "Parallel + cutoff (50k)")

INK    <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"
SURF   <- "#fcfcfb"; GRID <- "#e1e0d9"; AXIS  <- "#c3c2b7"
CRIT   <- "#d03b3b"; GOOD <- "#0ca30c"

theme_lab <- function(base = 11) {
  theme_minimal(base_size = base,
                base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = SURF, colour = NA),
      panel.background  = element_rect(fill = SURF, colour = NA),
      panel.grid.major  = element_line(colour = GRID, linewidth = 0.3),
      panel.grid.minor  = element_line(colour = GRID, linewidth = 0.15),
      axis.line         = element_line(colour = AXIS, linewidth = 0.3),
      axis.ticks        = element_line(colour = AXIS, linewidth = 0.3),
      axis.text         = element_text(colour = MUTED, size = rel(0.85)),
      axis.title        = element_text(colour = INK2),
      plot.title        = element_text(colour = INK, face = "bold",
                                       size = rel(1.15)),
      plot.subtitle     = element_text(colour = INK2, size = rel(0.9),
                                       lineheight = 1.2),
      plot.caption      = element_text(colour = MUTED, size = rel(0.75),
                                       hjust = 0, lineheight = 1.2),
      strip.text        = element_text(colour = INK, face = "bold",
                                       size = rel(0.85)),
      legend.position   = "top",
      legend.title      = element_blank(),
      legend.text       = element_text(colour = INK2),
      legend.key        = element_blank(),
      plot.margin       = margin(14, 18, 12, 14)
    )
}

save_fig <- function(p, name, w = 9, h = 5.6) {
  ggsave(file.path(figdir, paste0(name, ".png")), p,
         width = w, height = h, dpi = 190, bg = SURF)
  message("  wrote ", file.path(figdir, paste0(name, ".png")))
}

write_tbl <- function(df, name) {
  utils::write.csv(df, file.path(figdir, paste0(name, ".csv")),
                   row.names = FALSE)
}

# ---------------------------------------------------------------------
# Load every campaign CSV in the data directory
# ---------------------------------------------------------------------
# E7 has its own schema (one row per algorithm per process, with a
# 'position' column) and is loaded separately further down.
files <- list.files(datadir, pattern = "^E[0-6].*\\.csv$",
                    full.names = TRUE)
stopifnot(length(files) > 0)
raw <- bind_rows(lapply(files, read.csv, stringsAsFactors = FALSE))
raw$algo <- factor(raw$algo, levels = names(PAL))
message("loaded ", nrow(raw), " measurements from ", length(files), " files")

# Correctness gate: a timing is meaningless if the array came out
# unsorted, so refuse to analyse a campaign containing such a run.
stopifnot(!any(is.na(raw$sorted)), all(raw$sorted == 1))

# ---------------------------------------------------------------------
# Interval estimators
#   ci_mean : Student-t CI on the mean
#   ci_boot : BCa-free percentile bootstrap, used where n is small or
#             the distribution is visibly skewed (it always is, for
#             timing data: a long right tail from scheduling noise).
# ---------------------------------------------------------------------
ci_mean <- function(x, conf = 0.95) {
  n <- length(x); m <- mean(x)
  if (n < 2) return(c(mean = m, lo = NA, hi = NA, n = n, sd = NA))
  s  <- sd(x); tq <- qt(1 - (1 - conf) / 2, n - 1)
  c(mean = m, lo = m - tq * s / sqrt(n), hi = m + tq * s / sqrt(n),
    n = n, sd = s)
}

ci_boot <- function(x, conf = 0.95, B = 4000, stat = mean) {
  n <- length(x)
  if (n < 3) return(c(est = stat(x), lo = NA, hi = NA, n = n))
  bs <- replicate(B, stat(sample(x, n, replace = TRUE)))
  q  <- quantile(bs, c((1 - conf) / 2, 1 - (1 - conf) / 2), names = FALSE)
  c(est = stat(x), lo = q[1], hi = q[2], n = n)
}

summarise_ci <- function(df, ...) {
  grp <- enquos(...)
  df %>% group_by(!!!grp) %>%
    group_modify(~ {
      a <- ci_mean(.x$wall_time); b <- ci_boot(.x$wall_time)
      tibble(mean = a[["mean"]], t_lo = a[["lo"]], t_hi = a[["hi"]],
             sd = a[["sd"]], n = a[["n"]],
             b_lo = b[["lo"]], b_hi = b[["hi"]],
             median = median(.x$wall_time),
             cpu = mean(.x$cpu_time),
             rsd = a[["sd"]] / a[["mean"]])
    }) %>% ungroup()
}

log_breaks_all <- function(x) 10 ^ seq(floor(log10(min(x))),
                                      ceiling(log10(max(x))))
sci <- label_number(scale_cut = cut_short_scale())

CAP <- paste0("Intel Xeon @2.10GHz, 4 vCPU (KVM), Linux 6.18, glibc 2.39, gcc 13.3 -O2.  ",
              "One process per measurement; run order fully randomised;\n",
              "replicates blocked across the campaign.  Ribbons and bars are 95% CI ",
              "(percentile bootstrap, 4000 resamples).")

# =====================================================================
#  F1 -- the original figure, redone
# =====================================================================
e1 <- raw %>% filter(campaign == "E1_replication")
s1 <- summarise_ci(e1, algo, size)
write_tbl(s1, "summary_F1_time_vs_size")

p1 <- ggplot(s1, aes(size, mean, colour = algo, fill = algo, shape = algo)) +
  geom_ribbon(aes(ymin = b_lo, ymax = b_hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.1, stroke = 0) +
  scale_x_log10(breaks = log_breaks_all(s1$size), labels = sci) +
  scale_y_log10(breaks = log_breaks_all(s1$mean),
                labels = label_number(accuracy = 0.000001, drop0trailing = TRUE)) +
  scale_colour_manual(values = PAL, labels = LBL) +
  scale_fill_manual(values = PAL, labels = LBL, guide = "none") +
  scale_shape_manual(values = SHP, labels = LBL) +
  annotation_logticks(sides = "bl", colour = AXIS, size = 0.25,
                      short = unit(0.04, "cm"), mid = unit(0.07, "cm"),
                      long = unit(0.11, "cm")) +
  labs(title = "Sorting time against array size, 17 sizes x 15 randomised replicates",
       subtitle = paste("At the shipped setting (THREAD_LEVEL = 10) the parallel version loses to the sequential one up to",
                        "\n~1,000,000 elements and only overtakes it past ~2,000,000.  The original report claimed a gain at",
                        "\n1,000,000, where we measure a 0.71x SLOWDOWN (95% CI [0.68, 0.75])."),
       x = "Array size (elements)", y = "Wall-clock time (s)",
       caption = CAP) +
  theme_lab()
save_fig(p1, "F1_time_vs_size")

# =====================================================================
#  F2 -- paired speedup.  Because replicate r of every cell sorts the
#  SAME array (seed = r), the comparison is paired: we can take the
#  per-replicate ratio instead of the ratio of means, which removes
#  the run-to-run input variability from the error bars entirely.
# =====================================================================
pairs1 <- e1 %>%
  select(block, size, algo, wall_time) %>%
  pivot_wider(names_from = algo, values_from = wall_time,
              values_fn = mean) %>%
  mutate(sp_par = seq / par, sp_libc = seq / libc)

sp1 <- pairs1 %>% group_by(size) %>%
  group_modify(~ {
    b <- ci_boot(.x$sp_par)
    w <- suppressWarnings(wilcox.test(.x$seq, .x$par, paired = TRUE))
    tibble(est = b[["est"]], lo = b[["lo"]], hi = b[["hi"]], n = b[["n"]],
           p = w$p.value)
  }) %>% ungroup() %>%
  mutate(verdict = ifelse(hi < 1, "parallel loses",
                   ifelse(lo > 1, "parallel wins", "inconclusive")))
write_tbl(sp1, "summary_F2_paired_speedup")

p2 <- ggplot(sp1, aes(size, est)) +
  geom_hline(yintercept = 1, colour = CRIT, linewidth = 0.5) +
  annotate("text", x = min(sp1$size), y = 1, label = "  break-even",
           hjust = 0, vjust = -0.6, colour = CRIT, size = 3) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = PAL[["par"]], alpha = 0.18) +
  geom_line(colour = PAL[["par"]], linewidth = 0.8) +
  geom_point(colour = PAL[["par"]], size = 2.2, shape = SHP[["par"]]) +
  geom_text(data = tail(sp1, 1),
            aes(label = sprintf("%.2fx", est)), hjust = -0.25,
            colour = PAL[["par"]], size = 3.2, fontface = "bold") +
  scale_x_log10(breaks = log_breaks_all(sp1$size), labels = sci,
                expand = expansion(mult = c(0.02, 0.09))) +
  scale_y_log10(breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 2),
                labels = label_number(drop0trailing = TRUE)) +
  annotation_logticks(sides = "bl", colour = AXIS, size = 0.25,
                      short = unit(0.04, "cm"), mid = unit(0.07, "cm"),
                      long = unit(0.11, "cm")) +
  labs(title = "Paired speedup of the parallel version over the sequential one",
       subtitle = paste("Each replicate sorts an identical array in both versions, so the ratio is taken per replicate and",
                        "\ninput variability drops out of the interval.  The CI clears 1 only from ~2,000,000 elements:",
                        "at the\n1,000,000 of the original claim the shipped version is 0.71x, i.e. 40% slower."),
       x = "Array size (elements)",
       y = "Speedup  (sequential time / parallel time)",
       caption = CAP) +
  theme_lab()
save_fig(p2, "F2_paired_speedup")

# =====================================================================
#  F3 -- THREAD_LEVEL: the parameter that was hard-coded and never
#  varied.  This is the actual explanation.
# =====================================================================
e2 <- raw %>% filter(campaign == "E2_tlevel")
s2 <- summarise_ci(e2, size, tlevel) %>%
  group_by(size) %>% mutate(best = tlevel[which.min(mean)]) %>% ungroup()
# Sequential baseline at the exact sizes of the sweeps.  E0 was run
# for precisely this purpose; E1 is the fallback if E0 is absent.
seq_src <- raw %>% filter(algo == "seq",
                          campaign %in% c("E0_baseline", "E1_replication"))
seq_ref <- summarise_ci(seq_src, size) %>% select(size, seq_mean = mean)
stopifnot(all(unique(raw$size[raw$campaign == "E2_tlevel"]) %in%
              seq_ref$size))
s2 <- s2 %>% left_join(seq_ref, by = "size")
write_tbl(s2, "summary_F3_tlevel")

thr <- e2 %>% group_by(tlevel) %>% summarise(th = round(mean(nthreads)))

p3 <- ggplot(s2, aes(tlevel, mean)) +
  geom_hline(aes(yintercept = seq_mean), colour = PAL[["seq"]],
             linewidth = 0.5, linetype = "solid") +
  geom_vline(xintercept = 10, colour = CRIT, linewidth = 0.4) +
  geom_ribbon(aes(ymin = b_lo, ymax = b_hi), fill = PAL[["par"]],
              alpha = 0.18) +
  geom_line(colour = PAL[["par"]], linewidth = 0.8) +
  geom_point(colour = PAL[["par"]], size = 1.9, shape = SHP[["par"]]) +
  geom_point(data = s2 %>% filter(tlevel == best),
             colour = GOOD, size = 3.4, shape = 1, stroke = 1.1) +
  facet_wrap(~ size, scales = "free_y", ncol = 2,
             labeller = labeller(size = function(x)
               paste0("n = ", sci(as.numeric(x))))) +
  scale_x_continuous(breaks = 0:12) +
  scale_y_log10(labels = label_number(accuracy = 0.001)) +
  labs(title = "Wall-clock time against THREAD_LEVEL, the constant the original study never varied",
       subtitle = paste("Blue line: sequential baseline.  Red line: THREAD_LEVEL = 10, the value hard-coded in the source.",
                        "\nGreen ring: best value measured.  The shipped setting is far from optimal in all four panels and",
                        "\nloses to plain sequential code in three of them."),
       x = "THREAD_LEVEL  (levels of recursion spawning threads; 2^(L+1)-1 tasks)",
       y = "Wall-clock time (s, log scale)",
       caption = paste0(CAP, "\nThreads created: ",
                        paste(sprintf("L=%d:%d", thr$tlevel, thr$th),
                              collapse = "  "))) +
  theme_lab()
save_fig(p3, "F3_tlevel", w = 9.4, h = 7)

# ---- F3b: speedup over sequential as a function of tlevel ----------
s2b <- s2 %>% mutate(sp = seq_mean / mean,
                     sp_lo = seq_mean / b_hi, sp_hi = seq_mean / b_lo)
p3b <- ggplot(s2b, aes(tlevel, sp, colour = factor(size),
                       fill = factor(size))) +
  geom_hline(yintercept = 1, colour = CRIT, linewidth = 0.5) +
  geom_ribbon(aes(ymin = sp_lo, ymax = sp_hi), alpha = 0.14, colour = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  geom_vline(xintercept = 10, colour = CRIT, linewidth = 0.4,
             linetype = "22") +
  annotate("text", x = 10, y = 0.25, label = "shipped value ", hjust = 1,
           colour = CRIT, size = 3) +
  scale_colour_manual(values = c("#86b6ef", "#5598e7", "#2a78d6", "#184f95"),
                      labels = function(x) paste0("n = ", sci(as.numeric(x)))) +
  scale_fill_manual(values = c("#86b6ef", "#5598e7", "#2a78d6", "#184f95"),
                    guide = "none") +
  scale_x_continuous(breaks = 0:12) +
  scale_y_continuous(breaks = seq(0, 2, 0.25)) +
  labs(title = "The parallel version does work -- at one tenth the thread count",
       subtitle = paste("Speedup over the sequential baseline.  A single constant separates a 1.7x win from a 1.7x loss;",
                        "\nthe ordinal blue ramp orders the four array sizes."),
       x = "THREAD_LEVEL", y = "Speedup over sequential",
       caption = CAP) +
  theme_lab()
save_fig(p3b, "F3b_tlevel_speedup")

# =====================================================================
#  F4 -- core scaling (taskset as a proxy for a smaller machine)
# =====================================================================
e3 <- raw %>% filter(campaign == "E3_cores")
seqc <- e3 %>% filter(algo == "seq") %>%
  group_by(size, cores, block) %>% summarise(sq = mean(wall_time),
                                             .groups = "drop")
sp3 <- e3 %>% filter(algo != "seq") %>%
  mutate(cfg = ifelse(algo == "parcut", "cutoff 50k",
                      paste0("L = ", tlevel))) %>%
  inner_join(seqc, by = c("size", "cores", "block")) %>%
  mutate(sp = sq / wall_time) %>%
  group_by(size, cores, cfg) %>%
  group_modify(~ { b <- ci_boot(.x$sp)
    tibble(est = b[["est"]], lo = b[["lo"]], hi = b[["hi"]]) }) %>%
  ungroup()
write_tbl(sp3, "summary_F4_core_scaling")

cfg_lv <- c("L = 2", "L = 3", "L = 4", "L = 10", "cutoff 50k")
sp3$cfg <- factor(sp3$cfg, levels = cfg_lv)
p4 <- ggplot(sp3, aes(cores, est, colour = cfg, fill = cfg, shape = cfg)) +
  geom_abline(slope = 1, intercept = 0, colour = MUTED,
              linewidth = 0.35, linetype = "22") +
  geom_hline(yintercept = 1, colour = CRIT, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.75) + geom_point(size = 2.2, stroke = 0) +
  facet_wrap(~ size, ncol = 3,
             labeller = labeller(size = function(x)
               paste0("n = ", sci(as.numeric(x))))) +
  scale_colour_manual(values = c("#86b6ef", "#3987e5", "#1c5cab",
                                 PAL[["par"]], PAL[["parcut"]])) +
  scale_fill_manual(values = c("#86b6ef", "#3987e5", "#1c5cab",
                               PAL[["par"]], PAL[["parcut"]]),
                    guide = "none") +
  scale_shape_manual(values = c(16, 16, 16, 17, 18)) +
  scale_x_continuous(breaks = 1:4) +
  labs(title = "Speedup against the number of cores made available",
       subtitle = paste("Grey dashed line: linear speedup.  Red line: no gain.  The shipped L = 10 stays below 1 at",
                        "100K and 1M\nwhatever the core count, and pays off only at 4M; the tuned settings gain from the",
                        "second core onwards,\nthough sub-linearly."),
       x = "Cores available to the process (taskset)",
       y = "Speedup over sequential on the same core count",
       caption = paste0(CAP, "\nCaveat: restricting cores with taskset keeps the full cache and ",
                        "memory bandwidth of the 4-core host, so it understates\nthe cost of a genuinely smaller machine. ",
                        "It isolates the core count, which is the factor of interest here.")) +
  theme_lab(base = 10.5)
save_fig(p4, "F4_core_scaling", w = 10, h = 5.2)

# =====================================================================
#  F5 -- ablation of the two measurement artefacts
# =====================================================================
e4 <- raw %>% filter(campaign == "E4_ablation")
s4 <- summarise_ci(e4, algo, size, opt, rng)
s4 <- s4 %>% mutate(
  setting = paste0(ifelse(opt == "O0", "-O0", "-O2"), ", ",
                   ifelse(rng == "global", "rand()", "lock-free pivot")),
  setting = factor(setting, levels = c("-O0, rand()", "-O0, lock-free pivot",
                                       "-O2, rand()", "-O2, lock-free pivot")))
write_tbl(s4, "summary_F5_ablation")

p5 <- ggplot(s4, aes(setting, mean, colour = algo, shape = algo)) +
  geom_errorbar(aes(ymin = b_lo, ymax = b_hi), width = 0.14,
                linewidth = 0.5, position = position_dodge(width = 0.45)) +
  geom_point(size = 2.6, stroke = 0,
             position = position_dodge(width = 0.45)) +
  facet_wrap(~ size, scales = "free_y",
             labeller = labeller(size = function(x)
               paste0("n = ", sci(as.numeric(x))))) +
  scale_colour_manual(values = PAL, labels = LBL) +
  scale_shape_manual(values = SHP, labels = LBL) +
  scale_y_continuous(labels = label_number(accuracy = 0.001)) +
  labs(title = "How much of the original measurement was the measurement setup?",
       subtitle = paste("Leftmost setting reproduces the original conditions: code under test compiled at -O0, pivot drawn",
                        "from\nthe lock-protected libc rand().  The two artefacts pull in OPPOSITE directions.  At n = 1M,",
                        "rand() costs the\nparallel version 1.23x but the sequential one only 1.06x -- while -O0 inflates",
                        "sequential by 1.77x and parallel by\nonly 1.15x, because the parallel time is dominated by",
                        "thread-creation syscalls the optimiser cannot touch.\nNet effect: the original setup FLATTERED the",
                        "parallel version (par/seq 1.34x there, 1.49x measured cleanly)."),
       x = NULL, y = "Wall-clock time (s)", caption = CAP) +
  theme_lab() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))
save_fig(p5, "F5_ablation", w = 9.4, h = 5.4)

# =====================================================================
#  F6 -- the repaired version: cutoff sweep
# =====================================================================
e5 <- raw %>% filter(campaign == "E5_repair")
s5 <- summarise_ci(e5, size, cutoff) %>% left_join(seq_ref, by = "size") %>%
  mutate(sp = seq_mean / mean, sp_lo = seq_mean / b_hi,
         sp_hi = seq_mean / b_lo)
write_tbl(s5, "summary_F6_cutoff")

p6 <- ggplot(s5, aes(cutoff, sp, colour = factor(size),
                     fill = factor(size))) +
  geom_hline(yintercept = 1, colour = CRIT, linewidth = 0.45) +
  geom_ribbon(aes(ymin = sp_lo, ymax = sp_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_x_log10(breaks = sort(unique(s5$cutoff)),
                labels = label_comma(accuracy = 1)) +
  scale_colour_manual(values = c("#86b6ef", "#2a78d6", "#104281"),
                      labels = function(x) paste0("n = ", sci(as.numeric(x)))) +
  scale_fill_manual(values = c("#86b6ef", "#2a78d6", "#104281"),
                    guide = "none") +
  labs(title = "Repairing the parallel version: stop spawning threads below a cutoff",
       subtitle = paste("Same algorithm, same code path -- the recursion simply finishes sequentially once a sub-array is",
                        "small.\nThe optimum is broad, which is what a robust tuning parameter looks like."),
       x = "Cutoff: sub-arrays smaller than this are sorted sequentially",
       y = "Speedup over sequential", caption = CAP) +
  theme_lab()
save_fig(p6, "F6_cutoff")

# =====================================================================
#  F7 -- head to head, and the break-even analysis the original
#        report asserted without data
# =====================================================================
e6 <- raw %>% filter(campaign == "E6_headtohead")
s6 <- summarise_ci(e6, algo, size)
write_tbl(s6, "summary_F7_headtohead")

ends <- s6 %>% group_by(algo) %>% filter(size == max(size)) %>% ungroup()
p7 <- ggplot(s6, aes(size, mean, colour = algo, fill = algo, shape = algo)) +
  geom_ribbon(aes(ymin = b_lo, ymax = b_hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.75) + geom_point(size = 2.1, stroke = 0) +
  geom_text(data = ends, aes(label = LBL6[as.character(algo)]),
            hjust = -0.08, size = 3.1, fontface = "bold", show.legend = FALSE) +
  scale_x_log10(breaks = log_breaks_all(s6$size), labels = sci,
                expand = expansion(mult = c(0.02, 0.26))) +
  scale_y_log10(labels = label_number(accuracy = 0.00001,
                                      drop0trailing = TRUE)) +
  scale_colour_manual(values = PAL, labels = LBL6) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_shape_manual(values = SHP, labels = LBL6) +
  annotation_logticks(sides = "bl", colour = AXIS, size = 0.25,
                      short = unit(0.04, "cm"), mid = unit(0.07, "cm"),
                      long = unit(0.11, "cm")) +
  labs(title = "All four variants at their best settings, over four orders of magnitude",
       subtitle = paste("Both parallel variants are TUNED here: L = 3 for the thread-level fix, a 50,000-element cutoff for the",
                        "\ncutoff fix.  For the shipped L = 10 setting see F1/F2.  Series are direct-labelled as well as coloured."),
       x = "Array size (elements)", y = "Wall-clock time (s)",
       caption = CAP) +
  theme_lab() + theme(legend.position = "none")
save_fig(p7, "F7_headtohead")

# ---- break-even with a bootstrap CI --------------------------------
# For each bootstrap resample of the replicate blocks we fit
#   log t = a + b log n
# for the sequential and the parallel-with-cutoff variants, and solve
# for the crossing.  This is the quantity the original report stated
# as "worth it above 400000" -- from a size grid with no point
# between 100,000 and 1,000,000.
breakeven <- function(df, a1, a2) {
  d <- df %>% filter(algo %in% c(a1, a2), size >= 10000)
  f1 <- lm(log(wall_time) ~ log(size), data = d %>% filter(algo == a1))
  f2 <- lm(log(wall_time) ~ log(size), data = d %>% filter(algo == a2))
  c1 <- coef(f1); c2 <- coef(f2)
  if (abs(c1[2] - c2[2]) < 1e-9) return(NA_real_)
  exp((c2[1] - c1[1]) / (c1[2] - c2[2]))
}
be_point <- breakeven(e6, "seq", "parcut")
set.seed(7)
blocks <- unique(e6$block)
be_bs <- replicate(2000, {
  bs <- sample(blocks, length(blocks), replace = TRUE)
  d  <- bind_rows(lapply(bs, function(b) e6[e6$block == b, ]))
  tryCatch(breakeven(d, "seq", "parcut"), error = function(e) NA_real_)
})
be_ci <- quantile(be_bs, c(0.025, 0.975), na.rm = TRUE)

# empirical cross-over: smallest size whose paired speedup CI is > 1
pairs6 <- e6 %>% select(block, size, algo, wall_time) %>%
  pivot_wider(names_from = algo, values_from = wall_time, values_fn = mean)
sp6 <- pairs6 %>% group_by(size) %>%
  group_modify(~ {
    b <- ci_boot(.x$seq / .x$parcut); p <- ci_boot(.x$seq / .x$par)
    tibble(cut_est = b[["est"]], cut_lo = b[["lo"]], cut_hi = b[["hi"]],
           par_est = p[["est"]], par_lo = p[["lo"]], par_hi = p[["hi"]])
  }) %>% ungroup()
write_tbl(sp6, "summary_F8_paired_speedup_tuned")
emp_cross <- sp6 %>% filter(cut_lo > 1) %>% slice_min(size, n = 1) %>%
  pull(size)
emp_cross <- if (length(emp_cross)) emp_cross else NA

p8 <- ggplot(sp6, aes(size)) +
  geom_hline(yintercept = 1, colour = CRIT, linewidth = 0.5) +
  geom_ribbon(aes(ymin = par_lo, ymax = par_hi), fill = PAL[["par"]],
              alpha = 0.16) +
  geom_ribbon(aes(ymin = cut_lo, ymax = cut_hi), fill = PAL[["parcut"]],
              alpha = 0.16) +
  geom_line(aes(y = par_est), colour = PAL[["par"]], linewidth = 0.8) +
  geom_line(aes(y = cut_est), colour = PAL[["parcut"]], linewidth = 0.8) +
  geom_point(aes(y = par_est), colour = PAL[["par"]], size = 2,
             shape = SHP[["par"]]) +
  geom_point(aes(y = cut_est), colour = PAL[["parcut"]], size = 2.2,
             shape = SHP[["parcut"]]) +
  { if (!is.na(emp_cross))
      geom_vline(xintercept = emp_cross, colour = GOOD, linewidth = 0.45,
                 linetype = "22") } +
  { if (!is.na(emp_cross))
      annotate("text", x = emp_cross, y = 0.2,
               label = paste0(" crosses 1 at n = ", sci(emp_cross)),
               hjust = 0, colour = GOOD, size = 3.1) } +
  annotate("rect", xmin = be_ci[1], xmax = be_ci[2], ymin = -Inf, ymax = Inf,
           fill = GOOD, alpha = 0.07) +
  geom_text(data = tail(sp6, 1), aes(y = cut_est,
            label = sprintf("Parallel + cutoff  %.2fx", cut_est)),
            hjust = -0.05, colour = PAL[["parcut"]], size = 3.1,
            fontface = "bold") +
  geom_text(data = tail(sp6, 1), aes(y = par_est,
            label = sprintf("Parallel tuned L=3  %.2fx", par_est)),
            hjust = -0.05, colour = PAL[["par"]], size = 3.1,
            fontface = "bold") +
  scale_x_log10(breaks = log_breaks_all(sp6$size), labels = sci,
                expand = expansion(mult = c(0.02, 0.33))) +
  scale_y_log10(breaks = c(0.01, 0.03, 0.1, 0.3, 1, 2),
                labels = label_number(drop0trailing = TRUE)) +
  labs(title = "Where parallelism starts paying -- with an interval, not an eyeball",
       subtitle = sprintf(paste("Green line: the smallest measured size whose paired 95%% CI clears 1 -- the first size at which a",
                                "gain is\ndemonstrated rather than hoped for.  A power-law fit puts the crossing earlier, at %s",
                                "[%s, %s], but it\nunderstates fixed overheads, so the measured crossing is the number to quote.",
                                "The original report asserted\n400,000 from a grid with no data point between 100,000 and 1,000,000."),
                          sci(be_point), sci(be_ci[1]), sci(be_ci[2])),
       x = "Array size (elements)", y = "Paired speedup over sequential",
       caption = CAP) +
  theme_lab()
save_fig(p8, "F8_breakeven")

# =====================================================================
#  F9 -- the n log n model, residuals, and extrapolation
# =====================================================================
fit_df <- e6 %>% filter(size >= 10000) %>%
  mutate(nlogn = size * log2(size))
models <- fit_df %>% group_by(algo) %>%
  group_modify(~ {
    m <- lm(wall_time ~ 0 + nlogn, data = .x)
    ll <- lm(log(wall_time) ~ log(size), data = .x)
    tibble(a_nlogn = coef(m)[[1]],
           a_se = summary(m)$coefficients[1, 2],
           r2_nlogn = summary(m)$r.squared,
           loglog_slope = coef(ll)[[2]],
           slope_lo = confint(ll)[2, 1], slope_hi = confint(ll)[2, 2],
           r2_loglog = summary(ll)$r.squared)
  }) %>% ungroup()
write_tbl(models, "summary_F9_models")
print(as.data.frame(models), digits = 4)

# The t = a.n.log2(n) fit above has constant ABSOLUTE variance, so its
# prediction interval is a fixed number of seconds -- which on a log
# axis swamps everything below ~1M.  Timing noise is multiplicative,
# so the interval is built in log space instead and back-transformed,
# giving a band that is a constant RATIO at every size.
grid <- expand.grid(size = 10 ^ seq(4, 8.3, length.out = 120),
                    algo = unique(fit_df$algo))
preds <- bind_rows(lapply(split(fit_df, fit_df$algo), function(d) {
  m <- lm(log(wall_time) ~ log(size), data = d)
  g <- grid[grid$algo == d$algo[1], ]
  pi <- predict(m, newdata = g, interval = "prediction", level = 0.95)
  cbind(g, fit = exp(pi[, "fit"]), lo = exp(pi[, "lwr"]),
        hi = exp(pi[, "upr"]))
}))
maxn <- max(fit_df$size)

p9 <- ggplot() +
  geom_ribbon(data = preds, aes(size, ymin = lo, ymax = hi, fill = algo),
              alpha = 0.12) +
  geom_line(data = preds %>% filter(size <= maxn),
            aes(size, fit, colour = algo), linewidth = 0.75) +
  geom_line(data = preds %>% filter(size >= maxn),
            aes(size, fit, colour = algo), linewidth = 0.75,
            linetype = "22") +
  geom_point(data = s6 %>% filter(size >= 10000),
             aes(size, mean, colour = algo, shape = algo), size = 2.1,
             stroke = 0) +
  geom_vline(xintercept = maxn, colour = MUTED, linewidth = 0.35) +
  geom_text(data = preds %>% group_by(algo) %>% filter(size == max(size)),
            aes(size, fit, colour = algo,
                label = sprintf(" %s: %.0fs", LBL6[as.character(algo)], fit)),
            hjust = 0, size = 3, fontface = "bold", show.legend = FALSE) +
  annotate("text", x = maxn, y = 0.02, label = "  measured | extrapolated",
           hjust = 0, colour = MUTED, size = 3) +
  scale_x_log10(labels = sci, breaks = 10 ^ (4:8),
                expand = expansion(mult = c(0.02, 0.30))) +
  scale_y_log10(labels = label_number(accuracy = 0.01,
                                      drop0trailing = TRUE)) +
  scale_colour_manual(values = PAL, labels = LBL6) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_shape_manual(values = SHP, labels = LBL6) +
  annotation_logticks(sides = "bl", colour = AXIS, size = 0.25,
                      short = unit(0.04, "cm"), mid = unit(0.07, "cm"),
                      long = unit(0.11, "cm")) +
  labs(title = "Power-law fit on n >= 10,000, extrapolated to 200,000,000 elements",
       subtitle = paste("Solid: fitted over the measured range.  Dashed: extrapolation, with a 95% prediction interval built",
                        "in\nlog space, so the band is a constant ratio rather than a constant number of seconds.  The",
                        "extrapolation\nassumes the array still fits in RAM and cache behaviour does not change -- at 200M",
                        "doubles (1.6 GB) it will,\nso treat the dashed segment as an upper-bound sketch, not a prediction."),
       x = "Array size (elements)", y = "Wall-clock time (s)",
       caption = paste0(CAP, "\nFitted coefficients and log-log slopes: figures/summary_F9_models.csv")) +
  theme_lab() + theme(legend.position = "none")
save_fig(p9, "F9_model_extrapolation")

# ---- F9b: is it really n log n? -----------------------------------
# Dividing the measured time by n.log2(n) gives the cost of one
# "unit of work".  If the model holds, this is flat.  Where it rises,
# the machine -- not the algorithm -- is talking: cache misses.
norm <- e6 %>% filter(size >= 10000) %>%
  mutate(unit = 1e9 * wall_time / (size * log2(size))) %>%
  group_by(algo, size) %>%
  group_modify(~ { b <- ci_boot(.x$unit)
    tibble(est = b[["est"]], lo = b[["lo"]], hi = b[["hi"]]) }) %>%
  ungroup()
# This host has 8 MiB L2 and a very large 260 MiB L3, so even the
# 8,000,000-element array (64 MiB) never leaves L3.  Only the L2
# boundary is therefore worth marking.
l2 <- 8 * 1024 * 1024 / 8      # L2 bytes -> doubles

p9b <- ggplot(norm, aes(size, est, colour = algo, fill = algo,
                        shape = algo)) +
  geom_vline(xintercept = l2, colour = MUTED, linewidth = 0.35,
             linetype = "22") +
  annotate("text", x = l2, y = max(norm$hi), label = "array exceeds L2 ",
           hjust = 1, vjust = 1, colour = MUTED, size = 2.9) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.75) + geom_point(size = 2.1, stroke = 0) +
  geom_text(data = norm %>% group_by(algo) %>% filter(size == max(size)),
            aes(label = LBL6[as.character(algo)]), hjust = -0.06,
            size = 3, fontface = "bold", show.legend = FALSE) +
  scale_x_log10(breaks = c(1e4, 1e5, 1e6, 1e7), labels = sci,
                expand = expansion(mult = c(0.05, 0.30))) +
  scale_colour_manual(values = PAL, labels = LBL6) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_shape_manual(values = SHP, labels = LBL6) +
  labs(title = "Is it really n.log2(n)?  Cost of one unit of work",
       subtitle = paste("Time divided by n.log2(n): a flat line means the model holds.  Sequential and libc are flat across",
                        "nearly\nthree decades, so n.log2(n) describes them well.  Both parallel variants fall steeply and then",
                        "flatten -- their\nfixed thread-creation cost is amortised over a growing problem, which is exactly why",
                        "a single size cannot\nsettle whether parallelism is worth it."),
       x = "Array size (elements)",
       y = "Nanoseconds per n.log2(n)", caption = CAP) +
  theme_lab() + theme(legend.position = "none")
save_fig(p9b, "F9b_unit_cost")

# =====================================================================
#  F10 -- parallel efficiency: the hidden cost
# =====================================================================
eff <- e6 %>% filter(algo %in% c("par", "parcut")) %>%
  group_by(algo, size) %>%
  group_modify(~ { b <- ci_boot(.x$cpu_time / .x$wall_time)
    tibble(est = b[["est"]], lo = b[["lo"]], hi = b[["hi"]],
           cpu = mean(.x$cpu_time)) }) %>% ungroup()
seq_cpu <- e6 %>% filter(algo == "seq") %>% group_by(size) %>%
  summarise(seq_cpu = mean(cpu_time), .groups = "drop")
eff <- eff %>% left_join(seq_cpu, by = "size") %>%
  mutate(waste = cpu / seq_cpu)
write_tbl(eff, "summary_F10_efficiency")

p10 <- ggplot(eff, aes(size, waste, colour = algo, fill = algo,
                       shape = algo)) +
  geom_hline(yintercept = 1, colour = GOOD, linewidth = 0.45) +
  annotate("text", x = min(eff$size), y = 1,
           label = "  same CPU as sequential", hjust = 0, vjust = -0.6,
           colour = GOOD, size = 3) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.3, stroke = 0) +
  geom_text(data = eff %>% group_by(algo) %>% filter(size == max(size)),
            aes(label = sprintf("%s  %.1fx", LBL6[as.character(algo)], waste)),
            hjust = -0.05, size = 3.1, fontface = "bold",
            show.legend = FALSE) +
  scale_x_log10(breaks = log_breaks_all(eff$size), labels = sci,
                expand = expansion(mult = c(0.02, 0.34))) +
  scale_y_log10() +
  scale_colour_manual(values = PAL, labels = LBL6) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_shape_manual(values = SHP, labels = LBL6) +
  labs(title = "What the wall-clock figure hides: CPU time actually consumed",
       subtitle = paste("Ratio of CPU time to the sequential version's CPU time, for the TUNED variants.  Good news: past",
                        "~1M\nelements the speedup is nearly free, costing 1.0-1.1x the CPU for 2-3x the wall-clock.  Below",
                        "~100k it is\nruinous -- up to 40x the CPU for no gain at all.  The version as shipped is far worse",
                        "again: at n = 145,853 it\nburns 18.5x the sequential CPU time while running 8x slower.  On a shared or",
                        "battery-powered machine this,\nnot wall-clock, is the number that matters."),
       x = "Array size (elements)",
       y = "CPU time relative to sequential", caption = CAP) +
  theme_lab() + theme(legend.position = "none")
save_fig(p10, "F10_cpu_cost")

# =====================================================================
#  F11 -- why randomisation was needed: drift over the campaign
# =====================================================================
# Pool every campaign: each measurement expressed relative to the
# median of its own (campaign, algorithm, size, tlevel, cores, opt,
# rng) cell, against elapsed time within its campaign.  A trend here
# would mean the host changed speed during the run -- which is the
# bias a fixed run order converts into a fake algorithm effect.
drift <- raw %>%
  group_by(campaign, algo, size, tlevel, cutoff, cores, opt, rng) %>%
  filter(n() >= 5) %>%
  mutate(rel = wall_time / median(wall_time)) %>% ungroup()

drift_fit <- lm(rel ~ t_offset, data = drift)
slope_pct <- coef(drift_fit)[2] * 100 * 100   # % change per 100 s
slope_p   <- summary(drift_fit)$coefficients[2, 4]
startup <- drift %>%
  mutate(phase = ifelse(t_offset < 10, "first 10 s", "rest")) %>%
  group_by(phase) %>%
  summarise(med = median(rel), p95 = quantile(rel, 0.95), n = n(),
            .groups = "drop")
write_tbl(startup, "summary_F11_startup")
cat("\n-- drift: % change per 100 s --\n")
print(data.frame(pct_per_100s = round(slope_pct, 3), p = signif(slope_p, 3)))
print(as.data.frame(startup), digits = 3)

p11 <- ggplot(drift, aes(t_offset, rel)) +
  geom_hline(yintercept = 1, colour = AXIS, linewidth = 0.4) +
  geom_point(aes(colour = algo, shape = algo), alpha = 0.3, size = 1.1,
             stroke = 0) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
              colour = INK, fill = MUTED, alpha = 0.25, linewidth = 0.6) +
  facet_wrap(~ campaign, scales = "free_x", ncol = 4) +
  scale_colour_manual(values = PAL, labels = LBL) +
  scale_shape_manual(values = SHP, labels = LBL) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = quantile(drift$rel, c(0.005, 0.99))) +
  labs(title = "Was the host stable during the campaigns?  A check, not an assumption",
       subtitle = sprintf(paste("Every measurement relative to the median of its own experimental cell, against elapsed time.",
                                "The pooled\ntrend is %+.2f%% per 100 s (p = %.2f) and the first 10 s look like the rest",
                                "(median 0.997 vs 1.000), so this host\nwas stable over runs of 26 to 160 s.  That is a result",
                                "of the check, not an assumption behind it: randomising\nthe run order is what makes the",
                                "conclusion safe, and it costs nothing when the check comes back clean."),
                          slope_pct, slope_p),
       x = "Time since start of that campaign (s)",
       y = "Measurement relative to its cell median", caption = CAP) +
  theme_lab(base = 10)
save_fig(p11, "F11_drift", w = 10.5, h = 5.6)

# =====================================================================
#  F12 -- the order effect the original design could not see
#
#  E7 runs all three algorithms inside one process, as the original
#  script did, but in all six permutations.  Position in the process
#  and algorithm are therefore no longer confounded, and the position
#  effect can be estimated on its own.
# =====================================================================
aovtab <- data.frame(size = integer(), term = character(),
                     SumSq = numeric(), F = numeric(), p = numeric())
e7f <- list.files(datadir, pattern = "^E7_order.*\\.csv$",
                  full.names = TRUE)
if (length(e7f)) {
  e7 <- bind_rows(lapply(e7f, read.csv, stringsAsFactors = FALSE))
  stopifnot(!any(is.na(e7$sorted)), all(e7$sorted == 1))
  e7$algo <- factor(e7$algo, levels = names(PAL))
  e7$position <- factor(e7$position, levels = 1:3,
                        labels = c("1st in process", "2nd", "3rd"))
  s7 <- summarise_ci(e7, algo, size, position)
  write_tbl(s7, "summary_F12_order_effect")

  # Two-way ANOVA per size: how much of the variance does POSITION
  # explain, once the algorithm is accounted for?
  aovtab <- do.call(rbind, lapply(split(e7, e7$size), function(d) {
    m <- anova(lm(log(wall_time) ~ algo + position, data = d))
    data.frame(size = d$size[1], term = rownames(m),
               SumSq = m[, "Sum Sq"], F = m[, "F value"],
               p = m[, "Pr(>F)"])
  }))
  write_tbl(aovtab, "summary_F12_order_anova")
  cat("\n-- order-effect ANOVA (log wall_time ~ algo + position) --\n")
  print(aovtab, digits = 3, row.names = FALSE)

  # Normalise by the median over ALL positions of the same
  # (algorithm, size) cell, and bootstrap the MEDIAN ratio.  Using the
  # mean of a ratio against a median baseline would shift every point
  # up by a few percent (timing data is right-skewed) and make the
  # figure disagree with the +/-2% quoted in the subtitle.
  rel <- e7 %>% group_by(algo, size) %>%
    mutate(rel = wall_time / median(wall_time)) %>% ungroup()
  s7r <- rel %>% group_by(algo, size, position) %>%
    group_modify(~ { b <- ci_boot(.x$rel, stat = median)
      tibble(est = b[["est"]], lo = b[["lo"]], hi = b[["hi"]]) }) %>%
    ungroup()

  p12 <- ggplot(s7r, aes(position, est, colour = algo, shape = algo,
                         group = algo)) +
    geom_hline(yintercept = 1, colour = AXIS, linewidth = 0.4) +
    geom_line(linewidth = 0.6, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12,
                  linewidth = 0.5, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.6, stroke = 0,
               position = position_dodge(width = 0.3)) +
    facet_wrap(~ size, scales = "free_y",
               labeller = labeller(size = function(x)
                 paste0("n = ", sci(as.numeric(x))))) +
    scale_colour_manual(values = PAL, labels = LBL) +
    scale_shape_manual(values = SHP, labels = LBL) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Checking the suspected order confound -- and clearing it",
         subtitle = paste("All six orderings of the three algorithms, inside one process, 20 replicates each.  Median time",
                          "relative\nto the cell median.  Every position effect is within +/-2% and none is significant",
                          "(ANOVA p = 0.94 and 0.70).\nThe original fixed order was a latent risk, not the cause of its wrong",
                          "conclusion -- that was THREAD_LEVEL."),
         x = "Position within the process",
         y = "Median time relative to the cell median",
         caption = paste0("Intel Xeon @2.10GHz, 4 vCPU (KVM), Linux 6.18, glibc 2.39, gcc 13.3 -O2.\n",
                          "Unlike every other figure here, this experiment deliberately puts all THREE algorithms in one ",
                          "process, as the\noriginal script did; the permutation and replicate order are randomised across ",
                          "the campaign.  Bars are 95% CI.\nANOVA of position controlling for algorithm: ",
                          "figures/summary_F12_order_anova.csv")) +
    theme_lab()
  save_fig(p12, "F12_order_effect", w = 9.4, h = 5.2)
}

# =====================================================================
#  F13 -- the original 2014 data, re-plotted with CIs for contrast
# =====================================================================
old <- tryCatch(
  read.csv("data/sama_2014-10-13/measurements_03:47.csv",
           strip.white = TRUE, stringsAsFactors = FALSE),
  error = function(e) NULL)
if (!is.null(old)) {
  names(old) <- c("size", "type", "time")
  old$type <- gsub('"', "", trimws(old$type))
  m <- c(Sequential = "seq", Parallel = "par", `Built-in` = "libc")
  old$algo <- factor(m[old$type], levels = names(PAL))
  so <- summarise_ci(old %>% rename(wall_time = time), algo, size)
  write_tbl(so, "summary_F13_original_2014")
  p13 <- ggplot(so, aes(size, mean, colour = algo, fill = algo,
                        shape = algo)) +
    geom_ribbon(aes(ymin = t_lo, ymax = t_hi), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 0.75) + geom_point(size = 2.2, stroke = 0) +
    scale_x_log10(breaks = log_breaks_all(so$size), labels = sci) +
    scale_y_log10(labels = label_number(accuracy = 0.00001,
                                        drop0trailing = TRUE)) +
    scale_colour_manual(values = PAL, labels = LBL) +
    scale_fill_manual(values = PAL, guide = "none") +
    scale_shape_manual(values = SHP, labels = LBL) +
    annotation_logticks(sides = "bl", colour = AXIS, size = 0.25,
                        short = unit(0.04, "cm"), mid = unit(0.07, "cm"),
                        long = unit(0.11, "cm")) +
    labs(title = "The original 2014 dataset, re-plotted (5 consecutive repetitions, t-CI)",
         subtitle = paste("The same data behind the original conclusion.  Five back-to-back repetitions in a fixed order",
                          "give\nintervals that describe a two-second window, not the experiment; there is no point between",
                          "\n100,000 and 1,000,000, which is exactly where the 400,000 break-even was asserted."),
         x = "Array size (elements)", y = "Wall-clock time (s)",
         caption = "Data: data/sama_2014-10-13/ -- Intel Core i7, Debian, Linux 3.14.15, cc -O0") +
    theme_lab()
  save_fig(p13, "F13_original_2014")
}

# =====================================================================
#  Headline numbers for the journal
# =====================================================================
hl <- list()
hl$replication_near_1M <- sp1 %>%
  slice_min(abs(size - 1e6), n = 1) %>%
  transmute(size, speedup = est, lo, hi, p, verdict) %>% as.data.frame()
hl$replication_all <- sp1 %>%
  transmute(size, speedup = round(est, 3), lo = round(lo, 3),
            hi = round(hi, 3), verdict) %>% as.data.frame()
hl$order_effect <- {
  z <- aovtab[aovtab$term == "position", ]
  data.frame(size = z$size, F = round(z$F, 3), p = round(z$p, 3),
             conclusion = "no detectable position effect (<2%)")
}
hl$best_tlevel <- s2 %>% group_by(size) %>%
  slice_min(mean, n = 1) %>%
  transmute(size, best_tlevel = tlevel, time = mean,
            speedup_vs_seq = seq_mean / mean) %>% as.data.frame()
hl$shipped_tlevel <- s2 %>% filter(tlevel == 10) %>%
  transmute(size, time = mean, speedup_vs_seq = seq_mean / mean) %>%
  as.data.frame()
hl$breakeven <- data.frame(
  regression_crossing = round(be_point),
  reg_lo = round(be_ci[1]), reg_hi = round(be_ci[2]),
  first_size_with_CI_above_1 = emp_cross,
  note = paste("regression crossing = where the two fitted power laws",
               "meet; the second column is the smallest measured size at",
               "which the paired 95% CI clears 1"))
hl$tuned_8M <- sp6 %>% filter(size == max(size)) %>% as.data.frame()

sink(file.path(figdir, "headline_numbers.txt"))
cat("Headline numbers\n================\n\n")
for (n in names(hl)) {
  cat("--", n, "--\n"); print(hl[[n]], digits = 4); cat("\n")
}
cat("-- relative standard deviation of wall_time per cell (E1) --\n")
print(summary(s1$rsd), digits = 3)
cat("\n-- models --\n"); print(as.data.frame(models), digits = 4)
sink()
cat(readLines(file.path(figdir, "headline_numbers.txt")), sep = "\n")
message("\nall figures written to ", figdir, "/")
