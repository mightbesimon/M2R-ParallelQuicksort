R Notebook
================
Pieleanu Mircea-Adrian
10/11/2026

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(scales)
```

``` r
# 1. Automatically load the newest CSV file from the data folder
csv_files <- list.files("data", pattern = "*.csv", full.names = TRUE)
latest_csv <- csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)][1]
cat("Loading data from:", latest_csv, "\n")
```

    ## Loading data from: data/measurements_large_mircea.csv

``` r
df <- read.csv(latest_csv, strip.white = TRUE)
df$Type <- factor(df$Type, levels = c("Sequential", "Parallel", "Built-in"))
```

``` r
# 2. Compute Mean & 95% Confidence Interval per Size and Algorithm
df_summary <- df %>%
  group_by(Size, Type) %>%
  summarise(
    N = n(),
    Mean = mean(Time),
    SD = sd(Time),
    SE = SD / sqrt(N),
    CI_lower = Mean - qt(0.975, df = N - 1) * SE,
    CI_upper = Mean + qt(0.975, df = N - 1) * SE,
    .groups = "drop"
  )
```

``` r
p <- ggplot(df_summary, aes(x = Size, y = Mean, color = Type, fill = Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, color = NA) +
  scale_x_log10(breaks = unique(df_summary$Size), labels = label_comma()) +
  scale_y_log10() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 12) +
  labs(
    title = "Quicksort Execution Time vs. Array Size",
    subtitle = "Mean execution time with 95% confidence intervals (30 randomized repetitions)",
    x = "Array Size N (log10 scale)",
    y = "Execution Time in Seconds (log10 scale)",
    color = "Algorithm",
    fill = "Algorithm"
  ) +theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilts numbers so they don't collide
  )
```

``` r
# Print plot to screen and save to disk
print(p)
```

![](journal2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave("quicksort_benchmark_plot.png", p, width = 8, height = 5, dpi = 300)
```

Experiment 2

``` r
# 1. Load the latest CSV file from the data folder
csv_files <- list.files("data", pattern = "*.csv", full.names = TRUE)
latest_csv <- csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)][1]
cat("Loading data from:", latest_csv, "\n")
```

    ## Loading data from: data/measurements_large_mircea.csv

``` r
df <- read.csv(latest_csv, strip.white = TRUE)
df$Type <- factor(df$Type, levels = c("Sequential", "Parallel", "Built-in"))
```

``` r
# 2. Compute Mean & 95% Confidence Interval across 30 repetitions
df_summary <- df %>%
  group_by(Size, Type) %>%
  summarise(
    N = n(),
    Mean = mean(Time),
    SD = sd(Time),
    SE = SD / sqrt(N),
    CI_lower = Mean - qt(0.975, df = N - 1) * SE,
    CI_upper = Mean + qt(0.975, df = N - 1) * SE,
    .groups = "drop"
  )
```

``` r
# 3. Generate ggplot with rotated labels
p2 <- ggplot(df_summary, aes(x = Size, y = Mean, color = Type, fill = Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, color = NA) +
  scale_x_log10(
    breaks = unique(df_summary$Size),
    labels = label_comma()
  ) +
  scale_y_log10() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 12) +
  labs(
    title = "Quicksort Execution Time: Large Scale (1M to 20M Elements)",
    subtitle = "Mean execution time with 95% confidence intervals (30 randomized repetitions)",
    x = "Array Size N (log10 scale)",
    y = "Execution Time in Seconds (log10 scale)",
    color = "Algorithm",
    fill = "Algorithm"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p2)
```

![](journal2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("quicksort_large_scale_plot.png", p2, width = 9, height = 5.5, dpi = 300)
```

``` r
# 1. Prepare data for model fitting (N >= 500k where parallel algorithm scales)
df_model_data <- df %>%
  filter(Size >= 500000) %>%
  mutate(Complexity = Size * log2(Size))
```

``` r
# 2. Fit O(N log N) linear regression models for each algorithm
fit_seq  <- lm(Time ~ Complexity, data = filter(df_model_data, Type == "Sequential"))
fit_par  <- lm(Time ~ Complexity, data = filter(df_model_data, Type == "Parallel"))
fit_libc <- lm(Time ~ Complexity, data = filter(df_model_data, Type == "Built-in"))
```

``` r
# 3. Create extrapolation grid up to N = 50,000,000 (50 Million elements)
extrap_sizes <- c(unique(df$Size), 30000000, 50000000)
grid_extrap  <- expand.grid(Size = extrap_sizes) %>%
  mutate(Complexity = Size * log2(Size))
```

``` r
# 4. Generate extrapolated predictions
pred_seq  <- data.frame(Size = grid_extrap$Size, Type = "Sequential", Time_Pred = predict(fit_seq, grid_extrap))
pred_par  <- data.frame(Size = grid_extrap$Size, Type = "Parallel",   Time_Pred = predict(fit_par, grid_extrap))
pred_libc <- data.frame(Size = grid_extrap$Size, Type = "Built-in",   Time_Pred = predict(fit_libc, grid_extrap))

df_extrap <- bind_rows(pred_seq, pred_par, pred_libc) %>%
  mutate(Type = factor(Type, levels = c("Sequential", "Parallel", "Built-in")))
```

``` r
# 5. Plot Measured Data alongside Continuous Extrapolation Lines
p_extrap <- ggplot() +
  geom_line(data = df_extrap, aes(x = Size, y = Time_Pred, color = Type), 
            linetype = "dashed", linewidth = 1) +
  geom_line(data = df_summary, aes(x = Size, y = Mean, color = Type), 
            linewidth = 1) +
  geom_point(data = df_summary, aes(x = Size, y = Mean, color = Type), 
             size = 2.5) +
  geom_ribbon(data = df_summary, aes(x = Size, ymin = CI_lower, ymax = CI_upper, fill = Type), 
              alpha = 0.2, color = NA) +
  scale_x_log10(labels = label_comma(), breaks = c(1000, 100000, 1000000, 10000000, 50000000)) +
  scale_y_log10() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 12) +
  labs(
    title = "Quicksort Scaling & Performance Extrapolation (up to N = 50M)",
    subtitle = "Solid lines: Measured 95% CI data | Dashed lines: O(N log N) extrapolated trend model",
    x = "Array Size N (log10 scale)",
    y = "Execution Time in Seconds (log10 scale)",
    color = "Algorithm",
    fill = "Algorithm"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_extrap)
```

![](journal2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggsave("quicksort_extrapolation_plot.png", p_extrap, width = 9, height = 5.5, dpi = 300)
```
