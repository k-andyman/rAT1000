# rAT1000

An R package for importing, analyzing, and visualizing OLSA (Oldenburg Sentence Test) measurements from the AT1000 audiometry system.

**Version 0.1.2** adds a rolling-window SRT50 maximum-likelihood estimate (MLE), alongside the cumulative MLE and the moving average of word intelligibility.

## Installation

```r
install.packages("remotes")  # only if required
remotes::install_github("k-andyman/rAT1000")
library(rAT1000)
```

To update an existing installation:

```r
remotes::install_github("k-andyman/rAT1000", force = TRUE, upgrade = "never")
```

Restart R after installation and check `packageVersion("rAT1000")`.

## Process measurements

```r
result <- process_olsa(
  "measurement.hd",
  slope = 0.15,
  MLE_rolling_window = 20,
  trim_aborted = TRUE,
  min_zero_run = 5
)

result$global              # Metadata and AT1000 results
result$MLE_rolling_window # Actual rolling MLE window length
result$trials              # Trial-level measurements and estimates
```

`result$trials` contains these derived columns:

| Column | Meaning |
| --- | --- |
| `SRT50_MLE` | Cumulative SRT50 MLE from trials 1 through *t* |
| `SRT50_sd` | Model-based standard error for the cumulative MLE |
| `rwMLE` | SRT50 MLE re-fitted to the most recent `MLE_rolling_window` trials |
| `rwSD` | Model-based standard error for the rolling-window MLE |

The rolling MLE starts at trial 20 by default (trials 1–19 are `NA`). At trial 21 the window contains trials 2–21. Fits with exclusively correct or incorrect words are returned as `NA` because a finite unbounded MLE does not exist.

These uncertainty columns are historically named `sd`, but mathematically represent **standard errors (SE)** rather than the observed standard deviation of thresholds.

## Plot measurements

```r
# Plot a processed measurement
plot_olsa(result)

# Alternatively, process and plot directly from a file
plot_olsa(
  "measurement.hd",
  intelligibility_moving_window = 8,
  MLE_rolling_window = 20,
  show_sd = TRUE
)

# Longer SRT50 recording with a 30-trial rolling fit
long_result <- process_olsa("long_measurement.hd", MLE_rolling_window = 30)
plot_olsa(long_result, intelligibility_moving_window = 10)
```

`plot_olsa()` produces four panels: cumulative and rolling SRT50 MLE trajectories with AT1000 estimates and optional ±1 SE curves; cumulative word intelligibility and its moving mean; sentence-score and word-position distributions; and an SRT50/results summary.

When plotting preprocessed results, `plot_olsa()` uses the **stored** rolling-window size. When plotting a `.hd` path directly, both windows can be set in the call. The old `moving_window` argument remains available as a backwards-compatible alias for `intelligibility_moving_window`.

### Example visualization

![Example of a four-panel OLSA plot](images/olsa_example.png)

*Example screenshot from an earlier plot iteration. The current version additionally displays the rolling-window SRT50 MLE in the upper-left panel and the summary.*

## Suspected aborted measurements

By default, `trim_aborted = TRUE` removes incomplete trailing records and a terminal run of at least `min_zero_run = 5` fully parsed trials with zero correct words, if earlier trials contain correct responses. This is a heuristic, not a guarantee that the measurement was aborted.

```r
result$global[, c(
  "trials_original", "trials_used",
  "trailing_rows_removed", "trailing_zero_count", "trailing_zero_trimmed"
)]

untrimmed <- process_olsa("measurement.hd", trim_aborted = FALSE)
```

The original `.hd` file is never modified. Summary intelligibility in the plot is computed from **retained trials** and can therefore differ from the value stored in the AT1000 header (especially with multi-list long tests or trimming).

## Method

Both SRT50 estimators use a binomial log-likelihood with the fixed logistic psychometric function:

\[
p(\text{correct}\mid L) = \operatorname{logit}^{-1}\!\left(4s(L-L_{50})\right), \quad s=0.15.
\]

Here `L` is the SNR of each trial. The slope `0.15` follows the `OldenburgerL50` setting in the AT1000 configuration used in this project; the software may use other fitting routines, such as `OldenburgerLX` with a variable slope. The model-based SE is calculated using likelihood curvature and assumes conditionally independent word responses.

A rolling-window MLE is not a moving average of earlier MLE values. It is a **new likelihood fit** using only the word scores and SNRs in the current window. With strongly overlapping windows, adjacent estimates are correlated; a visible trajectory alone does not establish a learning or habituation effect.

## Limitations and data protection

This package is for methodological research and exploratory analysis, not a clinically validated diagnostic tool. Do not upload identifiable patient `.hd` recordings to public GitHub repositories.

## License and author

MIT License © 2026 Andreas F. Kandolf.
