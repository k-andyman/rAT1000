# rAT1000

An R package for importing, analyzing, and visualizing OLSA (Oldenburg Sentence Test) measurements recorded with the AT1000 audiometry system.

The package provides tools to parse `.hd` measurement files and independently estimate the Speech Reception Threshold at 50% intelligibility (SRT50) using maximum likelihood estimation (MLE).

## Installation

Install directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("k-andyman/rAT1000")
```

Load the package:

```r
library(rAT1000)
```

## Usage

The package provides two main functions.

### 1. `process_olsa()`

Imports an AT1000 `.hd` file and computes cumulative SRT50 estimates.

```r
result <- process_olsa("measurement.hd")

# Measurement metadata
result$global

# Trial-level data
result$trials
```

The output contains two data frames:

- `global`: Measurement metadata and final AT1000 results.
- `trials`: Trial-level measurements, including word recognition, signal-to-noise ratios and SRT50 estimates.

Two additional columns are computed:

- `SRT50_MLE`: Cumulative maximum likelihood estimate of SRT50.
- `SRT50_sd`: Model-based standard error of the SRT50 estimate.

The estimator assumes a fixed psychometric slope (default: 0.15) and uses all observations up to each trial.

### 2. `plot_olsa()`

Visualizes the measurement results:

```r
plot_olsa(result)
```

Or directly from an `.hd` file:

```r
plot_olsa("measurement.hd")
```

The visualization includes:

- AT1000 and independently estimated SRT50 trajectories.
- Uncertainty of the MLE estimates.
- Word recognition rates by sentence position.
- Distribution of correctly recognized words per sentence.
- Final SRT50 values and measurement summary.

## Methodological notes

The independent SRT50 estimator uses a binomial likelihood with a logistic psychometric function and a fixed slope.

The AT1000 may use different fitting procedures depending on its configuration. In particular, the `OldenburgerLX` fitting procedure allows different psychometric slopes, whereas the current implementation of `rAT1000` assumes a fixed slope.

Consequently, the independently estimated SRT50 may differ from the value reported by the AT1000.

Standard errors are derived from the curvature of the log-likelihood and assume independent word-level responses conditional on the model.

Estimates that cannot be reliably identified are returned as `NA`.

## Limitations

This package is intended for research, methodological evaluation and exploratory analysis.

**The independent SRT50 estimator has not been clinically validated and should not replace validated diagnostic procedures.**

Original AT1000 measurement results remain unchanged.

Do not upload identifiable patient data to this public repository.

## License

This project is licensed under the MIT License.

Copyright © 2026 Andreas F. Kandolf.

## Author

Andreas F. Kandolf

## Source code

[github.com/k-andyman/rAT1000](https://github.com/k-andyman/rAT1000)