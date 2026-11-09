# ------------------------------------------------------------
# 1. Maximum-Likelihood-Schätzer für OLSA-Trialdaten
# ------------------------------------------------------------

olsa_mle <- function(trials, slope = 0.15) {

  if (!is.finite(slope) || slope <= 0) {
    stop("slope muss positiv sein.")
  }

  # Spalten aus read_olsa_hd()
  L <- trials$snr
  k <- trials$n_correct
  n <- trials$n_words

  valid <- is.finite(L) & is.finite(k) &
           is.finite(n) & n > 0 &
           k >= 0 & k <= n

  L <- L[valid]
  k <- k[valid]
  n <- n[valid]

  if (length(L) == 0) {
    return(c(SRT50_MLE = NA_real_,
             SRT50_sd = NA_real_))
  }

  # Bei ausschließlich richtigen oder falschen Antworten
  # ist kein endlicher, unbeschränkter MLE bestimmbar.
  if (sum(k) == 0 || sum(k) == sum(n)) {
    return(c(SRT50_MLE = NA_real_,
             SRT50_sd = NA_real_))
  }

  # Negative Log-Likelihood:
  # p = 1 / (1 + exp(4 * slope * (L50 - L)))
  neg_log_likelihood <- function(L50) {

    p <- plogis(4 * slope * (L - L50))

    -sum(dbinom(k, size = n, prob = p, log = TRUE))
  }

  # Optimierungsintervall um die tatsächlich gemessenen SNRs
  lower <- min(L) - 60
  upper <- max(L) + 60

  fit <- optimize(
    f = neg_log_likelihood,
    interval = c(lower, upper)
  )

  L50_hat <- fit$minimum

  # Beobachtete Krümmung / Hessian
  p_hat <- plogis(4 * slope * (L - L50_hat))

  hessian <- (4 * slope)^2 *
             sum(n * p_hat * (1 - p_hat))

  se <- if (is.finite(hessian) && hessian > 0) {
    sqrt(1 / hessian)
  } else {
    NA_real_
  }

  # Randlösung nicht als valide Schätzung ausgeben
  if (L50_hat <= lower + 0.01 ||
      L50_hat >= upper - 0.01) {
    return(c(SRT50_MLE = NA_real_,
             SRT50_sd = NA_real_))
  }

  c(
    SRT50_MLE = L50_hat,
    SRT50_sd = se
  )
}


#' Process an AT1000 OLSA .hd file
#'
#' @description Cumulative and rolling-window fixed-slope SRT50 MLEs.
process_olsa <- function(hd, slope = 0.15,
                         trim_aborted = TRUE, min_zero_run = 5,
                         MLE_rolling_window = 20) {

  if (length(MLE_rolling_window) != 1L ||
      !is.numeric(MLE_rolling_window) ||
      is.na(MLE_rolling_window) ||
      !is.finite(MLE_rolling_window) ||
      MLE_rolling_window < 1 ||
      MLE_rolling_window != floor(MLE_rolling_window)) {
    stop("MLE_rolling_window muss eine positive ganze Zahl sein.")
  }

  # Keep the original trimming controls available in the previous rAT1000 release.
  data <- read_olsa_hd(hd, trim_aborted = trim_aborted,
                        min_zero_run = min_zero_run)
  trials <- data$trials
  N <- nrow(trials)

  if (!is.data.frame(trials) || N == 0L) {
    stop("Keine verwendbaren OLSA-Trials in dieser .hd-Datei erkannt.")
  }

  required <- c("snr", "n_correct", "n_words")
  if (!all(required %in% names(trials))) {
    stop("Trialdaten enthalten nicht alle benoetigten Spalten: ",
         paste(setdiff(required, names(trials)), collapse = ", "))
  }

  trials$SRT50_MLE <- rep(NA_real_, N)
  trials$SRT50_sd  <- rep(NA_real_, N)
  trials$rwMLE     <- rep(NA_real_, N)
  trials$rwSD      <- rep(NA_real_, N)

  for (i in seq_len(N)) {
    cumulative_fit <- olsa_mle(
      trials = trials[seq_len(i), , drop = FALSE],
      slope = slope
    )

    trials$SRT50_MLE[i] <- cumulative_fit["SRT50_MLE"]
    trials$SRT50_sd[i]  <- cumulative_fit["SRT50_sd"]

    if (i >= MLE_rolling_window) {
      window_idx <- seq.int(i - MLE_rolling_window + 1L, i)

      rolling_fit <- olsa_mle(
        trials = trials[window_idx, , drop = FALSE],
        slope = slope
      )

      trials$rwMLE[i] <- rolling_fit["SRT50_MLE"]
      trials$rwSD[i]  <- rolling_fit["SRT50_sd"]
    }
  }

  data$trials <- trials
  data$MLE_rolling_window <- MLE_rolling_window
  data
}
