# Kumulativer Fixed-Slope-MLE für die SRT50
olsa_mle <- function(trials, slope = 0.15) {
  L <- suppressWarnings(as.numeric(trials$snr))
  k <- suppressWarnings(as.numeric(trials$n_correct))
  n <- suppressWarnings(as.numeric(trials$n_words))
  ok <- is.finite(L) & is.finite(k) & is.finite(n) & n > 0 & k >= 0 & k <= n
  L <- L[ok]; k <- k[ok]; n <- n[ok]
  ans <- c(SRT50_MLE = NA_real_, SRT50_sd = NA_real_)
  if (!length(L) || sum(k) == 0 || sum(k) == sum(n)) return(ans)
  if (!is.finite(slope) || slope <= 0) stop("slope muss positiv sein")
  loss <- function(L50) {
    p <- plogis(4 * slope * (L - L50))
    -sum(dbinom(k, size = n, prob = p, log = TRUE))
  }
  lower <- min(L) - 60; upper <- max(L) + 60
  fit <- optimize(loss, interval = c(lower, upper))
  if (fit$minimum < lower + 0.01 || fit$minimum > upper - 0.01) return(ans)
  p <- plogis(4 * slope * (L - fit$minimum))
  curvature <- (4 * slope)^2 * sum(n * p * (1-p))
  c(SRT50_MLE = fit$minimum,
    SRT50_sd = if (is.finite(curvature) && curvature > 0) sqrt(1/curvature) else NA_real_)
}

process_olsa <- function(hd, slope = 0.15) {
  data <- read_olsa_hd(hd)
  tr <- data$trials
  if (!is.data.frame(tr) || nrow(tr) < 1L) stop("Keine OLSA-Trials in dieser .hd-Datei erkannt")
  required <- c("snr", "n_correct", "n_words")
  if (!all(required %in% names(tr))) stop("Trialdaten enthalten nicht alle benötigten Spalten")
  fit <- t(vapply(seq_len(nrow(tr)), function(i) {
    olsa_mle(tr[seq_len(i), , drop = FALSE], slope = slope)
  }, numeric(2)))
  tr$SRT50_MLE <- fit[, 1]
  tr$SRT50_sd <- fit[, 2]
  data$trials <- tr
  data
}
