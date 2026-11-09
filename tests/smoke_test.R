# Base R smoke tests with a generated .hd fixture (no patient data).
library(rAT1000)

# Fixed-SNR binomial likelihood with exactly 50% correct.
synthetic <- data.frame(snr = rep(-10, 20), n_correct = rep(5, 20),
                        n_words = rep(10, 20))
fit <- rAT1000:::olsa_mle(synthetic, slope = 0.15)
stopifnot(is.finite(fit["SRT50_MLE"]), abs(fit["SRT50_MLE"] + 10) < 1e-3,
          is.finite(fit["SRT50_sd"]), fit["SRT50_sd"] > 0)

# A complete 26-trial sample where the last five trials are deliberately zero-correct.
fixture <- tempfile(fileext = ".hd")
fixture_lines <- c(
  "ReadableResult: 50%-Schwelle: -10,0 dB; Verständlichkeit: 50,0 %;",
  "MeasurementDuration: 100000",
  "TestList: 30/Olsaf30.01",
  "TrialsTotal: 26"
)
for (i in 1:26) {
  response <- if (i >= 22) "- - - - -" else if (i %% 2 == 0) {
    "eins zwei drei vier -"
  } else {
    "eins - - - -"
  }
  fixture_lines <- c(fixture_lines,
    paste0("TestListItem: ", i),
    "TargetWords: eins zwei drei vier fünf",
    paste0("SelectedWords: ", response),
    "SNR: -10",
    "SpeechLevel: 55",
    "NoiseChannelLevels: 65",
    "SRT 0.5: -10 dB")
}
writeLines(fixture_lines, fixture)
on.exit <- NULL  # cleanup via unlink below

res <- process_olsa(fixture)
stopifnot(nrow(res$trials) == 21L,
          isTRUE(res$global$trailing_zero_trimmed),
          identical(res$MLE_rolling_window, 20))
stopifnot(all(is.na(res$trials$rwMLE[1:19])),
          is.finite(res$trials$rwMLE[20]),
          is.finite(res$trials$rwSD[20]))
fit20 <- rAT1000:::olsa_mle(res$trials[1:20, ], slope = 0.15)
fit21 <- rAT1000:::olsa_mle(res$trials[2:21, ], slope = 0.15)
stopifnot(isTRUE(all.equal(unname(res$trials$rwMLE[20]), unname(fit20[1]))),
          isTRUE(all.equal(unname(res$trials$rwMLE[21]), unname(fit21[1]))),
          isTRUE(all.equal(unname(res$trials$SRT50_MLE[20]), unname(fit20[1]))))

res_all <- process_olsa(fixture, trim_aborted = FALSE,
                        MLE_rolling_window = 30)
stopifnot(nrow(res_all$trials) == 26L,
          all(is.na(res_all$trials$rwMLE)))

unlink(fixture)
