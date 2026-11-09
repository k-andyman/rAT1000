# Four-panel visualization from the updated OLSA notebook.

plot_olsa <- function(x,
                      slope = 0.15,
                      show_sd = TRUE,
                      main = NULL,
                      intelligibility_moving_window = 8,
                      MLE_rolling_window = 20,
                      trim_aborted = TRUE,
                      min_zero_run = 5,
                      moving_window = NULL) {

  check_window <- function(value, name) {
    if (length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 1 ||
        value != floor(value)) {
      stop(name, " muss eine positive ganze Zahl sein.")
    }
  }

  # Backward compatibility with the older package argument moving_window.
  if (!is.null(moving_window)) {
    if (!missing(intelligibility_moving_window)) {
      stop("Bitte nur intelligibility_moving_window oder moving_window angeben.")
    }
    intelligibility_moving_window <- moving_window
  }

  check_window(intelligibility_moving_window, "intelligibility_moving_window")
  check_window(MLE_rolling_window, "MLE_rolling_window")

  # ----------------------------------------------------------
  # 1) Input behandeln
  # ----------------------------------------------------------
  is_file <- is.character(x) && length(x) == 1L

  data <- if (is_file) {
    process_olsa(
      x,
      slope = slope,
      MLE_rolling_window = MLE_rolling_window,
      trim_aborted = trim_aborted,
      min_zero_run = min_zero_run
    )
  } else {
    x
  }

  if (!is.list(data) || is.null(data$trials) || is.null(data$global)) {
    stop("x muss ein .hd-Dateipfad oder das Ergebnis von process_olsa() sein.")
  }

  # Bei bereits berechneten Daten muss die gespeicherte Fenstergröße verwendet
  # werden, damit die Beschriftung zum tatsächlich berechneten RW-MLE passt.
  if (!is_file) {
    if (is.null(data$MLE_rolling_window)) {
      stop(
        "Die Daten enthalten keine gespeicherte MLE_rolling_window. ",
        "Bitte mit der aktualisierten process_olsa()-Funktion neu berechnen."
      )
    }

    MLE_rolling_window <- data$MLE_rolling_window
    check_window(MLE_rolling_window, "MLE_rolling_window")
  }

  trials <- data$trials
  global <- data$global

  required_cols <- c(
    "trial",
    "n_correct",
    "n_words",
    "srt50_after_trial",
    "SRT50_MLE",
    "SRT50_sd",
    "rwMLE",
    "rwSD"
  )

  missing_cols <- setdiff(required_cols, names(trials))

  if (length(missing_cols) > 0L) {
    stop(
      "Fehlende Spalten: ",
      paste(missing_cols, collapse = ", "),
      ". Bitte process_olsa() mit der aktualisierten Funktion ausführen."
    )
  }

  if (nrow(trials) == 0L) {
    stop("Keine Trials vorhanden.")
  }

  word_cols <- paste0("word", 1:5)
  has_word_cols <- all(word_cols %in% names(trials))

  # ----------------------------------------------------------
  # 2) Hilfsfunktionen
  # ----------------------------------------------------------
  last_valid_index <- function(x) {
    idx <- which(is.finite(x))
    if (length(idx) == 0L) NA_integer_ else tail(idx, 1L)
  }

  last_valid <- function(x) {
    idx <- last_valid_index(x)
    if (is.na(idx)) NA_real_ else x[idx]
  }

  fmt_db <- function(x, digits = 2) {
    if (length(x) == 0L || !is.finite(x[1])) {
      "NA"
    } else {
      sprintf(paste0("%.", digits, "f dB"), x[1])
    }
  }

  fmt_sd <- function(x, digits = 3) {
    if (length(x) == 0L || !is.finite(x[1])) {
      "NA"
    } else {
      sprintf(paste0("%.", digits, "f"), x[1])
    }
  }

  fmt_pct <- function(x) {
    if (length(x) == 0L || !is.finite(x[1])) {
      "NA"
    } else {
      sprintf("%.1f %%", 100 * x[1])
    }
  }

  # ----------------------------------------------------------
  # 3) Finale Kennwerte
  # ----------------------------------------------------------
  final_at1000 <- if ("final_srt50" %in% names(global)) {
    suppressWarnings(as.numeric(global$final_srt50[1]))
  } else {
    last_valid(trials$srt50_after_trial)
  }

  # Recalculate from retained trials: global summary can refer to one test list
  # rather than the entire SRT50-long measurement, or to pre-trimming trials.
  valid_words <- is.finite(trials$n_correct) &
    is.finite(trials$n_words) &
    trials$n_words > 0 &
    trials$n_correct >= 0 &
    trials$n_correct <= trials$n_words

  final_intell <- if (any(valid_words)) {
    sum(trials$n_correct[valid_words]) / sum(trials$n_words[valid_words])
  } else {
    NA_real_
  }

  # Letzter gültiger kumulativer MLE samt Fehler aus demselben Trial
  final_mle_idx <- last_valid_index(trials$SRT50_MLE)

  if (is.na(final_mle_idx)) {
    final_mle <- NA_real_
    final_sd <- NA_real_
  } else {
    final_mle <- trials$SRT50_MLE[final_mle_idx]
    final_sd <- trials$SRT50_sd[final_mle_idx]
  }

  # Letzter gültiger RW-MLE samt Fehler aus demselben Trial
  final_rw_idx <- last_valid_index(trials$rwMLE)

  if (is.na(final_rw_idx)) {
    final_rw_mle <- NA_real_
    final_rw_sd <- NA_real_
  } else {
    final_rw_mle <- trials$rwMLE[final_rw_idx]
    final_rw_sd <- trials$rwSD[final_rw_idx]
  }

  n_trials <- nrow(trials)

  if (is.null(main)) {
    main <- "OLSA-Auswertung"
  }

  # ----------------------------------------------------------
  # 4) Wortpositions-Statistik
  # ----------------------------------------------------------
  if (has_word_cols) {
    word_hits <- sapply(
      word_cols,
      function(w) sum(trials[[w]] %in% TRUE)
    )

    word_total <- sapply(
      word_cols,
      function(w) sum(!is.na(trials[[w]]))
    )
  } else {
    word_hits <- rep(0, 5)
    word_total <- rep(0, 5)
  }

  # ----------------------------------------------------------
  # 5) Farben und Layout
  # ----------------------------------------------------------
  col_mle <- "royalblue3"
  col_sd <- "skyblue3"
  col_at <- "firebrick3"
  col_rw <- "darkgreen"

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(
    mfrow = c(2, 2),
    mar = c(5, 4.5, 3, 1),
    oma = c(0, 0, 0, 0)
  )

  # ==========================================================
  # PANEL 1: SRT50-Verlauf
  # ==========================================================
  yvals <- c(
    trials$srt50_after_trial,
    trials$SRT50_MLE,
    trials$rwMLE,
    final_at1000,
    final_mle,
    final_rw_mle
  )

  if (show_sd) {
    yvals <- c(
      yvals,
      trials$SRT50_MLE - trials$SRT50_sd,
      trials$SRT50_MLE + trials$SRT50_sd,
      trials$rwMLE - trials$rwSD,
      trials$rwMLE + trials$rwSD
    )
  }

  yvals <- yvals[is.finite(yvals)]

  if (length(yvals) == 0L) {
    ylim1 <- c(-20, 0)
  } else {
    ylim1 <- range(yvals)
    ylim1 <- ylim1 + c(-0.5, 0.5)
  }

  plot(
    trials$trial,
    trials$SRT50_MLE,
    type = "n",
    xlab = "Trial",
    ylab = "SRT50 (dB SNR)",
    main = paste0(main, " - SRT50-Verlauf"),
    ylim = ylim1
  )

  # Unsicherheit des kumulativen MLE
  if (show_sd) {
    lines(
      trials$trial,
      trials$SRT50_MLE + trials$SRT50_sd,
      lty = 2,
      lwd = 1.3,
      col = col_sd
    )

    lines(
      trials$trial,
      trials$SRT50_MLE - trials$SRT50_sd,
      lty = 2,
      lwd = 1.3,
      col = col_sd
    )

    # Unsicherheit des Rolling-Window-MLE
    lines(
      trials$trial,
      trials$rwMLE + trials$rwSD,
      lty = 2,
      lwd = 1.3,
      col = col_rw
    )

    lines(
      trials$trial,
      trials$rwMLE - trials$rwSD,
      lty = 2,
      lwd = 1.3,
      col = col_rw
    )
  }

  # Kumulativer MLE
  lines(
    trials$trial,
    trials$SRT50_MLE,
    col = col_mle,
    lwd = 2
  )

  points(
    trials$trial,
    trials$SRT50_MLE,
    pch = 16,
    cex = 0.55,
    col = col_mle
  )

  # Rolling-Window-MLE
  lines(
    trials$trial,
    trials$rwMLE,
    col = col_rw,
    lwd = 2
  )

  points(
    trials$trial,
    trials$rwMLE,
    pch = 17,
    cex = 0.55,
    col = col_rw
  )

  # AT1000-Schätzung; NA-Werte erzeugen Unterbrechungen
  lines(
    trials$trial,
    trials$srt50_after_trial,
    lwd = 2,
    col = col_at
  )

  points(
    trials$trial,
    trials$srt50_after_trial,
    pch = 1,
    cex = 0.9,
    lwd = 1.5,
    col = col_at
  )

  # Finale Werte
  if (is.finite(final_at1000)) {
    abline(
      h = final_at1000,
      lty = 3,
      lwd = 1.4,
      col = col_at
    )
  }

   if (is.finite(final_mle)) {
    abline(
      h = final_mle,
      lty = 3,
      lwd = 1.4,
      col = col_mle
    )
  }

  if (is.finite(final_rw_mle)) {
    abline(
      h = final_rw_mle,
      lty = 3,
      lwd = 1.4,
      col = col_rw
    )
  }

legend(
  "topright",
  legend = c(
    "MLE laufend",
    paste0("RW", MLE_rolling_window, "-MLE"),
    "AT1000",
    "Final AT1000",
    "Final MLE",
    "Final RW-MLE"
  ),
  col = c(col_mle, col_rw, col_at, col_at, col_mle, col_rw),
  lty = c(1, 1, 1, 3, 3, 3),
  lwd = c(2, 2, 2, 1.4, 1.4, 1.4),
  pch = c(16, 17, 1, NA, NA, NA),
  bty = "n",
  cex = 0.75
)

  # ==========================================================
  # PANEL 2: Verständlichkeit im Verlauf
  # ==========================================================
  par(
    mfrow = c(2, 2),
    mfg = c(1, 2),
    mar = c(4.5, 4.5, 3, 1)
  )

  valid <- valid_words

  correct <- ifelse(valid, trials$n_correct, 0)
  possible <- ifelse(valid, trials$n_words, 0)

  # Kumulative Verständlichkeit ab dem ersten gültigen Trial
  cumulative_pct <- 100 * cumsum(correct) / cumsum(possible)
  cumulative_pct[cumsum(possible) == 0] <- NA_real_

  # Gleitender Wert: erst anzeigen, wenn ein vollständiges Fenster vorliegt
  moving_pct <- rep(NA_real_, nrow(trials))

  if (nrow(trials) >= intelligibility_moving_window) {
    for (i in seq.int(intelligibility_moving_window, nrow(trials))) {
      idx <- seq.int(
        i - intelligibility_moving_window + 1L,
        i
      )

      # Nur vollständige Fenster aus gültigen Trials verwenden
      if (all(valid[idx])) {
        moving_pct[i] <-
          100 * sum(trials$n_correct[idx]) / sum(trials$n_words[idx])
      }
    }
  }

  plot(
    trials$trial,
    cumulative_pct,
    type = "n",
    xlab = "Trial",
    ylab = "Verstandene Wörter (%)",
    ylim = c(0, 100),
    main = "Verständlichkeit im Verlauf"
  )

  abline(h = seq(0, 100, by = 20), col = "grey90")
  abline(h = 50, col = "grey70", lty = 2)

  # Orange zuerst zeichnen, damit die kumulative Linie darüberliegt
  lines(
    trials$trial,
    moving_pct,
    col = "darkorange2",
    lwd = 2
  )

  lines(
    trials$trial,
    cumulative_pct,
    col = "royalblue3",
    lwd = 2
  )

  legend(
    "topleft",
    legend = c(
      "Kumulativ",
      paste0("Letzte ", intelligibility_moving_window, " Trials")
    ),
    col = c("royalblue3", "darkorange2"),
    lty = 1,
    lwd = 2,
    bty = "n",
    cex = 0.8
  )

  # ==========================================================
  # PANEL 3: Verteilungen nebeneinander
  # ==========================================================
  par(mfrow = c(2, 2), mfg = c(2, 1))

  panel3_fig <- par("fig")
  split_x <- panel3_fig[1] +
    0.5 * (panel3_fig[2] - panel3_fig[1])

  fig_left <- c(
    panel3_fig[1],
    split_x,
    panel3_fig[3],
    panel3_fig[4]
  )

  fig_right <- c(
    split_x,
    panel3_fig[2],
    panel3_fig[3],
    panel3_fig[4]
  )

  counts <- table(factor(trials$n_correct, levels = 0:5))

  max_count <- max(
    c(as.numeric(counts), word_total, 1),
    na.rm = TRUE
  )

  count_ticks <- pretty(c(0, max_count))
  count_ylim <- c(0, max(count_ticks) * 1.15)

  # Links: Anzahl verstandener Wörter pro Satz
  par(
    fig = fig_left,
    new = TRUE,
    mar = c(4.5, 3.8, 2, 0.1)
  )

  mids <- barplot(
    as.numeric(counts),
    names.arg = rep("", 6),
    ylim = count_ylim,
    axes = FALSE,
    axisnames = FALSE,
    xlab = "Wörter pro Satz",
    ylab = "Häufigkeit",
    col = "goldenrod2",
    border = "grey35",
    cex.lab = 0.8
  )

  axis(1, at = mids, labels = 0:5, cex.axis = 0.75)
  axis(
    2,
    at = count_ticks,
    labels = count_ticks,
    las = 1,
    cex.axis = 0.75
  )

  text(
    x = mids,
    y = as.numeric(counts),
    labels = as.integer(counts),
    pos = 3,
    cex = 0.7
  )

  # Rechts: verstandene Wörter nach Wortposition
  par(
    fig = fig_right,
    new = TRUE,
    mar = c(4.5, 1.2, 2, 0.5)
  )

  if (has_word_cols) {
    mids_position <- barplot(
      word_hits,
      names.arg = rep("", 5),
      ylim = count_ylim,
      axes = FALSE,
      axisnames = FALSE,
      xlab = "Wortposition",
      ylab = "",
      col = "darkseagreen3",
      border = "grey35",
      cex.lab = 0.8
    )

    axis(
      1,
      at = mids_position,
      labels = 1:5,
      cex.axis = 0.75
    )

    text(
      x = mids_position,
      y = word_hits,
      labels = word_hits,
      pos = 3,
      cex = 0.7
    )
  } else {
    plot.new()
    text(0.5, 0.5, "word1 bis word5 nicht vorhanden")
  }

  # Gemeinsame Überschrift über beiden Diagrammen
  par(
    fig = panel3_fig,
    new = TRUE,
    mar = c(0, 0, 0, 0),
    xpd = NA
  )

  plot.new()
  text(
    0.5,
    0.96,
    "Verteilung der Verständlichkeiten",
    font = 2,
    cex = 1
  )

  # ==========================================================
  # PANEL 4: Zusammenfassung
  # ==========================================================
  par(mfrow = c(2, 2), mfg = c(2, 2))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))

  total_words <- sum(trials$n_words[valid_words])
  correct_words <- sum(trials$n_correct[valid_words])

  # Kasten mit den finalen SRT50-Werten
  rect(
    xleft = 0.04,
    ybottom = 0.56,
    xright = 0.96,
    ytop = 0.95,
    col = "grey96",
    border = "grey65",
    lwd = 1.2
  )

  # AT1000
  text(
    x = 0.08,
    y = 0.86,
    labels = paste0(
      "SRT50 AT1000:  ",
      fmt_db(final_at1000)
    ),
    adj = 0,
    font = 2,
    cex = 1.00,
    col = col_at
  )

  # Kumulativer MLE mit Fehler
  text(
    x = 0.08,
    y = 0.75,
    labels = paste0(
      "SRT50 MLE:  ",
      fmt_db(final_mle),
      " (± ",
      fmt_sd(final_sd),
      ")"
    ),
    adj = 0,
    font = 2,
    cex = 0.90,
    col = col_mle
  )

  # Finaler Rolling-Window-MLE mit Fenstergröße und Fehler
  text(
    x = 0.08,
    y = 0.64,
    labels = paste0(
      "SRT50 RW(",
      MLE_rolling_window,
      ") MLE:  ",
      fmt_db(final_rw_mle),
      " (± ",
      fmt_sd(final_rw_sd),
      ")"
    ),
    adj = 0,
    font = 2,
    cex = 0.90,
    col = col_rw
  )

  # Weitere Kennwerte unterhalb des Kastens
  summary_text <- c(
    paste0("Trials gesamt: ", n_trials),
    paste0("Finale Verständlichkeit: ", fmt_pct(final_intell)),
    paste0("Korrekte Wörter: ", correct_words, "/", total_words),
    paste0(
      "Mittlere Wörter korrekt/Satz: ",
      sprintf("%.2f", mean(trials$n_correct, na.rm = TRUE))
    )
  )

  text(
    x = 0.06,
    y = c(0.47, 0.37, 0.27, 0.17),
    labels = summary_text,
    adj = c(0, 0.5),
    cex = 0.90
  )

  invisible(data)
}
