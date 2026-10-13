# Originalfunktionen aus 01_SRT50_Daten_einlesen.html

read_hd <- function(file) {
  ## Einlesen von .hd Dateien, die von der SRT50 Software generiert werden.
  
  raw <- paste(readLines(file, warn = FALSE), collapse = "\n")
  
  # Escape-Zeichen entfernen / Zeilenumbrüche rekonstruieren
  txt <- raw
  
  # Falls die Datei explizite "\N" als Zeilenumbruch enthält:
  txt <- gsub("\\N", "\n", txt, fixed = TRUE)
  
  # Einzelne Backslashes entfernen:
  # z.B. "ReadableResult\:" -> "ReadableResult:"
  # z.B. "\{" -> "{"
  txt <- gsub("\\", "", txt, fixed = TRUE)
  
  return(txt)
}

extract_key <- function(txt, key, which = c("last", "first", "all")) {
  # Extrahiert Werte aus einem bereinigten .hd-Text.
  #
  # Beispiel:
  #   extract_key(txt, "ReadableResult")
  #   extract_key(txt, "MeasurementStart")
  #   extract_key(txt, "SRT 0.5", which = "all")
  #
  # Die Funktion sucht nach Zeilen im Format:
  #   Key: Wert
  #
  # Beispiel:
  #   MeasurementDuration: 234536
  #
  # und gibt nur den Wert rechts vom Doppelpunkt zurück:
  #   "234536"
  #
  # Wichtig:
  # Manche Keys kommen mehrfach in der .hd-Datei vor.
  # Zum Beispiel können "SRT 0.5", "Intelligibility",
  # "SpeechLevel" oder "SNR" sowohl trialweise als auch
  # global/blockweise auftreten.
  #
  # Deshalb kann man über `which` steuern, welcher Treffer
  # zurückgegeben wird:
  #
  #   which = "last"   -> letzter Treffer, Default
  #   which = "first"  -> erster Treffer
  #   which = "all"    -> alle Treffer
  
  
  # match.arg() prüft, ob `which` einer der erlaubten Werte ist.
  # Falls man versehentlich z.B. which = "alles" schreibt,
  # gibt R direkt einen verständlichen Fehler aus.
  which <- match.arg(which)
  
  
  # Regex-Pattern für eine komplette Zeile mit dem gesuchten Key.
  #
  # (?m)
  #   aktiviert den multiline-Modus.
  #   Dadurch beziehen sich ^ und $ auf Zeilenanfang und Zeilenende,
  #   nicht nur auf Anfang und Ende des gesamten Textstrings.
  #
  # ^\\s*
  #   erlaubt beliebig viele Leerzeichen am Zeilenanfang.
  #
  # key
  #   ist der gesuchte Feldname, z.B. "MeasurementStart".
  #
  # \\s*:\\s*
  #   sucht den Doppelpunkt zwischen Key und Wert.
  #   Leerzeichen vor/nach dem Doppelpunkt sind erlaubt.
  #
  # (.*)$
  #   nimmt alles nach dem Doppelpunkt bis zum Zeilenende als Wert.
  pattern <- paste0("(?m)^\\s*", key, "\\s*:\\s*(.*)$")
  
  
  # gregexpr() sucht alle Vorkommen des Patterns im Text.
  # regmatches() extrahiert die tatsächlich gefundenen Textstellen.
  #
  # Ergebnis:
  #   Ein Character-Vektor mit allen passenden Zeilen.
  #
  # Beispiel:
  #   "MeasurementDuration: 234536"
  hits <- regmatches(txt, gregexpr(pattern, txt, perl = TRUE))[[1]]
  
  
  # Falls der Key im Text nicht vorkommt, geben wir NA zurück.
  # Das ist robuster als ein Fehler, weil nicht jedes .hd-File
  # jedes mögliche Feld enthalten muss.
  if (length(hits) == 0) {
    return(NA_character_)
  }
  
  
  # Entfernt den linken Teil "Key:" und behält nur den Wert.
  #
  # Beispiel:
  #   "MeasurementDuration: 234536"
  #
  # wird zu:
  #   "234536"
  values <- sub(pattern, "\\1", hits, perl = TRUE)
  
  
  # Entfernt Leerzeichen am Anfang und Ende der Werte.
  values <- trimws(values)
  
  
  # Ersten Treffer zurückgeben.
  if (which == "first") {
    return(values[1])
  }
  
  
  # Letzten Treffer zurückgeben.
  #
  # Das ist für globale Werte oft sinnvoll, weil finale/global
  # zusammenfassende Werte häufig weiter unten im File stehen.
  if (which == "last") {
    return(values[length(values)])
  }
  
  
  # Alle Treffer zurückgeben.
  #
  # Das ist nützlich, wenn man später trialweise Werte extrahieren will,
  # z.B. alle SNR-Werte oder alle trialweisen SRT-Schätzungen.
  if (which == "all") {
    return(values)
  }
}

extract_readable_value <- function(readable, label) {
  # Extrahiert einzelne Werte aus ReadableResult.
  #
  # Beispiel ReadableResult:
  #   "50%-Schwelle: -11,2 dB; Verständlichkeit: 56,0 %; Freifeld; ..."
  #
  # Beispiel:
  #   extract_readable_value(readable, "50%-Schwelle")
  #   -> "-11,2 dB"
  #
  #   extract_readable_value(readable, "Verständlichkeit")
  #   -> "56,0 %"
  
  pattern <- paste0(label, "\\s*:\\s*([^;]+)")
  hit <- regmatches(readable, regexpr(pattern, readable, perl = TRUE))
  
  if (length(hit) == 0 || hit == "") {
    return(NA_character_)
  }
  
  value <- sub(pattern, "\\1", hit, perl = TRUE)
  value <- trimws(value)
  
  return(value)
}

as_num <- function(x) {
  # Wandelt Textwerte in numerische Werte um.
  #
  # Wichtig für deutsche Dezimalkommas:
  #   "-11,2" -> -11.2
  #
  # Außerdem:
  #   "-" oder "" -> NA
  
  x <- trimws(x)
  x[x %in% c("", "-", "NA")] <- NA_character_
  x <- gsub(",", ".", x, fixed = TRUE)
  
  suppressWarnings(as.numeric(x))
}

extract_global <- function(txt) {
  # Extrahiert globale Messdaten aus einem bereinigten .hd-Text.
  #
  # Erwartet:
  #   txt = Output von read_hd(file)
  #
  # Gibt zurück:
  #   data.frame mit genau einer Zeile pro .hd-Datei
  
  
  # Kompletter lesbarer Ergebnisstring.
  readable <- extract_key(txt, "ReadableResult", which = "last")
  
  
  # Finale SRT50 aus ReadableResult ziehen.
  #
  # Aus:
  #   "50%-Schwelle: -11,2 dB"
  #
  # wird:
  #   -11.2
  srt50_raw <- extract_readable_value(readable, "50%-Schwelle")
  srt50_raw <- gsub("dB", "", srt50_raw, fixed = TRUE)
  final_srt50 <- as_num(srt50_raw)
  
  
  # Finale Verständlichkeit aus ReadableResult ziehen.
  #
  # Aus:
  #   "Verständlichkeit: 56,0 %"
  #
  # wird:
  #   0.56
  intelligibility_raw <- extract_readable_value(readable, "Verständlichkeit")
  intelligibility_raw <- gsub("%", "", intelligibility_raw, fixed = TRUE)
  final_intelligibility <- as_num(intelligibility_raw) / 100
  
  
  # Messdauer.
  #
  # MeasurementDuration ist sehr wahrscheinlich in Millisekunden.
  duration_ms <- as_num(extract_key(txt, "MeasurementDuration", which = "last"))
  
  
  # Testliste.
  #
  # Beispiel:
  #   "30/Olsaf30.07"
  test_list <- extract_key(txt, "TestList", which = "last")
  
  
  # Listennummer aus TestList extrahieren.
  #
  # Beispiel:
  #   "30/Olsaf30.07" -> "30.07"
  list_number <- sub(".*Olsaf", "", test_list)
  
  if (identical(list_number, test_list)) {
    list_number <- NA_character_
  }
  
  
  # Globalen Dataframe bauen.
  global <- data.frame(
    readable_result = readable,
    final_srt50 = final_srt50,
    final_intelligibility = final_intelligibility,
    measurement_start = extract_key(txt, "MeasurementStart", which = "last"),
    duration_ms = duration_ms,
    duration_sec = duration_ms / 1000,
    trials_total = as.integer(as_num(extract_key(txt, "TrialsTotal", which = "last"))),
    test_list = test_list,
    list_number = list_number,
    stringsAsFactors = FALSE
  )
  
  return(global)
}

extract_trials <- function(txt) {
  # Extrahiert alle Trial-/Satzdaten aus einem bereinigten .hd-Text.
  #
  # Output:
  #   data.frame mit einer Zeile pro Satz/Trial.
  #
  # Wichtig:
  #   Jeder OLSA-Satz hat 5 Wörter.
  #   Deshalb speichern wir die Wortkorrektheit breit als:
  #   word1, word2, word3, word4, word5
  
  
  # Text in Trial-Blöcke zerlegen.
  #
  # Wir splitten an Zeilenumbrüchen direkt vor "TestListItem:".
  #
  # Wichtig:
  #   "\\n\\s*(?=TestListItem\\s*:)"
  #
  # bedeutet:
  #   - suche einen Zeilenumbruch
  #   - danach beliebig viele Leerzeichen
  #   - direkt danach muss "TestListItem:" kommen
  #
  # Der Lookahead (?=...) sorgt dafür, dass "TestListItem:"
  # nicht entfernt wird, sondern im jeweiligen Trial-Block erhalten bleibt.
  #
  # Das ist robuster als "(?=\\s*TestListItem\\s*:)",
  # weil "\\s*" sonst auch mehrfach vor dem T matchen kann
  # und dadurch viele kleine Mini-Strings entstehen.
  trial_blocks <- strsplit(
    txt,
    "\\n\\s*(?=TestListItem\\s*:)",
    perl = TRUE
  )[[1]]
  
  
  # Nur echte Trial-Blöcke behalten.
  #
  # Nach dem Split sollten echte Trial-Blöcke direkt mit
  # "TestListItem:" anfangen.
  #
  # Der Header-Teil vor dem ersten Trial wird dadurch entfernt.
  trial_blocks <- trial_blocks[grepl("^TestListItem\\s*:", trial_blocks)]
  
  
  # Falls keine Trials gefunden wurden: leeren data.frame zurückgeben.
  if (length(trial_blocks) == 0) {
    return(data.frame())
  }
  
  
  # Hilfsfunktion: Satz in genau 5 Wörter zerlegen.
  split_5_words <- function(sentence) {
    if (is.na(sentence) || sentence == "") {
      return(rep(NA_character_, 5))
    }
    
    words <- strsplit(trimws(sentence), "\\s+")[[1]]
    
    # Falls aus irgendeinem Grund weniger als 5 Wörter gefunden werden:
    if (length(words) < 5) {
      words <- c(words, rep(NA_character_, 5 - length(words)))
    }
    
    # Falls mehr als 5 Wörter gefunden werden:
    if (length(words) > 5) {
      words <- words[1:5]
    }
    
    return(words)
  }
  
  
  # Jeden Trial-Block einzeln parsen.
  trials_list <- lapply(seq_along(trial_blocks), function(i) {
    
    block <- trial_blocks[[i]]
    
    # Ziel-Satz und Antwort-Satz auslesen.
    target_sentence <- extract_key(block, "TargetWords", which = "last")
    selected_sentence <- extract_key(block, "SelectedWords", which = "last")
    
    # In Wörter zerlegen.
    target_words <- split_5_words(target_sentence)
    selected_words <- split_5_words(selected_sentence)
    
    # Wortweise vergleichen.
    #
    # Beispiel:
    #   target:   "Ulrich gewann elf nasse Dosen"
    #   selected: "- - elf nasse Dosen"
    #
    # ergibt:
    #   FALSE FALSE TRUE TRUE TRUE
    correct <- target_words == selected_words
    
    # NAs als FALSE behandeln.
    correct[is.na(correct)] <- FALSE
    
    # Laufende SRT50 nach diesem Trial.
    # In frühen Trials steht oft "-", as_num() macht daraus NA.
    srt50_raw <- extract_key(block, "SRT 0.5", which = "last")
    srt50_raw <- gsub("dB", "", srt50_raw, fixed = TRUE)
    
    
    data.frame(
      trial = i,
      
      test_list_item = extract_key(block, "TestListItem", which = "last"),
      
      target_sentence = target_sentence,
      selected_sentence = selected_sentence,
      
      word1 = correct[1],
      word2 = correct[2],
      word3 = correct[3],
      word4 = correct[4],
      word5 = correct[5],
      
      n_correct = sum(correct),
      n_words = 5,
      
      trial_intelligibility = as_num(
        extract_key(block, "TrialIntelligibility", which = "last")
      ),
      
      cumulative_intelligibility = as_num(
        extract_key(block, "Intelligibility", which = "last")
      ),
      
      snr = as_num(
        extract_key(block, "SNR", which = "last")
      ),
      
      speech_level = as_num(
        extract_key(block, "SpeechLevel", which = "last")
      ),
      
      noise_level = as_num(
        extract_key(block, "NoiseChannelLevels", which = "last")
      ),
      
      srt50_after_trial = as_num(srt50_raw),
      
      stringsAsFactors = FALSE
    )
  })
  
  
  # Einzelne Trial-Zeilen zusammenfügen.
  trials <- do.call(rbind, trials_list)
  
  return(trials)
}

read_olsa_hd <- function(file) {
  # Liest eine .hd-Datei ein und gibt eine Liste mit zwei Dataframes zurück.
  #
  # Output:
  #   data$global -> globale Messdaten, eine Zeile
  #   data$trials -> Trial-/Satzdaten, eine Zeile pro Satz
  #
  # Anwendung:
  #   data <- read_olsa_hd(file)
  #   data$global
  #   data$trials
  
  
  # 1. Datei einlesen und bereinigen
  txt <- read_hd(file)
  
  
  # 2. Globale Daten extrahieren
  global <- extract_global(txt)
  
  
  # 3. Trial-Daten extrahieren
  trials <- extract_trials(txt)
  
  
  # 4. Beide Dataframes in eine named list packen
  df <- list(
    global = global,
    trials = trials
  )
  
  
  return(df)
}
