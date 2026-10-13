plot_olsa <- function(x, slope = 0.15, show_sd = TRUE, main = "OLSA-Auswertung") {
  d <- if (is.character(x) && length(x)==1) process_olsa(x, slope) else x
  tr <- d$trials; g <- d$global
  if (!is.data.frame(tr) || !nrow(tr)) stop("Keine Trialdaten")
  getlast <- function(v) { v <- suppressWarnings(as.numeric(v)); if (!length(v)) NA_real_ else tail(v, 1) }
  at <- if ("final_srt50" %in% names(g)) getlast(g$final_srt50) else NA_real_
  mle <- getlast(tr$SRT50_MLE); sd <- getlast(tr$SRT50_sd)
  intell <- if ("final_intelligibility" %in% names(g)) getlast(g$final_intelligibility) else NA_real_
  fmt <- function(v, digits=2) if (is.finite(v)) sprintf(paste0("%.", digits, "f"), v) else "NA"
  old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
  par(mfrow=c(2,2), mar=c(5.2,4.5,3.5,1.4), cex.main=1.03)
  blue <- "royalblue3"; red <- "firebrick3"; light <- "skyblue3"
  # 1: SRT50
  yy <- c(tr$SRT50_MLE, tr$srt50_after_trial, at, mle)
  if (show_sd) yy <- c(yy, tr$SRT50_MLE - tr$SRT50_sd, tr$SRT50_MLE + tr$SRT50_sd)
  yy <- yy[is.finite(yy)]; if (!length(yy)) yy <- c(-20,0)
  ylim <- range(yy) + c(-0.5,0.5)
  xx <- if ("trial" %in% names(tr)) tr$trial else seq_len(nrow(tr))
  plot(xx, tr$SRT50_MLE, type="o", pch=16, cex=0.55, col=blue, lwd=2,
       ylim=ylim, xlab="Trial", ylab="SRT50 (dB SNR)", main="Verlauf der SRT50-Schätzung")
  if (show_sd) {
    lines(xx, tr$SRT50_MLE + tr$SRT50_sd, col=light, lty=2)
    lines(xx, tr$SRT50_MLE - tr$SRT50_sd, col=light, lty=2)
  }
  lines(xx, tr$srt50_after_trial, col=red, lwd=2)
  points(xx, tr$srt50_after_trial, col=red, pch=1)
  if (is.finite(at)) abline(h=at, col=red, lty=3)
  if (is.finite(mle)) abline(h=mle, col=blue, lty=3)
  legend("topright", legend=c("MLE", if(show_sd) "MLE ± 1 SD", "AT1000", "Final AT1000", "Final MLE"),
         col=c(blue, if(show_sd) light, red,red,blue),
         lty=c(1,if(show_sd) 2,1,3,3), cex=.72, bty="n")
  # 2: Wortpositionen, 45 Grad
  cols <- paste0("word",1:5)
  if (all(cols %in% names(tr))) {
    hits <- vapply(tr[cols], function(v) sum(v %in% TRUE), numeric(1))
    totals <- vapply(tr[cols], function(v) sum(!is.na(v)), numeric(1))
    ymax <- max(1,totals)*1.2
    mids <- barplot(hits, names.arg=rep("",5), col="darkseagreen3", border="grey45",
                    ylim=c(0,ymax), ylab="Anzahl verstanden", main="Verstandene Wörter nach Position")
    text(mids,hits, paste0(hits,"/",totals),pos=3,cex=.85)
    text(mids,pmax(.5,hits*.82),ifelse(totals>0,sprintf("%.0f%%",100*hits/pmax(totals,1)),"NA"),cex=.78)
    text(mids,par("usr")[3]-.06*ymax,paste("Wort",1:5),srt=45,adj=1,xpd=NA,cex=.83)
    mtext("Wortposition im Satz",side=1,line=4.1,cex=.87)
  } else {plot.new(); title("Wortpositionen"); text(.5,.5,"Wortspalten fehlen")}
  # 3: Wortzahl pro Satz
  cnt <- table(factor(tr$n_correct,levels=0:5))
  m <- barplot(cnt,col="goldenrod2",border="grey45",ylim=c(0,max(1,cnt)*1.2),
               xlab="Richtige Wörter pro Satz",ylab="Häufigkeit",main="Wörter pro Satz")
  text(m,as.numeric(cnt),as.integer(cnt),pos=3,cex=.83)
  # 4: Zusammenfassung
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  rect(.02,.64,.98,.96,col="grey96",border="grey65")
  text(.055,.85,paste0("SRT50 AT1000: ",fmt(at)," dB"),adj=0,font=2,cex=1.02,col=red)
  text(.055,.73,paste0("SRT50 MLE: ",fmt(mle)," dB (± ",fmt(sd,3),")"),adj=0,font=2,cex=.92,col=blue)
  tw <- sum(tr$n_words,na.rm=TRUE); cw <- sum(tr$n_correct,na.rm=TRUE)
  detail <- c(paste0("Trials gesamt: ",nrow(tr)),
              paste0("Verständlichkeit: ",if(is.finite(intell)) sprintf("%.1f %%",100*intell) else "NA"),
              paste0("Korrekte Wörter: ",cw," / ",tw),
              paste0("Wörter korrekt/Satz: ",fmt(mean(tr$n_correct,na.rm=TRUE))))
  text(.055,c(.52,.42,.32,.22),detail,adj=c(0,.5),cex=.89)
  invisible(d)
}
