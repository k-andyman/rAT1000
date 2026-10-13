\name{plot_olsa}
\alias{plot_olsa}
\title{Plot AT1000 and estimated OLSA results}
\usage{plot_olsa(x, slope = 0.15, show_sd = TRUE, main = "OLSA-Auswertung")}
\arguments{\item{x}{Path to an AT1000 .hd file or a result from \code{process_olsa}.}\item{slope}{Fixed psychometric slope used when reading a file.}\item{show_sd}{Show the model-based standard error lines.}\item{main}{Plot title (currently reserved).}}
\value{Invisibly returns the processed data.}
\description{Draws SRT50 history, word-position accuracy, correct-word counts, and a summary.}
