\name{plot_olsa}
\alias{plot_olsa}
\title{Plot AT1000 and estimated OLSA results}
\usage{plot_olsa(x, slope = 0.15, show_sd = TRUE, main = NULL,
          moving_window = 8, trim_aborted = TRUE, min_zero_run = 5)}
\arguments{
  \item{x}{Path to an AT1000 .hd file or an existing result from \code{process_olsa}.}
  \item{slope}{Fixed psychometric slope used if a file path is supplied.}
  \item{show_sd}{Show the model-based standard error lines.}
  \item{main}{Title prefix of the SRT50 plot, default \code{"OLSA-Auswertung"}.}
  \item{moving_window}{Window size in trials for running intelligibility, shown only after a full window is available.}
  \item{trim_aborted}{Passed to \code{process_olsa} if \code{x} is a file path.}
  \item{min_zero_run}{Passed to \code{process_olsa} if \code{x} is a file path.}
}
\value{Invisibly returns the processed data.}
\description{Draws cumulative SRT50 estimates and AT1000 values, cumulative and moving-window intelligibility, word-count and position distributions, and a final SRT50 summary.}
