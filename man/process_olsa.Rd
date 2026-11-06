\name{process_olsa}
\alias{process_olsa}
\title{Parse AT1000 OLSA data and calculate cumulative SRT50}
\usage{process_olsa(hd, slope = 0.15, trim_aborted = TRUE, min_zero_run = 5)}
\arguments{
  \item{hd}{Path to an AT1000 .hd file.}
  \item{slope}{Fixed psychometric slope per dB (default 0.15).}
  \item{trim_aborted}{If TRUE, remove trailing incomplete trials and a trailing run of at least \code{min_zero_run} zero-correct trials after an earlier nonzero trial.}
  \item{min_zero_run}{Minimum number of consecutive zero-correct trials at the end required for trimming.}
}
\value{A list with \code{global} and \code{trials} data frames. The trials data frame contains \code{SRT50_MLE} and \code{SRT50_sd}. The global data frame additionally records how many trials were removed.}
\description{Reads a measurement file and calculates one fixed-slope maximum-likelihood estimate per cumulative set of retained trials.}
\details{Caution: a genuine sequence of zero-correct trials may be mistaken for aborted trials. Set \code{trim_aborted = FALSE} to preserve all parsed trials.}
