\name{process_olsa}
\alias{process_olsa}
\title{Parse AT1000 OLSA data and calculate cumulative SRT50}
\usage{process_olsa(hd, slope = 0.15)}
\arguments{\item{hd}{Path to an AT1000 .hd file.}\item{slope}{Fixed psychometric slope per dB.}}
\value{A list with \code{global} and \code{trials} data frames. The trials data frame contains \code{SRT50_MLE} and \code{SRT50_sd}.}
\description{Reads a measurement file and calculates one fixed-slope maximum-likelihood estimate per cumulative set of trials.}
