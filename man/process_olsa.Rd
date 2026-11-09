\name{process_olsa}
\alias{process_olsa}
\title{Parse AT1000 OLSA data and calculate cumulative and rolling SRT50 MLEs}
\usage{
process_olsa(hd, slope = 0.15, trim_aborted = TRUE,
             min_zero_run = 5, MLE_rolling_window = 20)
}
\arguments{
  \item{hd}{Path to an AT1000 \code{.hd} measurement file.}
  \item{slope}{Fixed logistic psychometric slope. Default \code{0.15}.}
  \item{trim_aborted}{If \code{TRUE}, remove trailing incomplete records and suspected aborted zero-correct runs.}
  \item{min_zero_run}{Minimum length of a terminal zero-correct run for trimming (default \code{5}).}
  \item{MLE_rolling_window}{Window length in trials for a newly fitted rolling-window MLE (default \code{20}). Full windows only.}
}
\description{Reads trial-level AT1000 OLSA data and fits one cumulative and one rolling-window SRT50 MLE trajectory.}
\value{A list with \code{global} (measurement metadata), \code{trials} (retained trial-level observations), and \code{MLE_rolling_window}. The trial data include \code{SRT50_MLE} and \code{SRT50_sd} for cumulative estimates and \code{rwMLE} and \code{rwSD} for rolling-window estimates. Both sd columns are model-based standard errors of the fitted threshold, not empirical standard deviations.}
\details{Both estimators apply the binomial log-likelihood with a fixed logistic slope. Rolling estimates are \code{NA} until at least \code{MLE_rolling_window} retained trials have been observed. Windows containing exclusively all-correct or all-incorrect responses also yield \code{NA} by design. Adjacent rolling windows overlap and therefore are statistically dependent. Trimming may mistakenly remove true runs of zero-correct observations; disable with \code{trim_aborted = FALSE} when appropriate.}
\examples{
\dontrun{
res <- process_olsa("measurement.hd", MLE_rolling_window = 20)
head(res$trials[, c("SRT50_MLE", "SRT50_sd", "rwMLE", "rwSD")])
}
}
