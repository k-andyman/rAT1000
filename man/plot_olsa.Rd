\name{plot_olsa}
\alias{plot_olsa}
\title{Plot cumulative and rolling SRT50 estimates and OLSA performance}
\usage{
plot_olsa(x, slope = 0.15, show_sd = TRUE, main = NULL,
          intelligibility_moving_window = 8, MLE_rolling_window = 20,
          trim_aborted = TRUE, min_zero_run = 5, moving_window = NULL)
}
\arguments{
  \item{x}{An AT1000 \code{.hd} path or a result returned by \code{process_olsa}.}
  \item{slope}{Fixed psychometric slope used when \code{x} is a file path.}
  \item{show_sd}{Draw \eqn{\pm 1} model-based standard error trajectories for both MLEs.}
  \item{main}{Optional title prefix for the SRT50 panel.}
  \item{intelligibility_moving_window}{Moving mean window (number of trials) for the word intelligibility plot. Default \code{8}.}
  \item{MLE_rolling_window}{Rolling-window size (number of trials) for SRT50 MLE, when \code{x} is a file path. Default \code{20}. A preprocessed result retains its stored window length.}
  \item{trim_aborted}{If \code{x} is a file path, whether to trim suspected aborted end-of-test trials (default \code{TRUE}).}
  \item{min_zero_run}{Terminal run length required for trimming (default \code{5}).}
  \item{moving_window}{Deprecated alias for \code{intelligibility_moving_window}; do not specify both.}
}
\description{Four-panel base graphics display of cumulative and rolling-window SRT50 MLEs and their uncertainties, AT1000 SRT50 results, moving intelligibility, word response distributions, and final results.}
\value{Invisibly returns the processed data.}
\details{The displayed final intelligibility is recomputed from retained valid trials, which is important for multi-list long recordings and when suspected aborted trials are removed. The model-based uncertainty curves are not confidence bands for a time-varying learning effect.}
\examples{
\dontrun{
res <- process_olsa("measurement.hd", MLE_rolling_window = 20)
plot_olsa(res, intelligibility_moving_window = 8)
plot_olsa("measurement.hd", MLE_rolling_window = 30)
}
}
