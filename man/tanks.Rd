% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_funs.R
\name{tanks}
\alias{tanks}
\title{Title tanks simulation}
\usage{
tanks(
  max = NULL,
  size = 5,
  trials = 50,
  method = "double mean",
  show_truth = FALSE,
  show_samples = FALSE
)
}
\arguments{
\item{max}{numeric: Should be the Maximum number of tanks in the simulation}

\item{size}{numeric: This is the sample size being drawn.}

\item{trials}{numeric: This is the number of trials, or samples being taken}

\item{method}{string: Any of double mean", "3 sigma", "1.5 IQR Rule"}

\item{show_truth}{logical: Set to TRUE to show the maximum number of tanks}

\item{show_samples}{logical: Set to TRUE to show the samples taken}
}
\value{
list containing samples, estimates, estimate mean, estimate sampling standard deviation
        a simple confidence interval set at .95 confidence, and the Maximum number of tanks.
}
\description{
Title tanks simulation
}
\examples{
tanks()

tanks(max = 500, size = 10, trials = 1000, method = "3 sigma", show_truth = FALSE, show_samples = FALSE)


}
