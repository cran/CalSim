% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ternary_forecast_example}
\alias{ternary_forecast_example}
\title{Ternary probability forecast and observations.}
\format{
A data frame with 10,000 rows and 6 variables.
\describe{
  \item{p1}{forecast probability for outcome 1}
  \item{p3}{forecast probability for outcome 3}
  \item{obs0}{outcomes, such that the forecast is well-calibrated}
  \item{obs1}{outcomes, such that the forecast is overconfident}
  \item{obs2}{outcomes, such that the forecast is underconfident}
  \item{obs3}{outcomes, such that the forecast is unconditionally biased}
}
}
\source{
Data generated by package author.
}
\usage{
data(ternary_forecast_example)
}
\description{
10,000 realizations of a ternary probability forecast, which exhibits different characteristics, 
depending on the realizing outcome variable. Idealized forecast example, generated as described in Wilks (2013).
}
\references{
Daniel S. Wilks, 2013, The Calibration Simplex: A Generalization of the Reliability Diagram for Three-Category Probability Forecasts, \emph{Weather and Forecasting}, \strong{28}, 1210-1218
}
\keyword{datasets}
