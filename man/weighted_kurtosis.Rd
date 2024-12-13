% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_estimators.R
\name{weighted_kurtosis}
\alias{weighted_kurtosis}
\title{Calculate weighted kurtosis}
\usage{
weighted_kurtosis(x, w, na.rm = FALSE)
}
\arguments{
\item{x}{A numeric vector.}

\item{w}{A vector of weights. Use a vector of 1s for the unweighted estimator.}

\item{na.rm}{Drop missing values (does not consider missing weights).}
}
\value{
A numeric vector of length 1.
}
\description{
Calculate weighted kurtosis
}
