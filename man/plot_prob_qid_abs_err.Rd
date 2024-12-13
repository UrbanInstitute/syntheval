% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disc_qid_mi.R
\name{plot_prob_qid_abs_err}
\alias{plot_prob_qid_abs_err}
\title{Plot absolute error probabilities}
\usage{
plot_prob_qid_abs_err(
  agg_eval_data,
  keys,
  max_k = 20,
  probs = c(0.5, 0.75, 0.9),
  holdout = FALSE
)
}
\arguments{
\item{agg_eval_data}{Output from \code{aggregate_qid_eval}}

\item{keys}{A character vector of column names}

\item{max_k}{largest partition selection size}

\item{probs}{Quantiles at which to estimate confidence of QID count}

\item{holdout}{boolean, use data from holdout instead of confidential}
}
\value{
A \code{ggplot2} plot.
}
\description{
Plot absolute error probabilities
}
