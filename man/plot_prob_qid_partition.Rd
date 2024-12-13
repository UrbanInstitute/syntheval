% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disc_qid_mi.R
\name{plot_prob_qid_partition}
\alias{plot_prob_qid_partition}
\title{Plot partition selection probabilities}
\usage{
plot_prob_qid_partition(agg_eval_data, keys, max_k = 20)
}
\arguments{
\item{agg_eval_data}{Output from \code{aggregate_qid_eval}}

\item{keys}{A character vector of column names}

\item{max_k}{largest partition selection size}
}
\value{
A \code{ggplot2} plot.
}
\description{
Plot partition selection probabilities
}
