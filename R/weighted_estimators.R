#' Calculate weighted standard deviation with a finite sample correction
#'
#' @param x A numeric vector.
#' @param w A vector of weights. Use a vector of 1s for the unweighted estimator.
#' @param na.rm Drop missing values (does not consider missing weights).
#'
#' @return A numeric vector of length 1.
#'
weighted_sd <- function(x, w, na.rm = FALSE) {
  # https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
  
  # drop observations with missing values
  if (na.rm) {
    
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
    
  } 
  
  weighted_variance <- (sum(w) / (sum(w) ^ 2 - sum(w ^ 2))) * 
    sum(w * (x - stats::weighted.mean(x, w)) ^ 2) 

  weighted_sd <- sqrt(weighted_variance)
  
  return(weighted_sd)
  
}

#' Calculate weighted skewness
#'
#' @param x A numeric vector.
#' @param w A vector of weights. Use a vector of 1s for the unweighted estimator.
#' @param na.rm Drop missing values (does not consider missing weights).
#'
#' @return A numeric vector of length 1.
#'
#' @export
#'
weighted_skewness <- function(x, w, na.rm = FALSE) {
  # https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
 
  # drop observations with missing values
  if (na.rm) {
    
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
    
  } 
  
  # without bias correction
  sd <- sqrt(sum(w * (x - stats::weighted.mean(x, w)) ^ 2) / sum(w))
  
  weighted_skewness <- sum(w * ((x - stats::weighted.mean(x, w)) / 
                                  sd) ^ 3) / sum(w)
  
  return(weighted_skewness)
  
}

#' Calculate weighted kurtosis
#'
#' @param x A numeric vector.
#' @param w A vector of weights. Use a vector of 1s for the unweighted estimator.
#' @param na.rm Drop missing values (does not consider missing weights).
#'
#' @return A numeric vector of length 1.
#'
weighted_kurtosis <- function(x, w, na.rm = FALSE) {
  # https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
  
  # drop observations with missing values
  if (na.rm) {
    
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
    
  } 
  
  # without bias correction
  sd <- sqrt(sum(w * (x - stats::weighted.mean(x, w)) ^ 2) / sum(w))
  
  # 
  weighted_kurtosis <- sum(w * ((x - stats::weighted.mean(x, w)) / 
                                  sd) ^ 4) / sum(w) - 3
  
  return(weighted_kurtosis)
  
}
