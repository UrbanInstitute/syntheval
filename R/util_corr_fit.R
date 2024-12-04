#' 
#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param use optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an abbreviation 
#' of) one of the strings "everything", "all.obs", "complete.obs", 
#' "na.or.complete", or "pairwise.complete.obs".
#'
#' @return A `list` of fit metrics:
#'  - `correlation_original`: correlation matrix of the original data.
#'  - `correlation_synthetic`: correlation matrix of the synthetic data.
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the correlation matrix.
#' 
.util_corr_fit <- function(synth_data, conf_data, use = "everything") {
  
  
  synth_data <- dplyr::select_if(synth_data, is.numeric)
  conf_data <- dplyr::select_if(conf_data, is.numeric)

  # reorder data names
  conf_data <- dplyr::select(conf_data, names(synth_data))
  
  # helper function to find a correlation matrix with the upper tri set to zeros
  lower_triangle <- function(x, use) {
    
    # find the linear correlation matrix of numeric variables from a data set
    correlation_matrix <-
      x %>%
      dplyr::select_if(is.numeric) %>%
      stats::cor(use = use)
    
    # set the values in the upper triangle to zero to avoid double counting
    correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA
    
    return(correlation_matrix)
  }
  
  # find the lower triangle of the original data linear correlation matrix
  original_lt <- lower_triangle(conf_data, use = use)
  
  # find the lower triangle of the synthetic data linear correlation matrix
  synthetic_lt <- lower_triangle(synth_data, use = use)
  
  # compare names
  if (any(rownames(original_lt) != rownames(synthetic_lt))) {
    stop("ERROR: rownames are not identical")
  }
  
  if (any(colnames(original_lt) != colnames(synthetic_lt))) {
    stop("ERROR: colnames are not identical")
  }
  
  # find the difference between the matrices
  difference_lt <- synthetic_lt - original_lt
  
  # find the length of the nonzero values in the matrices
  n <- choose(ncol(difference_lt), 2)
  
  # calculate the correlation fit and divide by n
  correlation_fit <- sqrt(sum(difference_lt ^ 2, na.rm = TRUE)) / n
  
  difference_vec <- as.numeric(difference_lt)[!is.na(difference_lt)]
  
  # mean absolute error
  correlation_difference_mae <- difference_vec %>%
    abs() %>%
    mean()
  
  # root mean square error
  correlation_difference_rmse <- 
    difference_vec ^ 2%>%
    mean() %>%
    sqrt()
  
  return(
    list(
      correlation_original = original_lt,
      correlation_synthetic = synthetic_lt,
      correlation_difference = difference_lt,
      correlation_fit = correlation_fit,
      correlation_difference_mae = correlation_difference_mae,
      correlation_difference_rmse = correlation_difference_rmse
    )
  )
  
}

#' 
#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param eval_data An `eval_data` object
#' @param use optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an abbreviation 
#' of) one of the strings "everything", "all.obs", "complete.obs", 
#' "na.or.complete", or "pairwise.complete.obs".
#'
#' @return A `list` of fit metrics (one per each synthetic data replicate):
#'  - `correlation_original`: correlation matrix of the original data.
#'  - `correlation_synthetic`: correlation matrix of the synthetic data.
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the correlation matrix.
#'  
#' @family utility metrics
#'
#' @export
#' 
util_corr_fit <- function(eval_data, use = "everything") {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep == 1) {
    
    return(
      .util_corr_fit(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        use = use
      )
    )
    
  } else {
    
    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {
        
        .util_corr_fit(
          conf_data = eval_data$conf_data, 
          synth_data = sd, 
          use = use
        )
        
      }
    )
    
    return(result)
    
  }
  
}