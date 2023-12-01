#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param postsynth A postsynth object from tidysynthesis or a tibble
#' @param data an original (observed) data set.
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
#' @family utility metrics
#'
#' @export

util_corr_fit <- function(postsynth, data) {
  
  if (is_postsynth(postsynth)) {
  
    synthetic_data <- postsynth$synthetic_data
  
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  synthetic_data <- dplyr::select_if(synthetic_data, is.numeric)
  data <- dplyr::select_if(data, is.numeric)

  # reorder data names
  data <- dplyr::select(data, names(synthetic_data))
  
  # helper function to find a correlation matrix with the upper tri set to zeros
  lower_triangle <- function(x) {
    
    # find the linear correlation matrix of numeric variables from a data set
    correlation_matrix <-
      x %>%
      dplyr::select_if(is.numeric) %>%
      stats::cor()
    
    # set the values in the upper triangle to zero to avoid double counting
    correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA
    
    return(correlation_matrix)
  }
  
  # find the lower triangle of the original data linear correlation matrix
  original_lt <- lower_triangle(data)
  
  # find the lower triangle of the synthetic data linear correlation matrix
  synthetic_lt <- lower_triangle(synthetic_data)
  
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