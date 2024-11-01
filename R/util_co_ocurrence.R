#'
#' Compare the co-occurrence fit metric of a confidential and synthetic dataset
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param na.rm a logical indicating whether missing values should be removed. 
#'  Note: values are jointly removed for each pair of variables even if only one
#'  value is missing.
#'
#' @return A `list` of fit metrics:
#'  - `co_occurrence_original`: co-occurrence matrix of the original data.
#'  - `co_occurrence_synthetic`: co-occurrence matrix of the synthetic data.
#'  - `co_occurrence_difference`: difference between `co_occurrence_synthetic` and
#'  `co_occurrence_original`.
#'  `co_occurrence_synthetic` and `co_occurrence_original`, divided by the number of
#'  cells in the co-occurrence matrix.
#'  - `co_occurrence_difference_mae`: Mean absolute error between 
#'  `co_occurrence_original` and `co_occurrence_synthetic`
#'  - `co_occurrence_difference_rmse`: Root mean squared error between 
#'  `co_occurrence_original` and `co_occurrence_synthetic`
#' 
.util_co_occurrence <- function(synth_data, conf_data, na.rm = FALSE) {
  
  synth_data <- dplyr::select_if(synth_data, is.numeric)
  conf_data <- dplyr::select_if(conf_data, is.numeric)

  # reorder data names
  conf_data <- dplyr::select(conf_data, names(synth_data))
  
  # helper function to find a co-occurrence matrix with the upper tri set to zeros
  lower_triangle <- function(x) {
    
    # find the linear co-occurrence matrix of numeric variables from a data set
    co_occurrence_matrix <-
      x %>%
      dplyr::select_if(is.numeric) %>%
      co_occurrence(na.rm = na.rm)
    
    # set the values in the upper triangle to zero to avoid double counting
    co_occurrence_matrix[upper.tri(co_occurrence_matrix, diag = TRUE)] <- NA
    
    return(co_occurrence_matrix)
  }
  
  # find the lower triangle of the original data linear co_occurrence matrix
  original_lt <- lower_triangle(conf_data)
  
  # find the lower triangle of the synthetic data linear co_occurrence matrix
  synthetic_lt <- lower_triangle(synth_data)
  
  # compare names
  if (any(rownames(original_lt) != rownames(synthetic_lt))) {
    stop("ERROR: rownames are not identical")
  }
  
  if (any(colnames(original_lt) != colnames(synthetic_lt))) {
    stop("ERROR: colnames are not identical")
  }
  
  # find the difference between the matrices
  difference_lt <- synthetic_lt - original_lt
  
  difference_vec <- as.numeric(difference_lt)[!is.na(difference_lt)]
  
  # mean absolute error
  co_occurrence_difference_mae <- difference_vec %>%
    abs() %>%
    mean()
  
  # root mean square error
  co_occurrence_difference_rmse <- 
    difference_vec ^ 2 %>%
    mean() %>%
    sqrt()
  
  return(
    list(
      co_occurrence_original = original_lt,
      co_occurrence_synthetic = synthetic_lt,
      co_occurrence_difference = difference_lt,
      co_occurrence_difference_mae = co_occurrence_difference_mae,
      co_occurrence_difference_rmse = co_occurrence_difference_rmse
    )
  )
  
}

#'
#' Compare the co-occurrence fit metric of a confidential and synthetic dataset
#'
#' @param eval_data An `eval_data` object
#' @param na.rm a logical indicating whether missing values should be removed. 
#'  Note: values are jointly removed for each pair of variables even if only one
#'  value is missing.
#'
#' @return A `list` of fit metrics (one per each synthetic data replicate)::
#'  - `co_occurrence_original`: co-occurrence matrix of the original data.
#'  - `co_occurrence_synthetic`: co-occurrence matrix of the synthetic data.
#'  - `co_occurrence_difference`: difference between `co_occurrence_synthetic` and
#'  `co_occurrence_original`.
#'  `co_occurrence_synthetic` and `co_occurrence_original`, divided by the number of
#'  cells in the co-occurrence matrix.
#'  - `co_occurrence_difference_mae`: Mean absolute error between 
#'  `co_occurrence_original` and `co_occurrence_synthetic`
#'  - `co_occurrence_difference_rmse`: Root mean squared error between 
#'  `co_occurrence_original` and `co_occurrence_synthetic`
#' 
#' @family Utility metrics
#' 
#' @export
#' 
util_co_occurrence <- function(eval_data, na.rm = FALSE) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep == 1) {
    
    return(
      .util_co_occurrence(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        na.rm = na.rm
      )
    )
    
  } else {
    
    return(
      purrr::map(
        .x = eval_data$synth_data,
        .f = \(sd) {
          
          .util_co_occurrence(
            conf_data = eval_data$conf_data, 
            synth_data = sd, 
            na.rm = na.rm
          )
          
        }
      )
    )
    
  }
  
}