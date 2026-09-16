#'
#' Calculate Pearson's linear correlation coefficient clustering of missingness across variables
#'
#' @param conf_data A data.frame with the confidential data
#' @param synth_data A data.frame with the synthetic data
#' @param holdout_data An optional data.frame with the holdout data
#' @param na_values An optional scalar or vector of values (in addition to `NA`)
#' that should be treated as missing
#'
#' @return A `list` of fit metrics, restricted to variables with at least one
#' missing value in `conf_data` or `synth_data`:
#'  - `na_cluster_original`: phi coefficient matrix (correlation of binary
#'  missingness indicators) of the original data.
#'  - `na_cluster_synthetic`: phi coefficient matrix of the synthetic data.
#'  - `na_cluster_holdout`: phi coefficient matrix of the holdout data (if
#'  `holdout_data` is supplied).
#'  - `na_cluster_difference`: difference between `na_cluster_synthetic` and
#'  `na_cluster_original`.
#'  - `na_cluster_difference_mae`: mean absolute error between
#'  `na_cluster_original` and `na_cluster_synthetic`.
#'  - `na_cluster_difference_rmse`: root mean square error between
#'  `na_cluster_original` and `na_cluster_synthetic`.
#'
.util_na_cluster <- function(conf_data, synth_data, holdout_data = NULL, na_values = NULL) {
  
  conf_data <- .recode_custom_na(conf_data, na_values = na_values)
  synth_data <- .recode_custom_na(synth_data, na_values = na_values)
  
  # only compare variables with at least one missing value in conf or synth
  has_na_lgl <- purrr::map_lgl(
    .x = conf_data, 
    .f = ~ any(is.na(.x))) | purrr::map_lgl(synth_data, ~ any(is.na(.x))
  )
  
  has_na <- names(conf_data)[has_na_lgl]
  
  if (length(has_na) < 2) {
    stop("ERROR: at least two variables with missing values are required")
  }
  
  # find a phi coefficient matrix (correlation of missingness indicators) with the upper tri set to NA
  na_cluster_matrix <- function(data, has_na) {
    
    indicator <- is.na(data[has_na]) * 1
    
    phi_matrix <- stats::cor(indicator)
    phi_matrix[upper.tri(phi_matrix, diag = TRUE)] <- NA
    
    return(phi_matrix)
    
  }
  
  original_lt <- na_cluster_matrix(conf_data, has_na = has_na)
  synthetic_lt <- na_cluster_matrix(synth_data, has_na = has_na)
  
  # compare names
  if (any(rownames(original_lt) != rownames(synthetic_lt))) {
    stop("ERROR: rownames are not identical")
  }
  
  if (any(colnames(original_lt) != colnames(synthetic_lt))) {
    stop("ERROR: colnames are not identical")
  }
  
  difference_lt <- synthetic_lt - original_lt
  
  difference_vec <- as.numeric(difference_lt)[!is.na(difference_lt)]
  
  na_cluster_difference_mae <- difference_vec |>
    abs() |>
    mean()
  
  na_cluster_difference_rmse <- difference_vec ^ 2 |>
    mean() |>
    sqrt()
  
  result <- list(
    na_cluster_original = original_lt,
    na_cluster_synthetic = synthetic_lt,
    na_cluster_difference = difference_lt,
    na_cluster_difference_mae = na_cluster_difference_mae,
    na_cluster_difference_rmse = na_cluster_difference_rmse
  )
  
  if (!is.null(holdout_data)) {
    
    holdout_data <- .recode_custom_na(holdout_data, na_values = na_values)
    
    result$na_cluster_holdout <- na_cluster_matrix(holdout_data)
    
  }
  
  return(result)
  
}

#'
#' Calculate Pearson's linear correlation coefficient clustering of missingness across variables
#'
#' @param eval_data An `eval_data` object
#' @param na_values An optional scalar or vector of values (in addition to `NA`)
#' that should be treated as missing
#'
#' @return A `list` of fit metrics (one per each synthetic data replicate),
#' restricted to variables with at least one missing value in
#' `eval_data$conf_data` or `eval_data$synth_data`:
#'  - `na_cluster_original`: phi coefficient matrix (correlation of binary
#'  missingness indicators) of the original data.
#'  - `na_cluster_synthetic`: phi coefficient matrix of the synthetic data.
#'  - `na_cluster_holdout`: phi coefficient matrix of the holdout data (if
#'  `eval_data$holdout_data` is supplied).
#'  - `na_cluster_difference`: difference between `na_cluster_synthetic` and
#'  `na_cluster_original`.
#'  - `na_cluster_difference_mae`: mean absolute error between
#'  `na_cluster_original` and `na_cluster_synthetic`.
#'  - `na_cluster_difference_rmse`: root mean square error between
#'  `na_cluster_original` and `na_cluster_synthetic`.
#'
#' @family utility metrics
#'
#' @examples
#' conf_data <- penguins_conf
#' conf_data$bill_length_mm[1:10] <- NA
#' conf_data$bill_depth_mm[1:10] <- NA
#'
#' ed <- eval_data(conf_data = conf_data, synth_data = penguins_postsynth)
#'
#' util_na_cluster(ed)
#'
#' @export
#'
util_na_cluster <- function(eval_data, na_values = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep == 1) {
    
  result <- .util_na_cluster(
    conf_data = eval_data$conf_data,
    synth_data = eval_data$synth_data,
    holdout_data = eval_data$holdout_data,
    na_values = na_values
  )

  return(result)
    
  } else {
    
    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {
        
        .util_na_cluster(
          conf_data = eval_data$conf_data,
          synth_data = sd,
          holdout_data = eval_data$holdout_data,
          na_values = na_values
        )
        
      }
    )
    
    return(result)
    
  }
  
}
