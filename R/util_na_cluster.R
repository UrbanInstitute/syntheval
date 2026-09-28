#'
#' Calculate Pearson's linear correlation coefficient clustering of missingness across variables
#'
#' @param conf_data A data frame with the confidential data
#' @param synth_data A data frame with the synthetic data
#' @param holdout_data An optional data frame with the holdout data
#' @param na_values A character vector of values that should be treated as 
#' missing in addition to `NA`
#'
#' @return A `list` of fit metrics, restricted to the variables common to
#' `conf_data` and `synth_data` with at least one missing value in either:
#'  - `na_cluster_confidential`: phi coefficient matrix (correlation of binary
#'  missingness indicators) of the confidential data.
#'  - `na_cluster_synthetic`: phi coefficient matrix of the synthetic data.
#'  - `na_cluster_holdout`: phi coefficient matrix of the holdout data (if
#'  `holdout_data` is supplied), further restricted to the variables present
#'  in `holdout_data`.
#'  - `na_cluster_difference`: difference between `na_cluster_synthetic` and
#'  `na_cluster_confidential`.
#'  - `na_cluster_difference_mae`: mean absolute error between
#'  `na_cluster_confidential` and `na_cluster_synthetic`.
#'  - `na_cluster_difference_rmse`: root mean square error between
#'  `na_cluster_confidential` and `na_cluster_synthetic`.
#'
.util_na_cluster <- function(conf_data, synth_data, holdout_data = NULL, na_values = NULL) {
  
  conf_data <- .recode_custom_na(conf_data, na_values = na_values)
  synth_data <- .recode_custom_na(synth_data, na_values = na_values)
  
  # a variable missing from conf or synth can't be compared across the two
  common_vars <- intersect(names(conf_data), names(synth_data))
  
  # only compare variables with at least one missing value in conf or synth
  has_na_lgl <- purrr::map_lgl(.x = conf_data[common_vars], .f = ~ any(is.na(.x))) |
    purrr::map_lgl(.x = synth_data[common_vars], .f = ~ any(is.na(.x)))
  
  has_na <- common_vars[has_na_lgl]
  
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
  
  confidential_lt <- na_cluster_matrix(conf_data, has_na = has_na)
  synthetic_lt <- na_cluster_matrix(synth_data, has_na = has_na)
  
  # compare names
  if (any(rownames(confidential_lt) != rownames(synthetic_lt))) {
    stop("ERROR: rownames are not identical")
  }
  
  if (any(colnames(confidential_lt) != colnames(synthetic_lt))) {
    stop("ERROR: colnames are not identical")
  }
  
  difference_lt <- synthetic_lt - confidential_lt
  
  difference_vec <- as.numeric(difference_lt)[!is.na(difference_lt)]
  
  na_cluster_difference_mae <- difference_vec |>
    abs() |>
    mean()
  
  na_cluster_difference_rmse <- difference_vec ^ 2 |>
    mean() |>
    sqrt()
  
  result <- list(
    na_cluster_confidential = confidential_lt,
    na_cluster_synthetic = synthetic_lt,
    na_cluster_difference = difference_lt,
    na_cluster_difference_mae = na_cluster_difference_mae,
    na_cluster_difference_rmse = na_cluster_difference_rmse
  )
  
  if (!is.null(holdout_data)) {
    
    holdout_data <- .recode_custom_na(holdout_data, na_values = na_values)
    
    # the holdout is a reference, so it narrows itself instead of conf vs. synth
    result$na_cluster_holdout <- na_cluster_matrix(
      holdout_data,
      has_na = intersect(has_na, names(holdout_data))
    )
    
  }
  
  return(result)
  
}

#'
#' Calculate Pearson's linear correlation coefficient clustering of missingness across variables
#'
#' @param eval_data An `eval_data` object
#' @param na_values A character vector of values that should be treated as 
#' missing in addition to `NA`
#'
#' @return A `list` of fit metrics (one per each synthetic data replicate),
#' restricted to the variables common to `eval_data$conf_data` and
#' `eval_data$synth_data` with at least one missing value in either:
#'  - `na_cluster_confidential`: phi coefficient matrix (correlation of binary
#'  missingness indicators) of the confidential data.
#'  - `na_cluster_synthetic`: phi coefficient matrix of the synthetic data.
#'  - `na_cluster_holdout`: phi coefficient matrix of the holdout data (if
#'  `eval_data$holdout_data` is supplied), further restricted to the variables
#'  present in `eval_data$holdout_data`.
#'  - `na_cluster_difference`: difference between `na_cluster_synthetic` and
#'  `na_cluster_confidential`.
#'  - `na_cluster_difference_mae`: mean absolute error between
#'  `na_cluster_confidential` and `na_cluster_synthetic`.
#'  - `na_cluster_difference_rmse`: root mean square error between
#'  `na_cluster_confidential` and `na_cluster_synthetic`.
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
