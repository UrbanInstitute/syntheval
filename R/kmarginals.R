#' Calculate k-way marginals metric
#'
#' This is the mean absolute difference between cells from a k-way frequency 
#' histogram from the original data and a frequency histogram from the synthetic
#' histogram. This calculates the mean of many MabsDD.
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param k The number of marginals to consider
#'
#' @return A numeric MabsDD score
#'
#' @export
#'
kmarginals <- function(postsynth, data, k) {
  
  # throw error if k is too high
  if (k > 3) stop("k can't exceed 3")
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # 1. agree on list of variables
  synth_vars <- names(synthetic_data)
  
  synth_data_vars <- synth_vars[synth_vars %in% names(data)]
  
  vars_difference <- length(synth_vars) - length(synth_data_vars)
  
  # if (vars_difference != 0) {
  #   
  #   cat("Warning: the synthetic data contains", vars_difference, "more variables than data \n")
  #   
  # }
  
  # 2. create list of 1-, 2-, or 3-way marginals
  kmarginals_vars <- t(combn(synth_data_vars, m = k))
  
  # 3. function to take a set of variables (should be in [0, 2])
  madd <- function(synthetic_data, data, vars) {
    
    prop_synth <- synthetic_data |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::group_by_all() |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::mutate(prop_synth = n / sum(n)) |>
      dplyr::select(-n)
    
    prop_data <- data |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::group_by_all() |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::mutate(prop_data = n / sum(n)) |>
      dplyr::select(-n)
    
    combined_data <- dplyr::full_join(
      prop_synth,
      prop_data, 
      by = vars
    ) |>
      tidyr::replace_na(list(prop_synth = 0, prop_data = 0))
    
    madd <- combined_data |>
      dplyr::summarize(madd = mean(abs(prop_synth - prop_data))) |>
      dplyr::pull(madd)
    
    return(madd)
    
  }
  
  # iterate over all k-way marginals
  madds <- purrr::map_dbl(
    .x = 1:nrow(kmarginals_vars), 
    .f = ~ madd(synthetic_data, data, kmarginals_vars[.x, ]))
  
  # calculate the mean of the MabsDDs and then turn into and ascending measure
  # on [0, 1000]
  kmarginal_score <- (1 - mean(madds)) * 1000
  
  return(kmarginal_score)
  
}