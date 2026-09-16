#'
#' Calculate the proportion of missing values in each variable
#'
#' @param conf_data A data.frame with the confidential data
#' @param synth_data A data.frame with the synthetic data
#' @param holdout_data An optional data.frame with the holdout data
#' @param na_values An optional scalar or vector of values (in addition to `NA`)
#' that should be treated as missing
#'
#' @return A tibble `na_prop` with one row per `variable` x `source`
#' (`"original"`, `"synthetic"`, and `"holdout"` when supplied), with the
#' proportion of missing values in `na_prop`.
#'
.util_na_amount <- function(conf_data, synth_data, holdout_data = NULL, na_values = NULL) {
  
  conf_data <- .recode_custom_na(conf_data, na_values = na_values)
  synth_data <- .recode_custom_na(synth_data, na_values = na_values)
  
  na_prop_by_source <- function(data, source) {
    
    data |>
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(is.na(x)))
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "variable",
        values_to = "na_prop"
      ) |>
      dplyr::mutate(source = source, .after = "variable")
    
  }
  
  na_prop <- dplyr::bind_rows(
    na_prop_by_source(conf_data, source = "original"),
    na_prop_by_source(synth_data, source = "synthetic")
  )
  
  if (!is.null(holdout_data)) {
    
    holdout_data <- .recode_custom_na(holdout_data, na_values = na_values)
    
    na_prop <- dplyr::bind_rows(na_prop, na_prop_by_source(holdout_data, source = "holdout"))
    
  }
  
  return(na_prop)
  
}

#'
#' Calculate the proportion of missing values in each variable
#'
#' @param eval_data An `eval_data` object
#' @param na_values An optional scalar or vector of values (in addition to `NA`)
#' that should be treated as missing
#'
#' @return A tibble `na_prop` (one per synthetic data replicate) with one row
#' per `variable` x `source` (`"original"`, `"synthetic"`, and `"holdout"`
#' when `eval_data$holdout_data` is supplied), with the proportion of missing
#' values in `na_prop`.
#'
#' @family utility metrics
#'
#' @examples
#' ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
#'
#' util_na_amount(ed)
#'
#' @export
#'
util_na_amount <- function(eval_data, na_values = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep == 1) {
    
    result <- .util_na_amount(
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
        
        .util_na_amount(
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
