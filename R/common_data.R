#' 
#' Create a data frame with variables common to an eval_data object's data sets
#' 
#' Build a `recipe()` or `formula` against the result of this function instead 
#' of the confidential, synthetic, or holdout data directly. Restricting to 
#' common variables avoids errors from columns present in one data set but not 
#' another, and combining rows from every source means factor predictors carry 
#' the union of levels observed anywhere, so a recipe built here is safe to fit 
#' and predict against any of `eval_data`'s data sets.
#' 
#' @param eval_data An `eval_data` object.
#' 
#' @returns A tibble restricted to the variables common to the confidential, 
#' synthetic, and (if supplied) holdout data, with rows combined from every 
#' source.
#' 
#' @export
#' 
common_data <- function(eval_data) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep > 1) {
    
    synth_data <- eval_data$synth_data[[1]]
    message("Creating common_data using 1 synthetic data replicate.")
    
  } else {
    
    synth_data <- eval_data$synth_data
    
  }
  
  conf_data <- eval_data$conf_data
  holdout_data <- eval_data$holdout_data
  
  common_vars <- intersect(names(conf_data), names(synth_data))
  
  if (!is.null(holdout_data)) {
    
    common_vars <- intersect(common_vars, names(holdout_data))
    
  }
  
  mismatched_variables <- unique(c(
    setdiff(names(conf_data), common_vars),
    setdiff(names(synth_data), common_vars),
    setdiff(names(holdout_data), common_vars)
  ))
  
  if (length(mismatched_variables) != 0) {
    
    message(
      paste(
        paste(mismatched_variables, collapse = ", "),
        "exists in one data set but not the others. common_data() will only include common variables."
      )
    )
    
  }
  
  data_list <- list(
    dplyr::select(conf_data, dplyr::all_of(common_vars)),
    dplyr::select(synth_data, dplyr::all_of(common_vars))
  )
  
  if (!is.null(holdout_data)) {
    
    data_list <- c(data_list, list(dplyr::select(holdout_data, dplyr::all_of(common_vars))))
    
  }
  
  common <- dplyr::bind_rows(data_list)
  
  return(common)
  
}
