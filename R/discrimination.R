#' Combine synthetic data and data for a discriminant based metric
#'
#' @param eval_data An `eval_data` object.
#'
#' @return A list of class discrimination
#' 
#' @family Utility metrics
#' 
#' @export
#'
discrimination <- function(eval_data) {
  
  stopifnot(is_eval_data(eval_data)) 
  
  if (eval_data$n_rep > 1 ) {
    
    synthetic_data <- eval_data[["synth_data"]][[1]]
    message("Creating discriminator object using 1 synthetic data replicate.")
    
  } else {
    
    synthetic_data <- eval_data[["synth_data"]]
    
  }
  data <- eval_data[["conf_data"]]
  
  
  mismatched_variables <- c(
    setdiff(names(synthetic_data), names(data)),
    setdiff(names(data), names(synthetic_data))
  )
  
  if (length(mismatched_variables) != 0) {
    
    message(
      paste(
        paste(mismatched_variables, collapse = ", "),
        "exists in one data set but not the other. discrimination() will only use common variables for modeling propensities."
      )
    )
    
  }

  ## combine original and synthetic data and add group indicator
  combined_data <- dplyr::bind_rows(
    original = dplyr::select(data, dplyr::any_of(colnames(synthetic_data))),
    synthetic = dplyr::select(synthetic_data, dplyr::any_of(colnames(data))),
    .id = ".source_label"
  ) %>%
    dplyr::mutate(.source_label = factor(.data$.source_label, levels = c("synthetic", "original")))
  
  discrimination <- list(
    combined_data = combined_data,
    propensities = NULL,
    discriminator = NULL,
    discriminator_auc = NULL,
    pmse = NULL,
    specks = NULL
  )
  
  attr(discrimination, "class") <- "discrimination"
  
  return(discrimination)
  
}

is_discrimination <- function(x) {
  inherits(x, "discrimination")
}

