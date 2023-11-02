#' Combine synthetic data and data for a discriminant based metric
#'
#' @param postsynth A postsynth object from tidysynthesis or a tibble
#' @param data an original (observed) data set.
#'
#' @return A list of class discrimination
#' 
#' @export
#'
discrimination <- function(postsynth, data) {
  
  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
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
    discriminator = NULL,
    discriminator_auc = NULL
  )
  
  attr(discrimination, "class") <- "discrimination"
  
  return(discrimination)
  
}

is_discrimination <- function(x) {
  inherits(x, "discrimination")
}

