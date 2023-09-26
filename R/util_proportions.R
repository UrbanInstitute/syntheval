#' Calculate frequency tables for categorical variables
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param group_var 
#'
#' @return A tibble with variables, classes, and relative frequencies
#' 
#' @export
#'
util_proportions <- function(postsynth, data, group_var = NULL) {
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  synthetic_data <- synthetic_data |>
    dplyr::select(dplyr::where(is.factor), where(is.character))
  
  data <- data |>
    dplyr::select(dplyr::where(is.factor), where(is.character))
  
  combined_data <- 
    dplyr::bind_rows(
      synthetic = synthetic_data,
      original = data,
      .id = "source"
    )
  
  combined_data <- combined_data |>
    tidyr::pivot_longer(
      cols = -c(source, {{ group_var }}), 
      names_to = "variable", 
      values_to = "class"
    ) 
  
  combined_data <- combined_data |>
    dplyr::count({{ group_var }}, source, variable, class) |>
    dplyr::group_by({{ group_var }}, source, variable) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()
  
  # variable -- class -- original -- synthetic
  return(combined_data)
  
}