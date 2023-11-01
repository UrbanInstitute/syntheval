#' Calculate relative frequency tables for categorical variables
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param weight_var An unquoted name of a weight variable
#' @param group_by An unquoted name of a (or multiple) grouping variable(s)
#'
#' @return A tibble with variables, classes, and relative frequencies
#' 
#' @export
#'
util_proportions <- function(postsynth, data, weight_var = 1, 
                             group_by = NULL) {
  
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # dropping columns that are numeric (excluding the weight variable)
  synthetic_data <- synthetic_data %>%
    dplyr::mutate(.temp_weight = {{ weight_var }}) %>%
    dplyr::select(tidyselect::where(is.factor), 
                  tidyselect::where(is.character), 
                  ".temp_weight")
  
  data <- data %>%
    dplyr::mutate(.temp_weight = {{ weight_var }}) %>%
    dplyr::select(tidyselect::where(is.factor), 
                  tidyselect::where(is.character), 
                  ".temp_weight")
  
  # combining confidential and synthetic data 
  combined_data <- dplyr::bind_rows(
      synthetic = synthetic_data,
      original = data,
      .id = "source"
    ) 
  
  # lengthening combined data to find proportions for each level
  combined_data <- combined_data %>%
    tidyr::pivot_longer(
      cols = -c(source, {{ group_by }}, ".temp_weight"), 
      names_to = "variable", 
      values_to = "class"
    ) 
  
  # calculating proportions for each level of each variable 
  combined_data <- combined_data %>%
    dplyr::group_by(dplyr::across({{ group_by }}), .data$source, .data$variable, .data$class) %>%
    dplyr::summarise(.total_weight = sum(.data$.temp_weight)) %>%
    dplyr::mutate(prop = (.data$.total_weight) / sum(.data$.total_weight)) %>%
    dplyr::ungroup()
  
  # formatting results, getting proportion difference
  combined_data <- combined_data %>%
    tidyr::pivot_wider(names_from = source, values_from = "prop") %>%
    dplyr::group_by(dplyr::across({{ group_by }}), .data$variable, .data$class) %>%
    dplyr::summarise(synthetic = sum(.data$synthetic, na.rm = TRUE),
                     original = sum(.data$original, na.rm = TRUE)) %>%
    dplyr::mutate(difference = .data$synthetic - .data$original)
    
  # (group_by) -- variable -- class -- original -- synthetic -- difference
  return(combined_data)
  
}