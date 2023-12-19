#' Calculate relative frequency tables for categorical variables
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param weight_var An unquoted name of a weight variable
#' @param group_by An unquoted name of a (or multiple) grouping variable(s)
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_vars A logical for if only synthesized variables should be kept
#'
#' @return A tibble with variables, classes, and relative frequencies
#' 
#' @export
#'
util_proportions <- function(postsynth, 
                             data, 
                             weight_var = NULL, 
                             group_by = NULL,
                             common_vars = TRUE,
                             synth_vars = TRUE) {
  
  
  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
    variable_order <- 
      levels(postsynth$jth_synthesis_time$variable)
  
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # the goal is to create .temp_weight that is equal to 1 if weight_var isn't 
  # specified and equal to the weight if weight_var is a variable
  synthetic_weight <- synthetic_data %>%
    dplyr::select({{ weight_var }}) %>%
    dplyr::mutate(.temp_weight = {{ weight_var }}) %>%
    dplyr::select(-{{ weight_var }})
  
  # if {{ weight }} is NULL then set the weight to 1
  if (ncol(synthetic_weight) == 0) {
    
    synthetic_weight <- synthetic_data %>%
      dplyr::mutate(.temp_weight = 1) %>%
      dplyr::select(".temp_weight")
    
  }
  
  data_weight <- data %>%
    dplyr::select({{ weight_var }}) %>%
    dplyr::mutate(.temp_weight = {{ weight_var }}) %>%
    dplyr::select(-{{ weight_var }})
  
  # if {{ weight }} is NULL then set the weight to 1
  if (ncol(data_weight) == 0) {
    
    data_weight <- data_weight %>%
      dplyr::mutate(.temp_weight = 1) %>%
      dplyr::select(".temp_weight")
    
  }
  
  # combine the weight variable df to the synthetic data
  synthetic_data <- synthetic_data %>%
    dplyr::bind_cols(synthetic_weight) %>%
    dplyr::select(-{{ weight_var }})
  
  # combine the weight variable df to the confidential data
  data <- data %>%
    dplyr::bind_cols(data_weight) %>%
    dplyr::select(-{{ weight_var }})
  
  # filter to only synthesized variables
  # keep group_by variables
  if (is_postsynth(postsynth) & synth_vars) {
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(dplyr::all_of(variable_order), {{ group_by }}, ".temp_weight")
    
    data <- data %>%
      dplyr::select(dplyr::all_of(variable_order), {{ group_by }}, ".temp_weight")
    
  }
  
  # only keep variables in both data sets
  # keep group_by variables
  if (common_vars) {
    
    common_vars <- intersect(names(data), names(synthetic_data))
    
    data <- data %>%
      dplyr::select(dplyr::all_of(common_vars), {{ group_by }}, ".temp_weight")
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(dplyr::all_of(common_vars), {{ group_by }}, ".temp_weight")
    
  }
  
  # dropping columns that are numeric (excluding the weight variable)
  synthetic_data <- synthetic_data %>%
    dplyr::select(tidyselect::where(is.factor), 
                  tidyselect::where(is.character), 
                  ".temp_weight")
  
  data <- data %>%
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