#'
#' Create combined data for pointwise utility statistic evaluation 
#' 
#' @param synth_data A synthetic data.frame
#' @param conf_data A confidential data.frame
#' @param keep_numeric A boolean, if TRUE keeps only numeric variables, else keeps
#' factors and characters. Defaults to TRUE.
#' @param weight_var_q A quoted name of a weight variable
#' @param group_by_q The quoted name(s) of a (or multiple) grouping variable(s)
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_varnames A list of variables synthesized to filter on, else `NULL`
#' 
#' @return A tibble
#' 
.create_combined_data_pointwise <- function(
    synth_data,
    conf_data,
    keep_numeric = TRUE,
    group_by_q = NULL,
    weight_var_q = NULL, 
    common_vars = FALSE,
    synth_varnames = NULL) {
  
  # if provided, filter to synthesized variables
  if (!is.null(synth_varnames)) {
    
    synthetic_data <- synth_data %>%
      dplyr::select(dplyr::all_of(synth_varnames), 
                    dplyr::any_of(group_by_q), 
                    dplyr::any_of(weight_var_q))
    
    data <- conf_data %>%
      dplyr::select(dplyr::all_of(synth_varnames), 
                    dplyr::any_of(group_by_q), 
                    dplyr::any_of(weight_var_q))
    
  } else {
    
    synthetic_data <- synth_data
    data <- conf_data
    
  }
  
  # only keep variables in both data sets
  # keep group_by variables
  if (common_vars) {
    
    common_var_names <- intersect(names(data), names(synthetic_data))
    
    data <- data %>%
      dplyr::select(dplyr::all_of(common_var_names), 
                    dplyr::any_of(c(group_by_q, weight_var_q)))
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(dplyr::all_of(common_var_names), 
                    dplyr::any_of(c(group_by_q, weight_var_q)))
    
  }
  
  if (keep_numeric) {
    # drop non-numeric variables
    data <- data %>%
      dplyr::select(tidyselect::where(is.numeric), 
                    dplyr::any_of(c(group_by_q, weight_var_q)))
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(tidyselect::where(is.numeric), 
                    dplyr::any_of(c(group_by_q, weight_var_q)))
    
  } else {
    
    # drop non-factor variables
    data <- data %>%
      dplyr::select(tidyselect::where(is.factor), 
                    tidyselect::where(is.character), 
                    dplyr::any_of(c(group_by_q, weight_var_q)))
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(tidyselect::where(is.factor), 
                    tidyselect::where(is.character), 
                    dplyr::any_of(c(group_by_q, weight_var_q)))
    
  }
  
  # combine both data sources
  combined_data <- dplyr::bind_rows(
    `original` = data,
    `synthetic` = synthetic_data,
    .id = "source"
  )
  
  return(combined_data)
  
}