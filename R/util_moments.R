#' Calculate summary statistics for original and synthetic data.
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param weight_var An unquoted name of a weight variable
#' @param group_by The unquoted name of a (or multiple) grouping variable(s)
#' @param drop_zeros A logical for if zeros should be dropped
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_vars A logical for if only synthesized variables should be kept
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of summary statistics.
#'
#' @family utility metrics
#'
#' @export
#'
util_moments <- function(postsynth,
                         data,
                         weight_var = 1,
                         group_by = NULL,
                         drop_zeros = FALSE,
                         common_vars = TRUE,
                         synth_vars = TRUE, 
                         na.rm = FALSE) {
  
  # catch binding error
  . <- NULL

  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
    variable_order <- 
      levels(postsynth$jth_synthesis_time$variable)
    
    # filter to only synthesized variables
    # keep group_by variables
    if (synth_vars) {
      
      synthetic_data <- synthetic_data %>%
        dplyr::select(dplyr::all_of(variable_order), {{ group_by }})
      
      data <- data %>%
        dplyr::select(dplyr::all_of(variable_order), {{ group_by }})
      
    }
  
  } else {
    
    synthetic_data <- postsynth
  
  }
  
  # only keep variables in both data sets
  # keep group_by variables
  if (common_vars) {
    
    common_vars <- intersect(names(data), names(synthetic_data))
  
    data <- data %>%
      dplyr::select(dplyr::all_of(common_vars), {{ group_by }})
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(dplyr::all_of(common_vars), {{ group_by }})
    
  }
  
  # drop non-numeric variables
  data <- data %>%
    dplyr::select(tidyselect::where(is.numeric), {{ group_by }})
  
  synthetic_data <- synthetic_data %>%
    dplyr::select(tidyselect::where(is.numeric), {{ group_by }})
  
  # combine both data sources
  combined_data <- dplyr::bind_rows(
    `original` = data,
    `synthetic` = synthetic_data,
    .id = "source"
  )
  
  # prep data for NA handling
  combined_data <- prep_combined_data_for_na.rm(
    combined_data,
    na.rm = na.rm, 
    drop_zeros = drop_zeros,
    drop_zeros_exclude = group_by
  )
  na.rm_flag <- (na.rm | drop_zeros)
  
  # calculate summary statistics
  summary_stats <- combined_data %>%
    dplyr::mutate(.temp_weight = {{ weight_var }}) %>%
    dplyr::group_by(source, dplyr::across({{ group_by }})) %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = -".temp_weight",
        .fns = list(
          count = ~ sum((. != 0) * .data$.temp_weight, na.rm = na.rm_flag),
          mean = ~ stats::weighted.mean(x = ., w = .data$.temp_weight, na.rm = na.rm_flag),
          sd = ~ weighted_sd(x = ., w = .data$.temp_weight, na.rm = na.rm_flag),
          skewness = ~ weighted_skewness(x = ., w = .data$.temp_weight, na.rm = na.rm_flag),
          kurtosis = ~ weighted_kurtosis(x = ., w = .data$.temp_weight, na.rm = na.rm_flag)
        )
      )
    ) %>%
    tidyr::gather(key = "variable", value = "value", -source, - {{ group_by }}) %>%
    tidyr::separate(col = .data$variable,
                    into = c("variable", "statistic"),
                    sep = "_(?!.*_)") %>%
    tidyr::spread(key = source, value = .data$value) %>%
    dplyr::ungroup() 
  
  summary_stats <- summary_stats  %>%
    dplyr::mutate(
      difference = .data$synthetic - .data$original,
      proportion_difference = .data$difference / .data$original
    )
    
  statistics_order <- 
    c("count", "mean", "sd", "skewness", "kurtosis")  
  
  # sort table by synthesis order and keep factor levels for variables that 
  # weren't synthesized
  if (!is_postsynth(postsynth)) {
    
    variable_order <- names(dplyr::select(combined_data, -source))
    
  } else {
    
    all_vars <- names(dplyr::select(combined_data, -source))
    
    other_vars <- setdiff(all_vars, variable_order)
    
    variable_order <- c(variable_order, other_vars)
    
  }
  
  summary_stats <- summary_stats %>%
    dplyr::mutate(
      variable = factor(.data$variable, levels = variable_order),
      statistic = factor(.data$statistic, levels = statistics_order)
    ) %>%
    dplyr::arrange(.data$variable, .data$statistic)
    
  return(summary_stats)
  
}
