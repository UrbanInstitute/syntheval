#' Calculate summary statistics for original and synthetic data.
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param weight_var An unquoted name of a weight variable
#' @param group_by The unquoted name of a grouping variable
#' @param drop_zeros A Boolean for if zeros should be dropped
#'
#' @return A `tibble` of summary statistics.
#'
#' @family utility functions
#'
#' @export
#'
util_moments <- function(postsynth,
                    data,
                    weight_var = 1,
                    group_by = NULL,
                    drop_zeros = FALSE) {
  
  # catch binding error
  . <- NULL

  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
    variable_order <- 
      levels(postsynth$jth_synthesis_time$variable)
    
  } else {
    
    synthetic_data <- postsynth
  
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
    
  na.rm_toggle <- FALSE
  if (drop_zeros) {
    
    combined_data[combined_data == 0] <- NA
    na.rm_toggle <- TRUE
    
  }
  
  # calculate summary statistics
  summary_stats <- combined_data %>%
    dplyr::mutate(.temp_weight = {{ weight_var }}) %>%
    dplyr::group_by(source, dplyr::across({{ group_by }})) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = -.temp_weight,
        .fns = list(
          count = ~ sum((!is.na(.)) * .temp_weight, na.rm = na.rm_toggle),
          have = ~ sum((. != 0) * .temp_weight, na.rm = na.rm_toggle),
          mean = ~ stats::weighted.mean(x = ., w = .temp_weight, na.rm = na.rm_toggle),
          sd = ~ weighted_sd(x = ., w = .temp_weight, na.rm = na.rm_toggle),
          skewness = ~ weighted_skewness(x = ., w = .temp_weight, na.rm = na.rm_toggle),
          kurtosis = ~ weighted_kurtosis(x = ., w = .temp_weight, na.rm = na.rm_toggle)
        )
      )
    ) %>%
    tidyr::gather(key = "variable", value = "value", -source, - {{ group_by }}) %>%
    tidyr::separate(col = .data$variable,
                    into = c("variable", "statistic"),
                    sep = "_(?!.*_)") %>%
    tidyr::spread(key = source, value = .data$value)
  
  summary_stats <- summary_stats  %>%
    dplyr::mutate(
      difference = .data$synthetic - .data$original,
      proportion_difference = .data$difference / .data$original
    )
    
  statistics_order <- 
    c("count", "have", "mean", "sd", "skewness", "kurtosis")  
  
  if (!is_postsynth(postsynth)) {
    
    variable_order <- names(dplyr::select(combined_data, -source))
    
  }
  
  summary_stats <- summary_stats %>%
    dplyr::mutate(
      variable = factor(variable, levels = variable_order),
      statistic = factor(statistic, levels = statistics_order)
    ) %>%
    dplyr::arrange(.data$variable, .data$statistic)
    
  return(summary_stats)
  
}
