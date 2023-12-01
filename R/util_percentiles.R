#' Calculate summary statistics for original and synthetic data.
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param probs A numeric vector of probabilities with values in \[0,1\]. The 
#' percentiles are interpolated using an empirical CDF. It's possible that the
#' percentiles are an approximation; especially when weights are used.
#' @param group_by An unquoted name of a (or multiple) grouping variable(s)
#' @param weight_var An unquoted name of a weight variable
#' @param drop_zeros A Boolean for if zeros should be dropped
#'
#' @return A `tibble` of summary statistics.
#'
#' @family utility metrics
#'
#' @export
#'
util_percentiles <- function(postsynth,
                        data,
                        probs = c(0.1, 0.5, 0.9),
                        weight_var = NULL, 
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
  
  # drop non-numeric variables except grouping variables
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
  
  
  # set weight to 1 for unweighted statistics
  if (missing(weight_var)) {

    summary_stats <- combined_data %>%
      dplyr::group_by(source, dplyr::across({{ group_by }})) %>%
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ stats::quantile(
            x = .x, 
            probs = probs, 
            na.rm = na.rm_toggle
          )
        ),
        p = probs
      ) %>%
      dplyr::select("p", dplyr::everything()) %>%
      dplyr::ungroup() %>%
      tidyr::gather(key = "variable", value = "value", -"source", -"p", 
                    -{{ group_by }}) %>%
      tidyr::spread(key = source, value = .data$value) %>%
      dplyr::arrange(.data$variable)

  } else {

    summary_stats <- combined_data %>%
      dplyr::group_by(source, dplyr::across({{ group_by }})) %>%
      dplyr::reframe(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ Hmisc::wtd.quantile(
            x = ., 
            weights = {{ weight_var }}, 
            probs = probs, 
            na.rm = na.rm_toggle
          )
        ),
        p = probs
      ) %>%
      dplyr::select("p", dplyr::everything()) %>%
      dplyr::ungroup() %>%
      tidyr::gather(key = "variable", value = "value", -"source", -"p", 
                    -{{ group_by }}) %>%
      tidyr::spread(key = source, value = .data$value) %>%
      dplyr::arrange(.data$variable)
    
  }
    
  
  summary_stats <- summary_stats  %>%
    dplyr::mutate(
      difference = .data$synthetic - .data$original,
      proportion_difference = .data$difference / .data$original
    )
  
  if (!is_postsynth(postsynth)) {
    
    variable_order <- names(dplyr::select(combined_data, -source))
    
  }
  
  summary_stats <- summary_stats %>%
    dplyr::mutate(
      variable = factor(.data$variable, levels = variable_order),
    ) %>%
    dplyr::arrange(.data$variable, .data$p)
    
  return(summary_stats)
  
}
