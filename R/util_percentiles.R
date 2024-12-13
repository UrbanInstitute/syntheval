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
#' @param common_vars A logical for if only common variables should be kept. 
#' This option will frequently result in an error because quantile() is strict 
#' about missing values.
#' @param synth_vars A logical for if only synthesized variables should be kept
#' @param na.rm A logical for ignoring `NA` values in computations.
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
  
    
  # prep data for NA handling
  combined_data <- prep_combined_data_for_na.rm(
    combined_data,
    na.rm = na.rm, 
    drop_zeros = drop_zeros,
    drop_zeros_exclude = group_by
  )
  na.rm_flag <- (na.rm | drop_zeros)
  
  # set weight to 1 for unweighted statistics
  if (missing(weight_var)) {

    summary_stats <- combined_data %>%
      dplyr::group_by(source, dplyr::across({{ group_by }})) %>%
      dplyr::reframe(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ stats::quantile(
            x = .x, 
            probs = probs, 
            na.rm = na.rm_flag
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
            na.rm = na.rm_flag
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
    ) %>%
    dplyr::arrange(.data$variable, .data$p)
    
  return(summary_stats)
  
}
