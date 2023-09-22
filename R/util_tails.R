#' Explore the tails of numeric variables
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param n The number of observations to consider for each variable
#' @param weight_var An unquoted name of a weight variable
#' @param end "min" for minimum values and "max" for maximum values
#'
#' @return A `tibble` of summary statistics.
#'
#' @family utility functions
#'
#' @export
#'
util_tails <- function(postsynth,
                  data,
                  n = 10,
                  weight_var = 1,
                  end = "max") {
  
  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
    variable_order <- 
      levels(postsynth$jth_synthesis_time$variable)
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # drop non-numeric variables
  data <- data %>%
    dplyr::select_if(is.numeric)
  
  synthetic_data <- synthetic_data %>%
    dplyr::select_if(is.numeric)
  
  # combine both data sources
  combined_data <- dplyr::bind_rows(
    `original` = data,
    `synthetic` = synthetic_data,
    .id = "source"
  )
  
  # pivot longer
  long_data <- combined_data %>%
    tidyr::pivot_longer(
      cols = -c(source, {{ weight_var }}),
      names_to = "variable", 
      values_to = ".value"
    )
  
  # multiple values by weight
  long_data <- long_data %>%
    dplyr::mutate(.weighted_value = .value * {{ weight_var }})
    
  # calculate proportion of total contained in each observation
  long_data <- long_data %>%
    dplyr::group_by(source, variable) %>%
    dplyr::mutate(.weighted_prop = .weighted_value / sum(.weighted_value))
  
  # keep top n
  if (end == "max") {
    
    long_data <- long_data %>%
      dplyr::group_by(source, variable) %>%
      dplyr::slice_max(.weighted_value, n = n, with_ties = FALSE) %>%
      dplyr::ungroup()
    
  } else if (end == "min") {
    
    long_data <- long_data %>%
      dplyr::group_by(source, variable) %>%
      dplyr::slice_min(.weighted_value, n = n, with_ties = FALSE) %>%
      dplyr::ungroup()
    
  }
  
  # add rank and cumulative proportion variable
  long_data <- long_data %>%
    dplyr::group_by(source, variable) %>%
    dplyr::mutate(
      .rank = dplyr::row_number(),
      .cumulative_prop = cumsum(.weighted_prop)
    ) %>%
    dplyr::ungroup()
  
  return(long_data)
  
}
