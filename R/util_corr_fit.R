#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param postsynth A postsynth object from tidysynthesis or a tibble
#' @param data an original (observed) data set.
#' @param group_by The unquoted name of a (or multiple) grouping variable(s)
#'
#' @return A `list` of fit metrics:
#'  - `correlation_data`: A `tibble` of the correlations among the 
#'  numeric variables for the actual and synthetic data
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  the synthetic and original data, divided by the number of
#'  cells in the correlation matrix.
#'  - `correlation_difference_mae`: the mean of the absolute correlation 
#'  differences between the actual and synthetic data
#'  - `correlation_difference_rmse`: the root mean of the squared correlation 
#'  differences between the actual and synthetic data
#'  
#' @family utility functions
#'
#' @export

util_corr_fit <- function(postsynth,
                          data, 
                          group_by = NULL) {
  
   
  
  if (is_postsynth(postsynth)) {
  
    synthetic_data <- postsynth$synthetic_data
  
  } else {
    
    synthetic_data <- postsynth
  }
  
  # reorder data names (this appears to check if the variables are the same)
  # issue when the groups in the synthetic data do not match the groups in the og data, and vice versa
  # thinking about filling in all of groupings for each dataset first then running everything else
  data <- dplyr::select(data, names(synthetic_data))
  
  synthetic_data <- dplyr::select(synthetic_data, dplyr::where(is.numeric), {{ group_by }})  |>
    dplyr::arrange(dplyr::across({{ group_by }})) |>
    dplyr::group_split(dplyr::across({{ group_by }})) 
  
  data <- dplyr::select(data, dplyr::where(is.numeric), {{ group_by }}) |>
    dplyr::arrange(dplyr::across({{ group_by }})) |>
    dplyr::group_split(dplyr::across({{ group_by }}))
  
  groups <- lapply(data, function(x) dplyr::select(x, {{ group_by }}) |>
                     slice(1))

  results <- purrr::pmap(
      .l = list(synthetic_data, data, groups),
      .f = get_correlations
    )
  
    metrics <- dplyr::bind_cols(
      correlation_fit = map_dbl(results, "correlation_fit"),
      correlation_difference_mae = map_dbl(results, "correlation_difference_mae"),
      correlation_difference_rmse = map_dbl(results, "correlation_difference_rmse"),
      bind_rows(groups)
    )
    
    corr_data <- dplyr::bind_rows(map_dfr(results, "correlation_data"))
    
    return(list(
      corr_data,
      metrics
    ))
}

get_correlations <- function(synthetic_data,
                             data,
                             groups) {
  # helper function to find a correlation matrix with the upper tri set to zeros
  lower_triangle <- function(x) {
    
    # find the linear correlation matrix of numeric variables from a data set
    correlation_matrix <-
      x %>%
      dplyr::select_if(is.numeric) %>%
      stats::cor()
    
    # set NA values in the lower triangle to "", set the values in the upper triangle to zero to avoid double counting
    correlation_matrix[is.na(correlation_matrix[lower.tri(correlation_matrix, diag = FALSE)])] <- ""
    correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA
    
    return(correlation_matrix)
  }
  
  
  # find the lower triangle of the linear correlation matrices and add a var column 
  original_lt <- data.frame(lower_triangle(data))
  original_lt$var2 <- colnames(original_lt)
  
  synthetic_lt <- data.frame(lower_triangle(synthetic_data))
  synthetic_lt$var2 <- colnames(synthetic_lt)
  
  # restructure the correlation matrix so the cols are var1, var2, original/synthetic
  original_lt <- original_lt %>%
    tidyr::pivot_longer(cols = !var2, names_to = "var1", values_to = "original") %>%
    dplyr::filter(!is.na(original)) %>%
    dplyr::arrange(var1) %>%
    dplyr::select(var1, var2, original) %>%
    dplyr::mutate(original = dplyr::case_when(.data$original == "" ~ NA, 
                                              .default = .data$original))
  
  synthetic_lt <- synthetic_lt %>%
    tidyr::pivot_longer(cols = !var2, names_to = "var1", values_to = "synthetic") %>%
    dplyr::filter(!is.na(synthetic)) %>%
    dplyr::arrange(var1) %>%
    dplyr::select(var1, var2, synthetic) %>%
    dplyr::mutate(synthetic = dplyr::case_when(.data$synthetic == "" ~ NA, 
                                               .default = .data$synthetic))
  
  # combine the data and find the difference between the original and synthetic correlations 
  correlation_data <- original_lt %>%
    dplyr::left_join(synthetic_lt, by = c("var1","var2")) %>%
    dplyr::mutate(original = as.numeric(.data$original),
                  synthetic = as.numeric(.data$synthetic),
                  difference = .data$original - .data$synthetic,
                  proportion_difference = .data$difference / .data$original)
  
  correlation_data <- bind_cols(correlation_data, groups)
  
  # find the number of values in the lower triangle 
  n <- nrow(dplyr::filter(correlation_data, !is.na(difference)))
  
  # calculate the correlation fit and divide by n
  correlation_fit <- sqrt(sum(correlation_data$difference ^ 2, na.rm = TRUE)) / n
  
  difference_vec <- as.numeric(correlation_data$difference)
  
  # mean absolute error
  correlation_difference_mae <- difference_vec %>%
    abs() %>%
    mean()
  
  # root mean square error
  correlation_difference_rmse <- difference_vec ^ 2 %>%
    mean() %>%
    sqrt()
  
  
  return(
    list(
      correlation_data = correlation_data,
      correlation_fit = correlation_fit,
      correlation_difference_mae = correlation_difference_mae,
      correlation_difference_rmse = correlation_difference_rmse
    )
  )
  
}

