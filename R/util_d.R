#' Calculate the KS distance (D) for numeric variables
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#'
#' @return A tibble with the D and location of the largest distance for each 
#' numeric variable
#' 
#' @export
#'
util_d <- function(postsynth, data) {
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # drop non-numeric variables
  data <- data %>%
    dplyr::select(tidyselect::where(is.numeric))
  
  synthetic_data <- synthetic_data %>%
    dplyr::select(tidyselect::where(is.numeric))
  
  # find common variables
  variables <- intersect(names(synthetic_data), names(data))
  
  # find common minimum and common maximum
  # create grid
  distances <- vector(mode = "list", length = length(variables))
  names(distances) <- variables
  for (var in variables) {
    
    # find the eCDFs for both variables
    ecdf_synth <- stats::ecdf(dplyr::pull(synthetic_data, var))
    ecdf_orig <- stats::ecdf(dplyr::pull(data, var))
    
    # calculate the minimum and maximum across both variables
    minimum <- min(c(dplyr::pull(synthetic_data, var), dplyr::pull(data, var)))
    maximum <- max(c(dplyr::pull(synthetic_data, var), dplyr::pull(data, var)))
    
    # create a grid of values for calculating the distances between the two
    # eCDFs
    z <- seq(
      from = minimum, 
      to = maximum,
      length.out = min(nrow(synthetic_data), nrow(data), 10000)
    )
    
    # for each variable, find D and the location of D
    distances[[var]] <- tibble::tibble(
      value = z, 
      ecdf_orig = ecdf_orig(z),
      ecdf_synth = ecdf_synth(z)
    ) %>%
      dplyr::mutate(D = abs(.data$ecdf_orig - .data$ecdf_synth)) %>%
      dplyr::select(-"ecdf_orig", -"ecdf_synth") %>%
      dplyr::slice_max(.data$D)
  
  }
  
  # combine into tibble
  D <- dplyr::bind_rows(distances, .id = "variable")
  
  return(D)
  
}