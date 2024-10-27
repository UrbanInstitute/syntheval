#' Calculate the Kolmogorov-Smirnov distance (D) for each numeric variable in 
#' the synthetic and confidential data
#'
#' @param postsynth a postsynth object or tibble with synthetic data
#' @param data a data frame with the original data
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @return A tibble with the D and location of the largest distance for each 
#' numeric variable
#' 
#' @family Utility metrics
#' 
#' @export
#'
util_ks_distance <- function(postsynth, data, na.rm = FALSE) {
  
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
  
  var_not_in_synthetic <- setdiff(names(data), names(synthetic_data))
  if (length(var_not_in_synthetic) > 0) {
    
    warning("The following variables are in the confidential data but not the synthetic data: ", 
            paste(var_not_in_synthetic, collapse = ", "))
    
  }
    
  var_not_in_conf <- setdiff(names(synthetic_data), names(data))
  if (length(var_not_in_conf) > 0) {
    
    warning("The following variables are in the synthetic data but not the confidential data: ", 
            paste(var_not_in_conf, sep = ", "))
    
  }
  
  # find common minimum and common maximum
  # create grid
  distances <- vector(mode = "list", length = length(variables))
  names(distances) <- variables
  for (var in variables) {
    
    var_synth <- dplyr::pull(synthetic_data, var)
    var_data <- dplyr::pull(data, var)
    
    # drop missing values
    if (na.rm) {
      
      var_synth <- var_synth[!is.na(var_synth)]
      var_data <- var_data[!is.na(var_data)]
      
    }
    
    # find the eCDFs for both variables
    ecdf_synth <- stats::ecdf(var_synth)
    ecdf_orig <- stats::ecdf(var_data)
    
    # calculate the minimum and maximum across both variables
    minimum <- min(c(var_synth, var_data))
    maximum <- max(c(var_synth, var_data))
    
    # create a grid of values for calculating the distances between the two
    # eCDFs
    z <- seq(
      from = minimum, 
      to = maximum,
      length.out = min(length(var_synth), length(var_data), 10000)
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