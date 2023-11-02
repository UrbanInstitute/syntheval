#' Add SPECKS to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#'
#' @return A discrimination with SPECKS
#' 
#' @export
add_specks <- function(discrimination) {
  
  propensities_original <- discrimination$propensities %>%
    dplyr::filter(.data$.source_label == "original") %>%
    dplyr::pull(".pred_synthetic")
    
  propensities_synthetic <- discrimination$propensities %>%
    dplyr::filter(.data$.source_label == "synthetic") %>%
    dplyr::pull(".pred_synthetic")
  
  # Compare propensity score by comparing ECDFs using KS Distance --------------------------------------------------------
  # Generate ECDF for original and synthetic data
  ecdf_original <- stats::ecdf(propensities_original)
  ecdf_synthetic <- stats::ecdf(propensities_synthetic)
  
  # Sequence of values to extract ECDFs
  z <- seq(from = 0, to = 1, by = 0.0001)
  
  # Calculate KS Distance of the original and synthetic ECDFS
  specks <- suppressWarnings(stats::ks.test(ecdf_original(z), ecdf_synthetic(z), exact = FALSE))$statistic
  
  specks <- unname(specks)
  
  discrimination$specks <- specks
  
  return(discrimination)
}