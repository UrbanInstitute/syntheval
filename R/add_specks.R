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
  
  # Calculate KS Distance of the original and synthetic ECDFS
  specks <- suppressWarnings(
    stats::ks.test(
      propensities_original, 
      propensities_synthetic, 
      exact = FALSE)$statistic
    )
  
  specks <- unname(specks)
  
  discrimination$specks <- specks
  
  return(discrimination)
}