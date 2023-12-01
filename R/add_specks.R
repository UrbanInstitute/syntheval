#' Add SPECKS to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#'
#' @return A discrimination with SPECKS
#' 
#' @export
add_specks <- function(discrimination, split = TRUE) {
  
  calc_specks <- function(propensities) {
    
    propensities_original <- propensities %>%
      dplyr::filter(.data$.source_label == "original") %>%
      dplyr::pull(".pred_synthetic")
    
    propensities_synthetic <- propensities %>%
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
    
    return(specks)
    
  }
  
  if (split) {
    
    specks_training <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "training") %>%
      calc_specks()
    
    specks_testing <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "testing") %>%
      calc_specks()
    
    specks <- tibble::tibble(
      .source = factor(c("training", "testing"), levels = c("training", "testing")),
      .specks = c(specks_training, specks_testing)
    )
    
  } else {
    
    specks_overall <- discrimination$propensities %>%
      calc_specks()
    
    specks <- tibble::tibble(
      .source = factor("overall", levels = "overall"),
      .specks = specks_overall
    )
    
  }
  
  discrimination$specks <- specks
  
  return(discrimination)

}