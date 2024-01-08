#' Add pMSE to discrimination object
#'
#' @param discrimination A discrimination object with propensities (likely 
#' added using add_propensities())
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#'
#' @return A discrimination object with propensities (likely added using 
#' add_propensities()) with a pMSE
#' 
#' @family Utility metrics
#' 
#' @export
#' 
add_pmse <- function(discrimination, split = TRUE) {
  
  calc_pmse <- function(propensities) {
    
    # calculate the expected propensity
    prop_synthetic <- propensities %>%
      dplyr::summarize(
        n_synthetic = sum(.data$.source_label == "synthetic"),
        n_total = dplyr::n()
      ) %>%
      dplyr::mutate(prop_synthetic = .data$n_synthetic / .data$n_total) %>%
      dplyr::pull("prop_synthetic")
    
    propensities_vec <- propensities %>%
      dplyr::pull(".pred_synthetic")
    
    # calculate the observed pMSE
    pmse <- mean((propensities_vec - prop_synthetic) ^ 2)
    
    return(pmse)
    
  }
  
  if (split) {
    
    pmse_training <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "training") %>%
      calc_pmse()
    
    pmse_testing <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "testing") %>%
      calc_pmse()
    
    pmse <- tibble::tibble(
      .source = factor(c("training", "testing"), levels = c("training", "testing")),
      .pmse = c(pmse_training, pmse_testing)
    )
    
  } else {
    
    pmse_overall <- discrimination$propensities %>%
      calc_pmse()
    
    pmse <- tibble::tibble(
      .source = factor("overall", levels = "overall"),
      .pmse = pmse_overall
    )
    
  }
  
  discrimination$pmse <- pmse
  
  return(discrimination)
  
}