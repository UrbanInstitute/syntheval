#' Add pMSE to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#'
#' @return A discrimination with pMSE
#' 
#' @export
add_pmse <- function(discrimination, split = TRUE) {
  
  calc_pmse <- function(propensities) {
    
    # calculate the expected propensity
    p_hat <- propensities %>%
      dplyr::summarize(
        n_synthetic = sum(.data$.source_label == "synthetic"),
        n_total = dplyr::n()
      ) %>%
      dplyr::mutate(p_hat = .data$n_synthetic / .data$n_total) %>%
      dplyr::pull("p_hat")
    
    propensities_vec <- propensities %>%
      dplyr::pull(".pred_synthetic")
    
    # calculate the observed pMSE
    pmse <- mean((propensities_vec - p_hat) ^ 2)
    
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