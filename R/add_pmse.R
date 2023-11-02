#' Add pMSE to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#'
#' @return A discrimination with pMSE
#' 
#' @export
add_pmse <- function(discrimination) {
  
  # calculate the expected propensity
  p_hat <- discrimination$propensities %>%
    dplyr::summarize(
      n_synthetic = sum(.source_label == "synthetic"),
      n_total = dplyr::n()
    ) %>%
    dplyr::mutate(p_hat = n_synthetic / n_total) %>%
    dplyr::pull(p_hat)
  
  propensities <- discrimination$propensities %>%
    dplyr::pull(".pred_synthetic")
  
  # calculate the observed pMSE
  pmse <- mean((propensities - p_hat) ^ 2)
  
  discrimination$pmse <- pmse
  
  return(discrimination)
  
}