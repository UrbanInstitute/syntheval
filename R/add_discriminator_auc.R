#' Add discriminator AUC to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#'
#' @return A discrimination with discriminator AUC
#' 
#' @export
#'
add_discriminator_auc <- function(discrimination, split = TRUE) {
  
  if (split) {
  
  discriminator_auc <- discrimination$propensities %>%
    dplyr::group_by(.data$.sample) %>%
    yardstick::roc_auc(".source_label", ".pred_synthetic") %>%
    dplyr::mutate(.sample = factor(.data$.sample, levels = c("training", "testing"))) %>%
    dplyr::arrange(.data$.sample)
  
  } else {
    
    discriminator_auc <- discrimination$propensities %>%
      yardstick::roc_auc(".source_label", ".pred_synthetic") %>%
      dplyr::mutate(.sample = factor("overall", levels = "overall"))
    
  }
  
  discrimination$discriminator_auc <- discriminator_auc
  
  return(discrimination)
  
}