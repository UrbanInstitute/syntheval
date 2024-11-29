#' Add discriminator AUC to discrimination object
#'
#' @param discrimination A discrimination object with propensities (likely 
#' added using add_propensities())
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#' @param group A list of variable names to group by
#' 
#' @return A discrimination object with propensities (likely added using 
#' add_propensities()) with discriminator AUC
#' 
#' @export
#'
add_discriminator_auc <- function(discrimination, group = c(),split = TRUE) {
  
  if (split) {
    
    discriminator_auc <- discrimination$propensities %>%
      dplyr::group_by(across(all_of(c(".sample", group))))%>%
      yardstick::roc_auc(".source_label", ".pred_synthetic") %>%
      dplyr::mutate(.sample = factor(.data$.sample, levels = c("training", "testing"))) %>%
      dplyr::arrange(.data$.sample) %>%
      dplyr::ungroup()
    
  } else {
    
    discriminator_auc <- discrimination$propensities %>%
      dplyr::group_by(across(all_of(group)))%>%
      yardstick::roc_auc(".source_label", ".pred_synthetic") %>%
      dplyr::mutate(.sample = factor("overall", levels = "overall"))
    
  }
  
  discrimination$discriminator_auc <- discriminator_auc
  
  return(discrimination)
  
}