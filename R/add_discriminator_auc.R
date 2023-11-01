#' Title
#'
#' @param discrimination 
#'
#' @return
#' 
#' @export
#'
add_discriminator_auc <- function(discrimination) {
  
  discriminator_auc <- yardstick::roc_auc_vec(
    truth = discrimination$propensities$.source_label, 
    estimate = discrimination$propensities$.pred_synthetic
  )
  
  discrimination$discriminator_auc <- discriminator_auc
  
  return(discrimination)
  
}