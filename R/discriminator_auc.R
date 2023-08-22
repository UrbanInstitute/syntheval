#' Compute observed pmse for a single synthetic data set
#' 
#' @param postsynth A data frame with the synthetic data
#' @param data A postsynth object or data frame with confidential data
#' @param formula A formula for the predictive model
#' @param cp A hyperparameter for rpart
#' @param model A character vector with the model name of choice
#' 
#' @return joint distributional utility metric (num)
#' 
#' @importFrom stats predict
#' @importFrom rlang .data
#' 
#' @export
discriminator_auc <- function(
    postsynth, 
    data, 
    formula = id ~ ., 
    cp = 0.01, 
    model = "decision tree"
) {
  
  ## combine original and synthetic data and add group indicator
  comb_data <- dplyr::bind_rows(
    `0` = data,
    `1` = dplyr::select(postsynth$synthetic_data, colnames(data)),
    .id = "id"
  ) %>%
    dplyr::mutate(id = factor(.data$id))
  
  if (model == "decision tree") {
    
    ## fit model and predict prop scores
    prop_cart <- rpart::rpart(
      formula = formula, 
      data = comb_data, 
      cp = cp, 
      method = "class"
    )
    
    comb_data <- dplyr::bind_cols(
      comb_data,
      pred_prob = predict(prop_cart)[, 1]
    )
    
  } else if (model == "random forest") {
    
    prop_rf <- ranger::ranger(
      formula = formula, 
      data = comb_data, 
      probability = TRUE
    )
  
    comb_data <- dplyr::bind_cols(
      comb_data,
      pred_prob = predict(prop_rf, data = comb_data)$predictions[, 1]
    )
    
  }

  ## calculate the auc
  auc <- yardstick::roc_auc(data = comb_data, truth = id, estimate = pred_prob)
  
  return(
    list(
      auc = auc$.estimate
    )
  )
  
}


