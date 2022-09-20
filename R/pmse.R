#' Compute observed pmse for a single synthetic data set
#'
#' @param data_orig A data frame with the original data
#' @param data_syn A data frame with the synthetic data
#' @param formula A formula for the predictive model
#' @param cp A hyperparameter for CART
#'
#' @return joint distributional utility metric (num)
#'
#' @importFrom stats predict
#' @importFrom rlang .data
#'
#' @export
#'
pmse <- function(data_orig, data_syn, formula = id ~ ., cp = 0.01) {
  
  ## combine original and synthetic data and add group indicator
  comb_data <- dplyr::bind_rows(
    `0` = data_orig,
    `1` = dplyr::select(data_syn, colnames(data_orig)),
    .id = "id"
  ) %>%
    dplyr::mutate(id = as.numeric(.data$id))
  
  ## fit model and predict prop scores
  prop_cart <- rpart::rpart(formula, data = comb_data, cp = cp, method = "class")
  pred_prob <- predict(prop_cart)[, 2]
  
  variable_importance <- summary(prop_cart)[["variable.importance"]]
  
  ## estimate observed pmse
  pmse <- mean((pred_prob - (nrow(data_syn) / nrow(comb_data))) ^ 2)
  
  return(
    list(
      pmse = pmse,
      variable_importance = variable_importance
    )
  )
  
}