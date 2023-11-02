#' Add propensities for if an observation belongs to the synthetic data
#'
#' @param object A parsnip model specification or a 
#' [workflows::workflow()](http://127.0.0.1:30225/help/library/workflows/help/workflow). 
#' No tuning parameters are allowed.
#' @param save_fit is required for the pMSE ratio
#'
#' @return A discrimination object with propensities and a fitted model for
#' generating propensities
#' 
#' @export
#'
add_propensities <- function(
    discrimination,
    recipe = NULL,
    formula = NULL,
    spec,
    save_fit = TRUE
) {
  
  if (!is_discrimination(discrimination)) {
    
    stop("Error: discrimination must be of class discrimination. Use discrimination() before add_propensities()")
    
  }
  
  if (!is.null(recipe) & !is.null(formula)) {
    
    stop("Error: recipe and formula can't both be null")
    
  }
  
  if (is.null(recipe)) {
    
    if (is.null(formula)) {
    
      recipe <- recipes::recipe(.source_label ~ ., data = discrimination$combined_data)
    
    } else if (!is.null(formula)) {
      
      recipe <- recipes::recipe(formula, data = discrimination$combined_data)
      
    }
    
  }
  
  # fit model
  wf <- workflows::workflow() %>%
    workflows::add_model(spec = spec) %>%
    workflows::add_recipe(recipe = recipe) 
  
  fitted_model <- wf %>%
    parsnip::fit(data = discrimination$combined_data)

  propensities_df <- dplyr::bind_cols(
     stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
     discrimination$combined_data
  )
   
  discrimination$discriminator <- fitted_model
  discrimination$propensities <- propensities_df
  
  return(discrimination)
  
}





