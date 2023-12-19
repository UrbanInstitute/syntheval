#' Add propensities for if an observation belongs to the synthetic data
#'
#' @param discrimination A discrimination object created by discrimination()
#' @param recipe A recipe object from library(recipes). A recipe can't be used
#' if a formula is provided.
#' @param formula A formula for the discriminator model. A formula can't be used
#' if a recipe is provided.
#' @param spec A model object from library(parsnip). If no recipe or formula is 
#' specified, then all variables are used as predictors.
#' @param prop The proportion of data to be retained for modeling/analysis in 
#' the training/testing split. The sampling is stratified by the original and
#' synthetic data.
#' @param save_fit A logical for if the final model should be saved
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
    prop = 3 / 4,
    save_fit = TRUE
) {
  
  if (!is_discrimination(discrimination)) {
    
    stop("Error: discrimination must be of class discrimination. Use discrimination() before add_propensities()")
    
  }
  
  if (!is.null(recipe) & !is.null(formula)) {
    
    stop("Error: Cannot have both recipe and formula")
    
  }
  
  if (is.null(recipe)) {
    
    if (is.null(formula)) {
    
      recipe <- recipes::recipe(.source_label ~ ., data = discrimination$combined_data)
    
    } else if (!is.null(formula)) {
      
      recipe <- recipes::recipe(formula, data = discrimination$combined_data)
      
    }
    
  }
  
  # create a workflow to organize the recipe and model
  wf <- workflows::workflow() %>%
    workflows::add_model(spec = spec) %>%
    workflows::add_recipe(recipe = recipe) 
  
  # make training/testing split
  data_split <- rsample::initial_split(
    data = discrimination$combined_data,
    prop = prop,
    strata = ".source_label"
  )
  
  # fit the model
  fitted_model <- wf %>%
    parsnip::fit(data = rsample::training(data_split))

  propensities_df <- dplyr::bind_cols(
    stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
    discrimination$combined_data
  ) %>%
    dplyr::mutate(
      .sample = dplyr::if_else(
        dplyr::row_number() %in% data_split$in_id, 
        true = "training", 
        false = "testing"
      )
    ) %>%
    dplyr::relocate(
      dplyr::all_of(c(".pred_synthetic", ".source_label", ".sample")), 
      dplyr::everything()
    )
  
  discrimination$discriminator <- fitted_model
  discrimination$propensities <- propensities_df
  
  return(discrimination)
  
}





