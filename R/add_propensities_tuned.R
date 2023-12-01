#' Add propensities for if an observation belongs to the synthetic data
#'
#' @param discrimination A discrimination object created by discrimination()
#' @param recipe A recipe object from library(recipes)
#' @param formula A formula for the discriminator model
#' @param spec A model object from library(parsnip)
#' @param grid a tibble with hyperparameters for tuning
#' @param save_fit A logical for if the final model should be saved
#'
#' @return A discrimination object with propensities and a fitted model for
#' generating propensities
#' 
#' @family Utility metrics
#' 
#' @export
#'
add_propensities_tuned <- function(
    discrimination,
    recipe = NULL,
    formula = NULL,
    spec,
    grid,
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
  
  # set up workflow
  wf <- workflows::workflow() %>%
    workflows::add_model(spec = spec) %>%
    workflows::add_recipe(recipe = recipe) 
  
  # make training/testing split
  data_split <- rsample::initial_split(
    data = discrimination$combined_data,
    strata = ".source_label"
  )

  # create resamples for hyperparameter tuning
  folds <- rsample::vfold_cv(
    data = rsample::training(data_split),
    v = 10
  )

  # hyperparameter tune
  vfold_results <- wf %>%
    tune::tune_grid(
      resamples = folds,
      grid = grid
    )

  # add the tuned hyperparameters to the workflow
  tuned_wf <- 
    wf %>% 
    tune::finalize_workflow(tune::select_best(x = vfold_results, metric = "roc_auc"))
  
  # fit the model with the best hyperparameters on all of the training data
  final_fit <- 
    tuned_wf %>%
    tune::last_fit(split = data_split) 
  
  # finalize the workflow for predictions
  final_wf <- tune::extract_workflow(x = final_fit)
  
  # calculate the propensities
  propensities_df <- dplyr::bind_cols(
     stats::predict(final_wf, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
     discrimination$combined_data
  ) %>%
    dplyr::mutate(
      .sample = dplyr::if_else(
        dplyr::row_number() %in% data_split$in_id, 
        true = "training", 
        false = "testing"
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(c(".pred_synthetic", ".source_label", ".sample")), 
      dplyr::everything()
    )
   
  discrimination$discriminator <- final_wf
  discrimination$propensities <- propensities_df
  
  return(discrimination)
  
}





