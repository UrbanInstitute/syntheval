
#' Title
#'
#' @param postsynth 
#' @param data 
#' @param object A parsnip model specification or a 
#' [workflows::workflow()](http://127.0.0.1:30225/help/library/workflows/help/workflow). 
#' No tuning parameters are allowed.
#' @param save_fit is required for the pMSE ratio
#'
#' @return
#' 
#' @export
#'
add_propensities <- function(
    postsynth,
    data,
    recipe,
    spec,
    save_fit = TRUE
) {
  
  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
    variable_order <- 
      levels(postsynth$jth_synthesis_time$variable)
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  ## combine original and synthetic data and add group indicator
  comb_data <- dplyr::bind_rows(
    original = data,
    synthetic = dplyr::select(synthetic_data, colnames(data)),
    .id = "id"
  ) %>%
    dplyr::mutate(id = factor(.data$id))
  
  updated_recipe <- recipe %>%
    update_role(id, new_role = "outcome")
  
  # fit model
  wf <- workflow() %>%
    add_model(spec = spec) %>%
    add_recipe(recipe = recipe) 
  
  wf %>%
    fit(data = comb_data)
  
}





