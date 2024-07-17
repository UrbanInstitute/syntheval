#' Create evaluation data container
#'
#' @param conf_data A confidential dataframe
#' @param synth_data A single (or list of) dataframe(s) or `postsynth` object(s).
#' @param holdout_data An optional holdout dataframe containing the same columns
#' as the confidential dataframe
#'
#' @return An `eval_data` object.
#' 
#' @export
#'

eval_data <- function(conf_data, synth_data, holdout_data = NULL) {
  
  
  stopifnot(inherits(conf_data, "data.frame"))
  
  if (!is.null(holdout_data)) {
    
    # check holdout data is dataframe
    stopifnot(inherits(holdout_data, "data.frame"))
    
    # check holdout data has same columns as confidential data
    stopifnot(identical(names(conf_data), names(holdout_data)))
    
  }
  
  if (inherits(synth_data, "postsynth")) {
    
    synth_data <- synth_data$synthetic_data
    n_rep <- 1
    
  } else if (inherits(synth_data, "data.frame")) {
    
    n_rep <- 1
    
  } else {
    
    n_rep <- length(synth_data)
    
    if (inherits(synth_data[[1]], "postsynth")) {
      
      stopifnot(all(purrr::map_lgl(synth_data, ~ inherits(.x, "postsynth"))))
      
      synth_data <- purrr::map(synth_data, ~ .x$synthetic_data)
      
    } else {
      
      stopifnot(all(purrr::map_lgl(synth_data, ~ inherits(.x, "data.frame"))))
      
    }
    
  }
  
  eval_data <- list(
    conf_data = conf_data, 
    synth_data = synth_data,
    holdout_data = holdout_data,
    n_rep = n_rep
  )
      
  eval_data <- structure(eval_data, class = "eval_data")
  
  return(eval_data)
  
}