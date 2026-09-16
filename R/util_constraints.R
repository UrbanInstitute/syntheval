#' Evaluate constraints
#'
#' @param eval_data An `eval_data` object.
#' @param constraints_df_num A numeric constraints data frame in the style of `tidysynthesis`.
#' @param constraints_df_cat A categorical constraints data frame in the style of `tidysynthesis`.
#' @param na.rm a logical evaluating to `TRUE` or `FALSE` indicating whether `NA` values should be stripped 
#' before the computation proceeds.
#'
#' @returns A list with data frame with summary statistics for how often constraints apply and how often
#' constraints are satisfied.
#'
#' @export
#'
#' @examples
util_constraints <- function(eval_data, constraints_df_num = NULL, constraints_df_cat = NULL, na.rm = FALSE) {
  
  # check input data frames
  if (!is.null(constraints_df_num)) {
    
    correct_names <- c("var", "min", "max", "conditions")
    
    if (!all(names(constraints_df_num) == correct_names)) {
      
      stop('constraints_df_num must have columns: "var", "min", "max", "conditions"')
      
    }
    
    # iterate over each constraint row
    res_num <- purrr::pmap(
      .l = constraints_df_num,
      .f = function(...) {
        
        r <- tibble::tibble(...)
        
        # append if the constraint applies and if the constraint is met
        row_contraints <- eval_data[["synth_data"]] |>
          dplyr::mutate(
            # evaluate whether the constraint applies with the condition,
            # replacing NA with no applicable constraint
            .tidynum_cond = tidyr::replace_na(eval(parse(text = r$conditions)), FALSE),
            
            .num_constraint_met = (.tidynum_cond) & 
              (.data[[r$var]] <= r$max) &
              (.data[[r$var]] >= r$min)
            
          )
        
        # summarize the row-level data to a summary table and combine so there is
        # on row per variable
        row_contraints |>
          dplyr::summarize(
            n_constraints_applies = sum(.tidynum_cond, na.rm = na.rm),
            n_constraints_met = sum(.num_constraint_met, na.rm = na.rm),
            prop_constraint_applies = mean(.tidynum_cond, na.rm = na.rm)
          ) |>
          dplyr::mutate(
            prop_constraints_met = n_constraints_met / n_constraints_applies
          )
        
      }
      
    ) |>
      dplyr::bind_rows()
    
    res_num <- dplyr::bind_cols(
      constraints_df_num,
      res_num
      
    )
    
  } else {
    
    res_num <- NULL
    
  }
  
  if (!is.null(constraints_df_cat)) {
    
    correct_names <- c("var", "allowed", "forbidden", "conditions")
    
    if (!all(names(constraints_df_cat) == correct_names)) {
      
      stop('constraints_df_cat must have columns: "var", "allowed", "forbidden", "conditions"')
      
    }
    
    # each row must specify exactly one of allowed/forbidden, not both or neither
    row_has_allowed <- !is.na(constraints_df_cat$allowed)
    row_has_forbidden <- !is.na(constraints_df_cat$forbidden)
    
    if (any(row_has_allowed == row_has_forbidden)) {
      
      stop("each row of constraints_df_cat must specify exactly one of `allowed` or `forbidden` (not both, not neither)")
      
    }
    
    # iterate over each constraint row
    res_cat <- purrr::pmap(
      .l = constraints_df_cat,
      .f = function(...) {
        
        r <- tibble::tibble(...)
        
        # append if the constraint applies and if the constraint is met
        row_contraints <- eval_data[["synth_data"]] |>
          dplyr::mutate(
            # evaluate whether the constraint applies with the condition,
            # replacing NA with no applicable constraint
            .tidynum_cond = tidyr::replace_na(eval(parse(text = r$conditions)), FALSE),
            
            .num_constraint_met = (.tidynum_cond) & 
              (is.na(r$allowed) | .data[[r$var]] %in% r$allowed) &
              (is.na(r$forbidden) | !.data[[r$var]] %in% r$forbidden)
            
          )
        
        # summarize the row-level data to a summary table and combine so there is
        # on row per variable
        row_contraints |>
          dplyr::summarize(
            n_constraints_applies = sum(.tidynum_cond, na.rm = na.rm),
            n_constraints_met = sum(.num_constraint_met, na.rm = na.rm),
            prop_constraint_applies = mean(.tidynum_cond, na.rm = na.rm)
          ) |>
          dplyr::mutate(
            prop_constraints_met = n_constraints_met / n_constraints_applies
          )
        
      }
      
    ) |>
      dplyr::bind_rows()
    
    res_cat <- dplyr::bind_cols(
      constraints_df_cat,
      res_cat
    )
    
  } else {
    
    res_cat <- NULL
    
  }
  
  res <- list(
    constraints_cat = res_cat,
    constraints_num = res_num
  )
  
  return(res)
  
}


