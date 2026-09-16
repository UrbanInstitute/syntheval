#' Evaluate constraints
#'
#' @param eval_data An `eval_data` object.
#' @param constraints_df_num A numeric constraints data frame in the style of `tidysynthesis`.
#' @param constraints_df_cat A categorical constraints data frame in the style of `tidysynthesis`.
#' @param na.rm A logical evaluating to `TRUE` or `FALSE` indicating whether `NA` values should be stripped 
#' before the computation proceeds.
#'
#' @returns A list of data frames with summary statistics for how often constraints apply and how often
#' constraints are satisfied. Each summary data frame has a `source` column identifying whether the row
#' summarizes `conf_data`, `synth_data`, or `holdout_data`. For example, `constraints_num` for a single
#' numeric constraint looks like:
#'
#' | source | var | min | max | conditions | n_constraints_applies | n_constraints_met | prop_constraint_applies | prop_constraints_met |
#' |---|---|---|---|---|---|---|---|---|
#' | conf_data | var2 | 0 | 3 | var1 <= 2 | 2 | NA | 0.5 | NA |
#' | synth_data | var2 | 0 | 3 | var1 <= 2 | 2 | NA | 0.5 | NA |
#' | holdout_data | var2 | 0 | 3 | var1 <= 2 | 2 | NA | 0.5 | NA |
#'
#' @export
#'
#' @examples
#' ed <- eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]])
#'
#' constraints_df_num <- tibble::tribble(
#'   ~var, ~min, ~max, ~conditions,
#'   "age", 0, 17, "age <= 17"
#' )
#'
#' constraints_df_cat <- tibble::tribble(
#'   ~var, ~allowed, ~forbidden, ~conditions,
#'   # marst is always Single when age <= 18
#'   "marst", "Single", NA, "age <= 18",
#'   # empstat is never Employed when age <= 18
#'   "empstat", NA, "Employed", "age <= 18"
#' )
#'
#' util_constraints(
#'   eval_data = ed,
#'   constraints_df_num = constraints_df_num,
#'   constraints_df_cat = constraints_df_cat
#' )
util_constraints <- function(eval_data, constraints_df_num = NULL, constraints_df_cat = NULL, na.rm = FALSE) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data[["n_rep"]] > 1) {
    
    stop("only one synthesis is supported for now")
    
  }
  
  # gather whichever of conf_data/synth_data/holdout_data are available
  sources <- list(
    conf_data = eval_data[["conf_data"]],
    synth_data = eval_data[["synth_data"]],
    holdout_data = eval_data[["holdout_data"]]
  )
  
  sources <- purrr::compact(sources)
  
  # check input data frames
  if (!is.null(constraints_df_num)) {
    
    correct_names <- c("var", "min", "max", "conditions")
    
    if (!all(names(constraints_df_num) == correct_names)) {
      
      stop('constraints_df_num must have columns: "var", "min", "max", "conditions"')
      
    }
    
    res_num <- purrr::map(
      .x = sources,
      .f = ~ .util_constraints_num(
        data = .x, 
        constraints_df_num = constraints_df_num, 
        na.rm = na.rm
      )
    ) |>
      dplyr::bind_rows(.id = "source")
    
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
    
    res_cat <- purrr::map(
      .x = sources,
      .f = ~ .util_constraints_cat(
        data = .x, 
        constraints_df_cat = constraints_df_cat, 
        na.rm = na.rm
      )
    ) |>
      dplyr::bind_rows(.id = "source")
    
  } else {
    
    res_cat <- NULL
    
  }
  
  res <- list(
    constraints_cat = res_cat,
    constraints_num = res_num
  )
  
  return(res)
  
}

# evaluate numeric constraints for a single data frame
.util_constraints_num <- function(data, constraints_df_num, na.rm) {
  
  # iterate over each constraint row
  res_num <- purrr::pmap(
    .l = constraints_df_num,
    .f = function(...) {
      
      r <- tibble::tibble(...)
      
      # evaluate whether the constraint applies with the condition,
      # replacing NA with no applicable constraint. Worked example for
      # var = "var2", min = 0, max = 3, conditions = "var1 <= 2":
      #
      #   var1  var2  .tidynum_cond  .num_constraint_met
      #   1     NA    TRUE           NA     condition TRUE, var2 missing
      #   2     2     TRUE           TRUE   condition TRUE, 2 in [0, 3]
      #   3     3     FALSE          FALSE  condition FALSE, never checked
      #   4     4     FALSE          FALSE  condition FALSE, never checked
      row_contraints <- data |>
        dplyr::mutate(
          # evaluate whether the constraint applies with the condition,
          # replacing NA with no applicable constraint
          # `r$conditions` is a string such as "age <= 17"
          .tidynum_cond = tidyr::replace_na(eval(parse(text = r$conditions)), FALSE),
          
          .num_constraint_met = (.data[[".tidynum_cond"]]) & 
            (.data[[r$var]] <= r$max) &
            (.data[[r$var]] >= r$min)
          
        )
      
      # summarize the row-level data to a summary table and combine so there is
      # on row per variable
      row_contraints |>
        dplyr::summarize(
          n_constraints_applies = sum(.data[[".tidynum_cond"]], na.rm = na.rm),
          n_constraints_met = sum(.data[[".num_constraint_met"]], na.rm = na.rm),
          prop_constraint_applies = mean(.data[[".tidynum_cond"]], na.rm = na.rm)
        ) |>
        dplyr::mutate(
          prop_constraints_met = .data[["n_constraints_met"]] / .data[["n_constraints_applies"]]
        )
      
    }
    
  ) |>
    dplyr::bind_rows()
  
  result <- dplyr::bind_cols(
    constraints_df_num,
    res_num
  )
  
  return(result)
  
}

# evaluate categorical constraints for a single data frame
.util_constraints_cat <- function(data, constraints_df_cat, na.rm) {
  
  # iterate over each constraint row
  res_cat <- purrr::pmap(
    .l = constraints_df_cat,
    .f = function(...) {
      
      r <- tibble::tibble(...)
      
      # evaluate whether the constraint applies with the condition,
      # replacing NA with no applicable constraint. Worked example for
      # var = "var2", allowed = c("a", "b"), forbidden = NA, conditions = "var1 <= 2":
      #
      #   var1  var2  .tidynum_cond  .num_constraint_met
      #   1     NA    TRUE           NA     condition TRUE, var2 missing
      #   2     "a"   TRUE           TRUE   condition TRUE, "a" in allowed
      #   3     "c"   FALSE          FALSE  condition FALSE, never checked
      #   4     "c"   FALSE          FALSE  condition FALSE, never checked
      row_contraints <- data |>
        dplyr::mutate(
          # evaluate whether the constraint applies with the condition,
          # replacing NA with no applicable constraint
          # `r$conditions` is a string such as "age <= 17"
          .tidynum_cond = tidyr::replace_na(eval(parse(text = r$conditions)), FALSE),
          
          # `%in%` treats NA as never matching, which would silently mark a
          # missing target value as meeting an allowed/forbidden constraint;
          # force NA target values to NA here so missingness is not counted as met
          .num_constraint_met = (.data[[".tidynum_cond"]]) & 
            dplyr::if_else(
              is.na(.data[[r$var]]),
              NA,
              if (is.na(r$allowed)) !.data[[r$var]] %in% r$forbidden else .data[[r$var]] %in% r$allowed
            )
          
        )
      
      # summarize the row-level data to a summary table and combine so there is
      # on row per variable
      row_contraints |>
        dplyr::summarize(
          n_constraints_applies = sum(.data[[".tidynum_cond"]], na.rm = na.rm),
          n_constraints_met = sum(.data[[".num_constraint_met"]], na.rm = na.rm),
          prop_constraint_applies = mean(.data[[".tidynum_cond"]], na.rm = na.rm)
        ) |>
        dplyr::mutate(
          prop_constraints_met = .data[["n_constraints_met"]] / .data[["n_constraints_applies"]]
        )
      
    }
    
  ) |>
    dplyr::bind_rows()
  
  result <- dplyr::bind_cols(
    constraints_df_cat,
    res_cat
  )
  
  return(result)
  
}

