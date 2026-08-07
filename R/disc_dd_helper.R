#'
#' Aggregate and count factor-level quasi-identifiers.
#' 
#' @param df A data.frame.
#' @param keys A character vector of column names.
#' 
#' @return A data.frame of aggregated quasi-identifiers.
#' 
.aggregate_qid <- function(df, keys) {
  
  return(
    df |> 
      dplyr::group_by(
        dplyr::across(dplyr::all_of(keys)), 
        .drop=FALSE
      ) |>
      dplyr::summarise(raw_n = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::mutate(prop = .data$raw_n / nrow(df)) |>
      dplyr::arrange(!!!rlang::syms(keys)) |>
      tibble::remove_rownames() |>
      tibble::rowid_to_column("key_id")
  )
  
}


#' 
#' Input validation for categorical disclosure risk metrics.
#' 
#' @param eval_data An `eval_data` object.
#' @param keys A character vector of column names, or NULL to use all column names.
#' 
#' @return `0` if all validation checks pass. 
#' 
.validate_eval_keys <- function(eval_data, keys = NULL) {
  
  # check eval_data components
  stopifnot(is_eval_data(eval_data)) 
  
  # determine if using inferred keys or user-specified keys
  if (is.null(keys)) {
    
    keys <- names(eval_data$conf_data)
    
  } else {
    
    stopifnot(all(keys %in% names(eval_data$conf_data)))
    
  } 
  
  # check all confidential data keys present in synthetic data 
  if (eval_data$n_rep > 1) {
   
    for (ix in seq(eval_data$n_rep)) {
      
      stopifnot(all(keys %in% names(eval_data$synth_data[[ix]])))
      
    }
     
  } else {
    
    stopifnot(all(keys %in% names(eval_data$synth_data)))
    
  }
  
  if (!is.null(eval_data$holdout_data)) {
    
    stopifnot(all(keys %in% names(eval_data$holdout_data)))
    
  }
  
  # check factor levels in confidential and synthetic data
  for (key in keys) {
    
    if (eval_data$n_rep > 1) {
      
      for (ix in seq(eval_data$n_rep)) {
        
        if (!identical(
          levels(dplyr::pull(eval_data$conf_data, key)), 
          levels(dplyr::pull(eval_data$synth_data[[ix]], key))
        )) {
          
          stop(
            "Key {key} has mismatched levels in confidential and synthetic data"
          )
          
        }
        
      }
      
    } else {
      
      if (!identical(
        levels(dplyr::pull(eval_data$conf_data, key)), 
        levels(dplyr::pull(eval_data$synth_data, key))
      )) {
        
        stop(
          "Key {key} has mismatched levels in confidential and synthetic data"
        )
        
      }
      
    }
    
    if (!is.null(eval_data$holdout_data)) {
      
      if (!identical(
        levels(dplyr::pull(eval_data$conf_data, key)), 
        levels(dplyr::pull(eval_data$holdout_data, key))
      )) {
        
        stop(
          "Key {key} has mismatched levels in confidential and holdout data"
        )
        
      }
      
    }
    
  }
  
  # return 0 if all checks passed
  return(0)
  
}

#' 
#' Compute conditional target distributions within quasi-identifier equivalence classes.
#' 
#' @param df A data.frame.
#' @param qid_keys A character vector of quasi-identifying column names.
#' @param target_keys A character vector of target column names.
#' 
#' @return A long-format tibble with columns `key_id`, one column per `qid_keys`,
#' `target_var`, `target_level`, `n`, and `prob` (the conditional probability of
#' `target_level` within the equivalence class defined by `qid_keys`).
#' 
.conditional_distributions <- function(df, qid_keys, target_keys) {
  
  eq_classes <- .aggregate_qid(df, keys = qid_keys)
  
  target_distributions <- purrr::map(
    .x = target_keys,
    .f = .target_distribution,
    df = df,
    qid_keys = qid_keys
  ) |>
    dplyr::bind_rows()
  
  result <- target_distributions |>
    dplyr::inner_join(
      dplyr::select(eq_classes, dplyr::all_of(c("key_id", qid_keys))),
      by = qid_keys
    ) |>
    dplyr::relocate(
      dplyr::all_of(c("key_id", qid_keys, "target_var", "target_level", "n", "prob"))
    )
  
  return(result)
  
}

#' 
#' Compute the conditional distribution of a single target variable within 
#' quasi-identifier equivalence classes.
#' 
#' @param target A single target column name.
#' @param df A data.frame.
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns for `qid_keys`, `target_var`, `target_level`, 
#' `n`, and `prob`.
#' 
.target_distribution <- function(target, df, qid_keys) {
  
  # count observations for each equivalence class x target level combination.
  # note: .drop = FALSE completes the full cross-product of every distinct 
  # value observed in qid_keys and target (including an actual `NA` in a 
  # qid_keys column), so every equivalence class gets one row per target 
  # level, with n = 0 for combinations that weren't observed. `total_n` is 
  # still computed via a join rather than an inline grouped sum() to avoid 
  # ambiguous vector recycling.
  counts <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(qid_keys, target))),
      .drop = FALSE
    ) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
    dplyr::rename(target_level = dplyr::all_of(target)) |>
    dplyr::mutate(target_level = as.character(.data$target_level))
  
  totals <- counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(qid_keys))) |>
    dplyr::summarize(total_n = sum(.data$n), .groups = "drop")
  
  result <- counts |>
    dplyr::left_join(totals, by = qid_keys) |>
    dplyr::mutate(
      prob = dplyr::if_else(
        .data$total_n == 0, 
        NA_real_, 
        .data$n / .data$total_n
      ),
      target_var = target
    ) |>
    dplyr::select(-dplyr::all_of("total_n"))
  
  return(result)
  
}

#' 
#' Discretize numeric columns for disclosure risk metrics
#' 
#' @param eval_data An `eval_data` object.
#' @param col_map A list mapping each column to its discretization parameters, one
#' of either "k" (for specifying the total number of categories) or "width" (for 
#' specifying the width of each bin)
#' 
#' @return An `eval_data` object.
#' 
#' @examples
#' 
#' prep_discrete_eval_data(
#'   eval_data(acs_conf, acs_lr_synths),
#'   col_map = list(
#'     "age" = list("k" = 10),
#'     "inctot" = list("width" = 10000)
#'    )
#'  )
#' 
#' @export
#' 
prep_discrete_eval_data <- function(eval_data, col_map) {
  
  stopifnot(is_eval_data(eval_data)) 
  
  eval_data_disc <- eval_data
  
  for (col in names(col_map)) {
    
    stopifnot(any(c("k", "width") %in% names(col_map[[col]])))
    
    # get the observed extrema from all eval_data components, starting with 
    # the confidential data
    col_min_candidates <- c(
      min(dplyr::pull(eval_data$conf_data, col), na.rm = TRUE)
    )
    col_max_candidates <- c(
      max(dplyr::pull(eval_data$conf_data, col), na.rm = TRUE)
    )
    
    # if using a single synthetic data replicate, add extrema to candidate list
    if (eval_data$n_rep == 1) {
      
      col_min_candidates <- c(
        col_min_candidates, 
        min(dplyr::pull(eval_data$synth_data, col), na.rm = TRUE)
      )
      
      col_max_candidates <- c(
        col_max_candidates,
        max(dplyr::pull(eval_data$synth_data, col), na.rm = TRUE)
      )
    
      # else add column-wise extrema to candidate list across replicates
    } else {
      
      col_min_candidates <- c(
        col_min_candidates, 
        min(
          purrr::map_dbl(
            .x = eval_data$synth_data,
            .f = ~ min(dplyr::pull(.x, col), na.rm = TRUE)
          )
        )
      )
      
      col_max_candidates <- c(
        col_max_candidates, 
        max(
          purrr::map_dbl(
            .x = eval_data$synth_data,
            .f = ~ max(dplyr::pull(.x, col), na.rm = TRUE)
          )
        )
      )
      
    }
    
    # if holdout data is supplied, add to candidates
    if (!is.null(eval_data$holdout_data)) {
      
      col_min_candidates <- c(
        col_min_candidates, 
        min(dplyr::pull(eval_data$holdout_data, col), na.rm = TRUE)
      )
      
      col_max_candidates <- c(
        col_max_candidates,
        max(dplyr::pull(eval_data$holdout_data, col), na.rm = TRUE)
      )
      
    }
    
    col_min <- min(col_min_candidates, na.rm = TRUE)
    col_max <- max(col_max_candidates, na.rm = TRUE)
    
    # apply col_map parameters to calculate discretization bounds
    if ("k" %in% names(col_map[[col]])) {
      
      breaks <- seq(
        from = col_min, 
        to = col_max, 
        length.out = col_map[[col]][["k"]] + 1
      )
      
    } else {
      
      breaks <- seq(
        from = col_min, 
        to = col_max + col_map[[col]][["width"]], 
        by = col_map[[col]][["width"]]
      )
      
    }
    
    # define mutate function 
    discretize_col <- function(data, col_name, breaks) {
      
      # apply non-NA breaks
      non_na_cut <- base::cut(
        x = dplyr::pull(data, col_name), 
        breaks = breaks,
        include.lowest = TRUE
      )
      
      # reintroduce NA as a factor level 
      
      return(
        data |>
          dplyr::mutate(
            {{col_name}} := factor(
              non_na_cut, 
              levels = c(NA, levels(non_na_cut)),
              exclude = NULL
            )
          )
      )
      
    }
    
    # apply bounds to all relevant columns
    eval_data_disc$conf_data <- discretize_col(
      data = eval_data_disc$conf_data,
      col_name = col,
      breaks = breaks
    )
    
    if (eval_data_disc$n_rep == 1) {
      
      eval_data_disc$synth_data <- discretize_col(
        data = eval_data_disc$synth_data,
        col_name = col,
        breaks = breaks
      )
      
    } else {
      
      for (ix in seq(eval_data_disc$n_rep)) {
        
        eval_data_disc$synth_data[[ix]] <- discretize_col(
          data = eval_data_disc$synth_data[[ix]],
          col_name = col,
          breaks = breaks
        )

      }
      
    }
    
    if (!is.null(eval_data_disc$holdout_data)) {
      
      eval_data_disc$holdout_data <- discretize_col(
        data = eval_data_disc$holdout_data,
        col_name = col,
        breaks = breaks
      )
      
    }
    
  }
  
  return(eval_data_disc)

}


