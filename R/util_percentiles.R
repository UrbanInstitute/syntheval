#' Calculate summary statistics for original and synthetic data.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param probs A numeric vector of probabilities with values in \[0,1\]. The 
#' percentiles are interpolated using an empirical CDF. It's possible that the
#' percentiles are an approximation; especially when weights are used.
#' @param weight_var_q A quoted name of a weight variable
#' @param group_by_q The quoted name(s) of a (or multiple) grouping variable(s)
#' @param drop_zeros A Boolean for if zeros should be dropped
#' @param common_vars A logical for if only common variables should be kept. 
#' This option will frequently result in an error because quantile() is strict 
#' about missing values.
#' @param synth_varnames A list of variables synthesized to filter on, else `NULL`
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of summary statistics.
#'
.util_percentiles <- function(
    synth_data,
    conf_data,
    probs = c(0.1, 0.5, 0.9),
    weight_var_q = NULL, 
    group_by_q = NULL,
    drop_zeros = FALSE,
    common_vars = TRUE,
    synth_varnames = NULL,
    na.rm = FALSE) {
  
  # catch binding error
  . <- NULL
  
  # create combined data
  combined_data <- .create_combined_data_pointwise(
    synth_data = synth_data,
    conf_data = conf_data,
    group_by_q = group_by_q,
    weight_var_q = weight_var_q, 
    common_vars = common_vars,
    synth_varnames = synth_varnames
  )
  
  # prep data for NA handling
  combined_data <- .prep_combined_data_for_na.rm_q(
    combined_data,
    na.rm = na.rm, 
    drop_zeros = drop_zeros,
    drop_zeros_exclude = group_by_q
  )
  na.rm_flag <- (na.rm | drop_zeros)
  
  # add weight var
  if (weight_var_q == "NULL") {
    
    summary_stats <- combined_data %>%
      dplyr::group_by(source, dplyr::across(dplyr::all_of(group_by_q))) %>%
      dplyr::reframe(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ stats::quantile(
            x = .x, 
            probs = probs, 
            na.rm = na.rm_flag
          )
        ),
        p = probs
      ) %>%
      dplyr::select("p", dplyr::everything()) %>%
      dplyr::ungroup() %>%
      tidyr::gather(key = "variable", value = "value", -"source", -"p", 
                    -dplyr::all_of(group_by_q)) %>%
      tidyr::spread(key = source, value = .data$value) %>%
      dplyr::arrange(.data$variable)
    
  } else {
    
    summary_stats <- combined_data %>% 
      dplyr::mutate(.temp_weight = .data[[weight_var_q]]) %>%
      dplyr::group_by(source, dplyr::across(dplyr::all_of(group_by_q))) %>%
        dplyr::reframe(
          dplyr::across(
            .cols = dplyr::everything(),
            .fns = ~ Hmisc::wtd.quantile(
              x = ., 
              weights = !!rlang::sym(".temp_weight"), 
              probs = probs, 
              na.rm = na.rm_flag
            )
          ),
          p = probs
        ) %>%
        dplyr::select("p", dplyr::everything()) %>%
        dplyr::ungroup() %>%
        tidyr::gather(key = "variable", value = "value", -"source", -"p", 
                      -dplyr::all_of(group_by_q)) %>%
        tidyr::spread(key = source, value = .data$value) %>%
        dplyr::arrange(.data$variable)
  }

  summary_stats <- summary_stats  %>%
    dplyr::mutate(
      difference = .data$synthetic - .data$original,
      proportion_difference = .data$difference / .data$original
    )
  
  # sort table by synthesis order and keep factor levels for variables that 
  # weren't synthesized
  if (is.null(synth_varnames)) {
    
    variable_order <- names(dplyr::select(combined_data, -source))
    
  } else {
    
    all_vars <- names(dplyr::select(combined_data, -source))
    
    other_vars <- setdiff(all_vars, synth_varnames)
    
    variable_order <- c(synth_varnames, other_vars)
    
  }
  
  summary_stats <- summary_stats %>%
    dplyr::mutate(
      variable = factor(.data$variable, levels = variable_order),
    ) %>%
    dplyr::arrange(.data$variable, .data$p) %>%
    dplyr::filter(dplyr::if_all(.cols = dplyr::all_of("variable"),
                                .fns = \(x) { !is.na(x) }))
    
  return(summary_stats)
  
}

#' Calculate summary statistics for original and synthetic data.
#'
#' @param eval_data An `eval_data` object.
#' @param probs A numeric vector of probabilities with values in \[0,1\]. The 
#' percentiles are interpolated using an empirical CDF. It's possible that the
#' percentiles are an approximation; especially when weights are used.
#' @param weight_var An unquoted name of a weight variable
#' @param group_by The unquoted name(s) of a (or multiple) grouping variable(s)
#' @param drop_zeros A Boolean for if zeros should be dropped
#' @param common_vars A logical for if only common variables should be kept. 
#' This option will frequently result in an error because quantile() is strict 
#' about missing values.
#' @param synth_vars A logical for if only synthesized variables should be kept
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of summary statistics (one per synthetic data replicate)
#'
#' @family utility metrics
#'
#' @export
#'
util_percentiles <- function(
    eval_data,
    probs = c(0.1, 0.5, 0.9),
    weight_var = NULL, 
    group_by = NULL,
    drop_zeros = FALSE,
    common_vars = TRUE,
    synth_vars = TRUE,
    na.rm = FALSE) {
  
  stopifnot(is_eval_data(eval_data))
  
  # argument parsing
  weight_var_q <- base::deparse(rlang::enexpr(weight_var))
  group_by_q <- purrr::map_chr(as.list(rlang::enexpr(group_by)), base::deparse)
  group_by_q <- group_by_q[2:length(group_by_q)] %>% purrr::discard(is.na)
  synth_varnames <- if (identical(synth_vars, TRUE)) eval_data$synth_vars else NULL
  
  if (eval_data$n_rep == 1) {
    
    return(
      .util_percentiles(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        probs = probs,
        weight_var_q = weight_var_q,
        group_by_q = group_by_q,
        drop_zeros = drop_zeros,
        common_vars = common_vars,
        synth_varnames = synth_varnames,
        na.rm = na.rm
      )
    )
    
  } else {
    
    return(
      purrr::map(
        .x = eval_data$synth_data,
        .f = \(sd) {
          
          .util_percentiles(
            conf_data = eval_data$conf_data, 
            synth_data = sd, 
            probs = probs,
            weight_var_q = weight_var_q,
            group_by_q = group_by_q,
            drop_zeros = drop_zeros,
            common_vars = common_vars,
            synth_varnames = synth_varnames,
            na.rm = na.rm
          )
          
        }
      )
    )
    
  }
  
}