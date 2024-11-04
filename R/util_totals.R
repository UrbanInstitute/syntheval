#' Calculate totals for original and synthetic data.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param weight_var_q A quoted name of a weight variable
#' @param group_by_q The quoted name(s) of a (or multiple) grouping variable(s)
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_varnames A list of variables synthesized to filter on, else `NULL`
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of totals.
#'
.util_totals <- function(
    synth_data,
    conf_data,
    weight_var_q = NULL,
    group_by_q = NULL,
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
  
  # add weight var
  if (weight_var_q == "NULL") {
    
    combined_data <- combined_data %>%
      dplyr::mutate(.temp_weight = 1)
    
  } else {
    
    combined_data <- combined_data %>% 
      dplyr::mutate(.temp_weight = .data[[weight_var_q]])
    
  }
  
  # calculate summary statistics
  totals <- combined_data %>%
    dplyr::group_by(source, dplyr::across(dplyr::all_of(group_by_q))) %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = -".temp_weight",
        .fns = list(
          count = ~ sum((. != 0) * .data$.temp_weight, na.rm = na.rm),
          total = ~ sum(. * .data$.temp_weight, na.rm = na.rm)
        )
      )
    ) %>%
    tidyr::gather(key = "variable", value = "value", 
                  -source, -dplyr::all_of(group_by_q)) %>%
    tidyr::separate(col = .data$variable,
                    into = c("variable", "statistic"),
                    sep = "_(?!.*_)") %>%
    tidyr::spread(key = source, value = .data$value) %>%
    dplyr::ungroup() 
  
  totals <- totals  %>%
    dplyr::mutate(
      difference = .data$synthetic - .data$original,
      proportion_difference = .data$difference / .data$original
    )
    
  statistics_order <- 
    c("count", "total")  
  
  # sort table by synthesis order and keep factor levels for variables that 
  # weren't synthesized
  if (is.null(synth_varnames)) { 
    
    variable_order <- names(dplyr::select(combined_data, -source))
    
  } else {
    
    all_vars <- names(dplyr::select(combined_data, -source))
    
    other_vars <- setdiff(all_vars, synth_varnames)
    
    variable_order <- c(synth_varnames, other_vars)
    
  }
  
  totals <- totals %>%
    dplyr::mutate(
      variable = factor(.data$variable, levels = variable_order),
      statistic = factor(.data$statistic, levels = statistics_order)
    ) %>%
    dplyr::arrange(.data$variable, .data$statistic)
    
  return(totals)
  
}

#' Calculate totals for original and synthetic data.
#'
#' @param eval_data An `eval_data` object
#' @param weight_var An unquoted name of a weight variable
#' @param group_by The unquoted name of a (or multiple) grouping variable(s)
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_vars A logical for if only synthesized variables should be kept
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of totals.
#'
#' @family utility metrics
#'
#' @export
#'
util_totals <- function(
    eval_data,
    weight_var = NULL, 
    group_by = NULL,
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
      .util_totals(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        weight_var_q = weight_var_q,
        group_by_q = group_by_q,
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
          
          .util_totals(
            conf_data = eval_data$conf_data, 
            synth_data = sd, 
            weight_var_q = weight_var_q,
            group_by_q = group_by_q,
            common_vars = common_vars,
            synth_varnames = synth_varnames,
            na.rm = na.rm
          )
          
        }
      )
    )
    
  }
  
}