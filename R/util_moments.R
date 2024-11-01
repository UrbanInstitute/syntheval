#' Calculate summary statistics for original and synthetic data.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param weight_var A quoted name of a weight variable
#' @param group_by The quoted name(s) of a (or multiple) grouping variable(s)
#' @param drop_zeros A logical for if zeros should be dropped
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_vars A list of variables synthesized to filter on, else `NULL`
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of summary statistics.
#'
.util_moments <- function(
    synth_data,
    conf_data,
    weight_var = NULL,
    group_by = NULL,
    drop_zeros = FALSE,
    common_vars = TRUE,
    synth_vars = NULL, 
    na.rm = FALSE) {
  
  # catch binding error
  . <- NULL

  # if provided, filter to synthesized variables
  if (!is.null(synth_vars)) {
    
    synthetic_data <- synth_data %>%
      dplyr::select(dplyr::all_of(synth_vars), 
                    dplyr::all_of(group_by))
    
    data <- data %>%
      dplyr::select(dplyr::all_of(synth_vars), 
                    dplyr::all_of(group_by))
        
  } else {
    
    synthetic_data <- synth_data
    data <- conf_data
  
  }
  
  
  # only keep variables in both data sets
  # keep group_by variables
  if (common_vars) {
    
    common_var_names <- intersect(names(data), names(synthetic_data))
  
    data <- data %>%
      dplyr::select(dplyr::all_of(common_var_names), 
                    dplyr::all_of(group_by))
    
    synthetic_data <- synthetic_data %>%
      dplyr::select(dplyr::all_of(common_var_names), 
                    dplyr::all_of(group_by))
    
  }
  
  # drop non-numeric variables
  data <- data %>%
    dplyr::select(tidyselect::where(is.numeric), dplyr::all_of(group_by))
  
  synthetic_data <- synthetic_data %>%
    dplyr::select(tidyselect::where(is.numeric), dplyr::all_of(group_by))
  
  # combine both data sources
  combined_data <- dplyr::bind_rows(
    `original` = data,
    `synthetic` = synthetic_data,
    .id = "source"
  )
  
  # prep data for NA handling
  combined_data <- .prep_combined_data_for_na.rm_q(
    combined_data,
    na.rm = na.rm, 
    drop_zeros = drop_zeros,
    drop_zeros_exclude = group_by
  )
  na.rm_flag <- (na.rm | drop_zeros)
  
  # add weight var
  if (class(weight_var) == "NULL") {
    
    combined_data <- combined_data %>%
      dplyr::mutate(.temp_weight = 1)
    
  } else {
    
    combined_data <- combined_data %>% 
      dplyr::mutate(.temp_weight = .data[[weight_var]])
    
  }
  
  # calculate summary statistics
  summary_stats <- combined_data %>%
    dplyr::group_by(source, dplyr::across(dplyr::all_of(group_by))) %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = -".temp_weight",
        .fns = list(
          count = ~ sum((. != 0) * .data$.temp_weight, na.rm = na.rm_flag),
          mean = ~ stats::weighted.mean(x = ., w = .data$.temp_weight, na.rm = na.rm_flag),
          sd = ~ weighted_sd(x = ., w = .data$.temp_weight, na.rm = na.rm_flag),
          skewness = ~ weighted_skewness(x = ., w = .data$.temp_weight, na.rm = na.rm_flag),
          kurtosis = ~ weighted_kurtosis(x = ., w = .data$.temp_weight, na.rm = na.rm_flag)
        )
      )
    ) %>%
    tidyr::gather(key = "variable", value = "value", -source, - {{ group_by }}) %>%
    tidyr::separate(col = .data$variable,
                    into = c("variable", "statistic"),
                    sep = "_(?!.*_)") %>%
    tidyr::spread(key = source, value = .data$value) %>%
    dplyr::ungroup() 
  
  summary_stats <- summary_stats  %>%
    dplyr::mutate(
      difference = .data$synthetic - .data$original,
      proportion_difference = .data$difference / .data$original
    )
    
  statistics_order <- 
    c("count", "mean", "sd", "skewness", "kurtosis")  
  
  if (is.null(synth_vars)) {
    
    variable_order <- variable_order <- names(dplyr::select(combined_data, -source))
    
  } else {
    
    all_vars <- names(dplyr::select(combined_data, -source))
    
    other_vars <- setdiff(all_vars, synth_vars)
    
    variable_order <- c(variable_order, other_vars)
    
  }
  
  summary_stats <- summary_stats %>%
    dplyr::mutate(
      variable = factor(.data$variable, levels = variable_order),
      statistic = factor(.data$statistic, levels = statistics_order)
    ) %>%
    dplyr::arrange(.data$variable, .data$statistic)
    
  return(summary_stats)
  
}



#' Calculate summary statistics for original and synthetic data.
#'
#' @param eval_data An `eval_data` object
#' @param weight_var An unquoted name of a weight variable
#' @param group_by The unquoted name of a (or multiple) grouping variable(s)
#' @param drop_zeros A logical for if zeros should be dropped
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_vars A list of variables synthesized to filter on, else `NULL`
#' @param na.rm A logical for ignoring `NA` values in computations.
#'
#' @return A `tibble` of summary statistics.
#'
#' @family utility metrics
#'
#' @export
#'
#'
util_moments <- function(
    eval_data,
    weight_var = NULL,
    group_by = NULL,
    drop_zeros = FALSE,
    common_vars = TRUE,
    synth_vars = NULL, 
    na.rm = FALSE) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep == 1) {
    
    return(
      .util_moments(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        weight_var = base::substitute(weight_var),
        group_by = base::substitute(group_by),
        drop_zeros = drop_zeros,
        common_vars = common_vars,
        synth_vars = synth_vars,
        na.rm = na.rm
      )
    )
    
  } else {
    
    return(
      purrr::map(
        .x = eval_data$synth_data,
        .f = \(sd) {
          
          .util_moments(
            conf_data = eval_data$conf_data, 
            synth_data = sd, 
            weight_var = weight_var,
            group_by = group_by,
            drop_zeros = drop_zeros,
            common_vars = common_vars,
            synth_vars = synth_vars,
            na.rm = na.rm
          )
          
        }
      )
    )
    
  }
  
}

