#' Calculate relative frequency tables for categorical variables
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param weight_var_q A quoted name of a weight variable
#' @param group_by_q The quoted name(s) of a (or multiple) grouping variable(s)
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_varnames A list of variables synthesized to filter on, else `NULL`
#' @param keep_empty_levels A logical for keeping all class levels in the group_by
#' statements, including missing levels.
#' @param na.rm A logical for ignoring `NA` values in proportion calculations.
#'
#' @return A tibble with variables, classes, and relative frequencies
#'
.util_proportions <- function(
    synth_data,
    conf_data,
    weight_var_q = NULL, 
    group_by_q = NULL,
    common_vars = TRUE,
    synth_varnames = TRUE,
    keep_empty_levels = FALSE,
    na.rm = FALSE) {
  
  combined_data <- .create_combined_data_pointwise(
    synth_data = synth_data,
    conf_data = conf_data,
    keep_numeric = FALSE,
    group_by_q = group_by_q,
    weight_var_q = weight_var_q, 
    common_vars = common_vars,
    synth_varnames = synth_varnames)
  
  # add weight var
  if (weight_var_q == "NULL") {
    
    combined_data <- combined_data %>%
      dplyr::mutate(.temp_weight = 1)
    
  } else {
    
    combined_data <- combined_data %>% 
      dplyr::mutate(.temp_weight = .data[[weight_var_q]]) %>%
      dplyr::select(-dplyr::all_of(weight_var_q))
    
  }
  
  group_by_weights <- combined_data %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c("source", group_by_q, ".temp_weight")),
      names_to = "variable", 
      values_to = "class"
    ) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_by_q, "source", "variable"))),
      .drop = !keep_empty_levels
    ) 
  
  # convert NAs to separate levels
  combined_data <- convert_na_to_level(combined_data)
  
  # lengthening combined data to find proportions for each level
  combined_data_long <- combined_data %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c("source", group_by_q, ".temp_weight")), 
      names_to = "variable", 
      values_to = "class"
    ) 
  
  # if flagged, join in empty levels to pivoted data
  if (keep_empty_levels) {
    
    # first, get explicit column names for variables where we need to keep 
    # empty levels (excludes variables in group_by, excluded by common_vars, etc)
    prop_col_names <- names(
      combined_data %>% 
        dplyr::select(-dplyr::all_of(c("source", group_by_q, ".temp_weight")))
      )
    
    extract_levels <- function(x) { 
      
      # for character columns, convert to factor before extracting levels
      if (pillar::type_sum(combined_data[[x]]) == "chr") {
        
        return(
          data.frame("class" = levels(factor(combined_data[[x]]))) %>% 
          dplyr::mutate("variable" = x) 
        )
        
      } else {
        
        return(
          # else, use pre-existing column factor levels (in case empty levels 
          # are unsynthesized)
          data.frame("class" = levels(combined_data[[x]])) %>% 
          dplyr::mutate("variable" = x)  
        )
        
      }
      
    }
    
    all_levels <- purrr::map(
      .x = prop_col_names,  
      .f = extract_levels
    ) %>%
      dplyr::bind_rows() %>% 
      # create one copy of the complete levels for each source and groupby level
      # by cross-joining
      dplyr::cross_join(
        combined_data %>% 
          dplyr::select(dplyr::all_of(c("source", group_by_q))) %>% 
          dplyr::distinct()
      )
    
    # create the join specification depending on whether group_by is specified
    if (identical(group_by_q, character(0))) {

      join_spec <- dplyr::join_by(class, "variable", source)
      
    } else {
      
      join_spec <- dplyr::join_by(class, "variable", source, group_by_q)
      
    }
    
    # join in all_levels to combined data and set weights to 0 where levels are 
    # unobserved
    combined_data <- dplyr::left_join(
      all_levels,
      combined_data_long,
      by = join_spec,
      na_matches = "na"
    ) %>% 
      tidyr::replace_na(list(".temp_weight" = 0))
    
  } else {
    
    combined_data <- combined_data_long
    
  }
  
  # if flagged, remove NA levels
  if (na.rm) {
    
    combined_data <- combined_data %>%
      dplyr::filter(
        !is.na(class) & (class != "NA")
      )
    
  }
  
  # calculating proportions for each level of each variable 
  combined_data <- combined_data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_by_q)), 
      .data$source, 
      .data$variable, 
      .data$class, 
      .drop = !keep_empty_levels
    ) %>%
    dplyr::summarise(.total_weight = sum(.data$.temp_weight)) %>%
    dplyr::mutate(prop = (.data$.total_weight) / sum(.data$.total_weight)) %>%
    dplyr::ungroup()
  
  # formatting results, getting proportion difference
  combined_data <- combined_data %>%
    tidyr::pivot_wider(names_from = source, values_from = "prop") %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_by_q)), 
      .data$variable, 
      .data$class, 
      .drop = !keep_empty_levels
    ) %>%
    dplyr::summarise(synthetic = sum(.data$synthetic, na.rm = TRUE),
                     original = sum(.data$original, na.rm = TRUE)) %>%
    dplyr::mutate(difference = .data$synthetic - .data$original) %>%
    dplyr::ungroup()
  
  # (group_by) -- variable -- class -- original -- synthetic -- difference
  return(combined_data)
  
}

#' 
#' Calculate relative frequency tables for categorical variables
#'
#' @param eval_data An `eval_data` object
#' @param weight_var An unquoted name of a weight variable
#' @param group_by The unquoted name(s) of a (or multiple) grouping variable(s)
#' @param common_vars A logical for if only common variables should be kept
#' @param synth_vars A logical for if *only* synthesized variables should be kept
#' @param keep_empty_levels A logical for keeping all class levels in the group_by
#' statements, including missing levels.
#' @param na.rm A logical for ignoring `NA` values in proportion calculations.
#'
#' @return A tibble with variables, classes, and relative frequencies
#' 
#' @family Utility metrics
#' 
#' @export
#'
util_proportions <- function(
    eval_data,
    weight_var = NULL, 
    group_by = NULL,
    common_vars = TRUE,
    synth_vars = TRUE,
    keep_empty_levels = FALSE,
    na.rm = FALSE
) {
  
  stopifnot(is_eval_data(eval_data))
  
  # argument parsing
  weight_var_q <- base::deparse(rlang::enexpr(weight_var))
  group_by_q <- purrr::map_chr(as.list(rlang::enexpr(group_by)), base::deparse)
  group_by_q <- group_by_q[2:length(group_by_q)] %>% purrr::discard(is.na)
  synth_varnames <- if (identical(synth_vars, TRUE)) eval_data$synth_vars else NULL
  
  if (eval_data$n_rep == 1) {
    
    return(
      .util_proportions(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        weight_var_q = weight_var_q,
        group_by_q = group_by_q,
        common_vars = common_vars,
        synth_varnames = synth_varnames,
        keep_empty_levels = keep_empty_levels,
        na.rm = na.rm
      )
    )
    
  } else {
    
    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {
        
        .util_proportions(
          conf_data = eval_data$conf_data, 
          synth_data = sd, 
          weight_var_q = weight_var_q,
          group_by_q = group_by_q,
          common_vars = common_vars,
          synth_varnames = synth_varnames,
          keep_empty_levels = keep_empty_levels,
          na.rm = na.rm
        )
        
      }
    )
    
    return(result)
    
  }
  
}