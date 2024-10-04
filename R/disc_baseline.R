#'
#' Compute baseline disclosure risk metrics using confidential data.
#' 
#' @param eval_data An `eval_data` object or a tibble / data.frame corresponding 
#' to the confidential data.
#' @param qid_keys A character vector of quasi-identifying keys. Must be provided
#' as `factor` type variables. Defaults to all factor variables in `eval_data`.
#' @param sens_keys An optional character vector of sensitive variables of interest. 
#' Must be disjoint from `qid_keys`, or `FALSE` to provide no l-diversity or t-closeness
#' metrics. Defaults to the complement of `qid_keys`. 
#' @param tclose_metric_cat A string describing the t-closeness distance metric 
#' between proportions of categorical variables. One of `"l1"` (L1 distance), 
#' `"l2"` (L2 distance), or `"linf"` (L-infinity distance), defaults to `"linf"`.
#' @param tclose_metric_numeric A string describing the t-closeness distance metric
#' between numeric variable empirical CDFs. One of `"ks"` (Kolmogorov-Smirnov), 
#' `"wass"` (Wasserstein L1 distance), `"cvm"` (Cramer-von-Mises distance), or
#' `"ad"` (Anderson-Darling), defaults, to `"ks"`. 
#' @param na.rm Boolean if TRUE, will remove `NA` values from numeric `sens_keys`.
#' 
#' @returns A `baseline_metrics` object. 
#' 
#' @export 
#' 
disc_baseline <- function(
    eval_data, 
    qid_keys = NULL,
    sens_keys = NULL,
    tclose_metric_cat = c("linf", "l1", "l2"),
    tclose_metric_numeric = c("ks", "wass", "cvm", "ad"),
    na.rm = FALSE
  ) {
  
  # default argument parsing
  tclose_metric_cat <- match.arg(tclose_metric_cat)
  tclose_metric_numeric <- match.arg(tclose_metric_numeric)
  
  # either use provided data.frame as-is or extract it from eval_data
  conf_data <- if (is_eval_data(eval_data)) eval_data$conf_data else eval_data
  stopifnot(is.data.frame(conf_data))
  
  conf_data_types <- unlist(purrr::map(conf_data, pillar::type_sum))
  
  # if no qid_keys provided, use all conf_data columns
  if (is.null(qid_keys)) { 
    
    qid_keys <- names(conf_data)[conf_data_types == "fct"]
    
  }

  # require factor types for qid_keys
  stopifnot(length(qid_keys) > 0)
  stopifnot(all(conf_data_types[qid_keys] == "fct"))
  
  # construct unique QID combinations
  qid_agg <- .aggregate_qid(conf_data, keys = qid_keys) 
  qid_metrics <- tidyr::pivot_longer(
    qid_agg,
    -dplyr::one_of(c("key_id", qid_keys)),
    names_to = "metric"
  )
  
  # return result early if not computing l-diversity or t-closeness metrics
  if (identical(sens_keys, FALSE)) {
    
    return(
      
      list(
        "qid_keys" = qid_keys,
        "qid_metrics" = qid_metrics,
        "sens_keys" = NULL,
        "sens_metrics" = NULL
      ) %>%
        structure(class = "baseline_metrics")
      
    )
    
  } 
  
  # if no sensitive keys provided use the complement of qid_keys
  if (is.null(sens_keys)) { 
    
    sens_keys <- setdiff(x = names(conf_data), y = qid_keys)
    
  }
  
  # ensure no overlap between qid_keys and sens_keys
  stopifnot(length(intersect(sens_keys, qid_keys)) == 0)
  
  # map sensitive keys to metrics
  sens_key_types <- unlist(conf_data_types[sens_keys])
  
  # calculate global distribution statistics
  complete_dist_stats <- purrr::map(
    .x = sens_keys, 
    .f = \(x) {
      
      # for factor variables...
      if (sens_key_types[[x]] == "fct") {
        
        return(
          # use the proportion of values in each factor level
          c(table(conf_data[[x]], exclude=NULL)) / 
            nrow(conf_data)
        )
        
      } else {
        
        if (!na.rm & any(is.na(conf_data[[x]]))) {
          
          warning(
            paste(
              "NA values will not be included in t-closeness calculations but",
              "will be part of sample size calculations for:", 
              x
            )
          )
          
        }
        # use observed values to construct empirical CDF
        return(if (na.rm) stats::na.omit(conf_data[[x]]) else conf_data[[x]])
        
      }
      
    }
  )
  names(complete_dist_stats) <- sens_keys
  
  # use unique, consistent QID key names constructed from .aggregate_qid
  conf_w_key_ids <- conf_data %>%
    dplyr::inner_join(
      qid_agg %>% 
        dplyr::select(dplyr::all_of(c(qid_keys, "key_id"))),
      by = qid_keys
    )
  
  # define per-metrics function per grouped data frame and key name 
  per_qik_metrics <- function(gdf, names) {
    
    # calculate distinct l-diversity across each sensitive key column
    l_div <- gdf %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(sens_keys), dplyr::n_distinct)
      )
    
    # define t-closeness function (defined here because it depends on 
    # `gdf` and `names`)
    t_closeness_func <- function(x) {
      
      if (sens_key_types[dplyr::cur_column()] == "fct") {
        
        # for factor variables, calculate per-level probabilities
        qid_prop <- c(table(x, exclude=NULL)) / nrow(gdf)
        complete_prop <- complete_dist_stats[[dplyr::cur_column()]]
        
        # calculate appropriate distance between probability vectors
        return(
          dplyr::case_when(
            tclose_metric_cat == "l1" ~ (
              sum(abs(qid_prop - complete_prop))
            ),
            tclose_metric_cat == "l2" ~ (
              sqrt(sum((qid_prop - complete_prop)**2))
            ),
            # default is L-infinity (maximum difference)
            TRUE ~ (
              max(abs(qid_prop - complete_prop))
            )
          )
        )
        
      } else {
        
        # for numerics, first collect samples for each column
        qid_samps <- dplyr::pull(gdf, dplyr::cur_column())
        complete_samps <- complete_dist_stats[[dplyr::cur_column()]]
        
        return(
          
          # calculate appropriate distance between empirical CDFs
          dplyr::case_when(
            tclose_metric_numeric == "wass" ~ (
              twosamples::wass_stat(qid_samps, complete_samps)
            ),
            tclose_metric_numeric == "cvm" ~ (
              twosamples::cvm_stat(qid_samps, complete_samps)
            ),
            tclose_metric_numeric == "ad" ~ (
              twosamples::ad_stat(qid_samps, complete_samps)
            ),
            # default is Kolmogorov-Smirnov
            TRUE ~ (
              twosamples::ks_stat(qid_samps, complete_samps)
            )
          )
          
        )
        
      }
      
    }
    
    # calculate t-closeness according to the specified metric
    t_close <- gdf %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(sens_keys), t_closeness_func))
    
    return(
      data.frame(
        "l_div" = l_div, 
        "t_close" = t_close,
        "key_id" = as.numeric(gdf[1, "key_id"])
      ) 
    )
    
  }

  # for observed keys, calculate additional metrics
  sens_metrics <- conf_w_key_ids %>% 
    dplyr::group_by(
      dplyr::across(dplyr::all_of(qid_keys))
    ) %>%
    dplyr::group_map(per_qik_metrics) %>%
    dplyr::bind_rows()
  
  # join together k-anonymity and l-diversity / t-closeness metrics...
  joined_sens_metrics <- dplyr::inner_join( 
    qid_agg %>% # first, select k-anonymity results with QID keys
      dplyr::select(dplyr::all_of(c("key_id", qid_keys, "raw_n", "prop"))),
    # next, create rows for each key_id per metric and sensitive variable
    tidyr::pivot_longer(
      sens_metrics, 
      # pivot longer on all variables except key_id
      -dplyr::one_of("key_id"),
      # break up metrics and associated variables (ex: "tcloseness.sens_col1") 
      names_sep = "\\.", 
      names_to = c("metric", "sens_var")
    ),
    by = "key_id"
  ) 
  
  result <- list(
    "qid_keys" = qid_keys,
    "qid_metrics" = qid_metrics,
    "sens_keys" = sens_keys,
    "sens_metrics" = joined_sens_metrics
  ) %>%
    structure(class = "baseline_metrics")
  
  return(result)
  
}

