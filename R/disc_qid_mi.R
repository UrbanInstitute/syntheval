#' 
#' Aggregate and count factor factor-level quasi-identifiers for eval_data
#' 
#' @param eval_data An `eval_data` object.
#' @param keys A character vector of column quasi-identifiers
#' 
#' @returns A data.frame of results
#' 
#' @export
#' 
aggregate_qid_eval <- function(eval_data, keys) {
  
  stopifnot(is_eval_data(eval_data))
  stopifnot(eval_data$n_rep > 1)
  
  # aggregate quasi-identifiers 
  conf_agg <- .aggregate_qid(eval_data$conf_data, keys)
  
  synth_aggs <- purrr::map(
    .x = eval_data$synth_data, 
    .f = \(x) { .aggregate_qid(x, keys) }
  )
  
  # join on key_id, which is consistently constructed from .aggregate_qid
  result <- dplyr::inner_join(
      conf_agg,
      dplyr::bind_rows(
        synth_aggs,
        .id = "synth_id"
      ) %>% dplyr::select(
        dplyr::all_of(c("synth_id", "key_id", "raw_n", "prop"))
      ), 
      by = "key_id",
      suffix = c("_conf", "_synth")
    ) %>% 
      dplyr::mutate(
        s_synth = (.data[["raw_n_synth"]] > 0),
        s_conf = (.data[["raw_n_conf"]] > 0),
        prop_abserr_conf = abs(.data[["prop_synth"]] - .data[["prop_conf"]])
      )
  
  if (is.null(eval_data$holdout_data)) {
    
    return(result)
    
  } else {
    
    holdout_agg <- .aggregate_qid(eval_data$holdout_data, keys)
    return(
      result %>%
        dplyr::inner_join(
          holdout_agg %>%
            dplyr::select(
              dplyr::all_of(c("key_id", "raw_n", "prop"))
            ) %>% dplyr::rename(
              "raw_n_holdout" = rlang::sym("raw_n"), 
              "prop_holdout" = rlang::sym("prop")
            ),
          by = "key_id"
        ) %>%
        dplyr::mutate(
          s_holdout = (.data[["raw_n_holdout"]] > 0),
          prop_abserr_holdout = abs(
            .data[["prop_synth"]] - .data[["prop_holdout"]]
          )
        )
    )
    
  }
}


#' 
#' Plot partition selection probabilities 
#' 
#' @param agg_eval_data Output from `aggregate_qid_eval`
#' @param keys A character vector of column names
#' @param max_k largest partition selection size
#' 
#' @returns A `ggplot2` plot. 
#' 
#' @export
#' 
plot_prob_qid_partition <- function(
    agg_eval_data, 
    keys,
    max_k = 20) { 
  
  probs <- agg_eval_data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(keys)),
      .drop = FALSE
    ) %>%
    dplyr::summarise(
      raw_n_conf = mean(.data[["raw_n_conf"]]), 
      s_synth = mean(.data[["s_synth"]])
    ) %>%
    dplyr::filter(.data[["raw_n_conf"]] <= max_k)
  
  return(
    
    ggplot2::ggplot(probs) + 
      ggplot2::geom_boxplot(
        ggplot2::aes(
          x = factor(.data[["raw_n_conf"]]), 
          y = .data[["s_synth"]]
        )
      ) + 
      ggplot2::xlab("n_orig") + 
      ggplot2::ylab("Estimated Prob(n_synth > 0)") + 
      ggplot2::ggtitle(paste("Quasi-ID keys: ", paste(keys, collapse=", ")))
    
  )
  
}


#' 
#' Plot absolute error probabilities 
#' 
#' @param agg_eval_data Output from `aggregate_qid_eval`
#' @param keys A character vector of column names
#' @param max_k largest partition selection size
#' @param qtiles Quantiles at which to estimate confidence of QID count
#' @param holdout boolean, use data from holdout instead of confidential 
#' 
#' @returns A `ggplot2` plot. 
#' 
#' @export
#' 
plot_prob_qid_abs_err <- function(agg_eval_data,
                          keys,
                          max_k = 20,
                          qtiles = c(.5, .75, .9),
                          holdout = FALSE) { 
  
  err_var <- if (holdout) "prop_abserr_holdout" else "prop_abserr_conf"
  
  probs <- agg_eval_data %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of("raw_n_conf"))) %>%
    dplyr::reframe(
      qtile_value = stats::quantile(.data[[err_var]], probs = qtiles),
      qtile = paste(qtiles * 100, "%ile", sep="")
    )

  return(
    
    ggplot2::ggplot(
      data = probs %>%
        dplyr::filter(.data[["raw_n_conf"]] <= max_k),
      mapping = ggplot2::aes(
        x = .data[["raw_n_conf"]], 
        y = .data[["qtile_value"]], 
        color = .data[["qtile"]]
        )
      ) + 
      ggplot2::geom_point() + 
      ggplot2::geom_line() + 
      ggplot2::xlab("n_orig") + 
      ggplot2::ylab("Estimated Quantile for |n_synth - n_conf|") + 
      ggplot2::ggtitle(paste("Quasi-ID keys: ", paste(keys, collapse=", ")))
    
  )
  
}
