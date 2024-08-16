#' Perform a nearest-neighbor membership inference test on one synthetic dataset
#'
#' @param synth_data A dataframe with synthetic data generated from the data input
#' @param conf_data A data frame with a subset of the original data
#' @param holdout_data A dataframe with observations similar to the original but
#' not used to train the synthesizer. The data should have the same variables as
#' postsynth.
#' @param threshold_percentile Distances below the value associated with this 
#' percentile will be predicted as in the training data. If the 
#' threshold_percentile is not provided, the function calculates it with the 
#' following formula: `nrow(data)/(nrow(data) + nrow(holdout_data))`
#' @param summary Boolean if TRUE, returns summary statistics, if FALSE, returns 
#' two disaggregated dataframes of individual distances and ROC curve points.
#'
#' @return Either a list with precision, recall, the confusion matrix, and ROC AUC 
#' or a list with two disaggregated dataframes (if summary = FALSE). 
#'
nn_membership_inference <- function(
    synth_data, 
    conf_data, 
    holdout_data, 
    threshold_percentile = NULL,
    summary = TRUE
    ) {

  # calculate threshold percentile for when the data are imbalanced
  if (!is.null(threshold_percentile)) {
    
    # test the threshold percentile
    if (threshold_percentile < 0 || threshold_percentile > 1) {
      
      stop("error: threshold_percentile must be in [0, 1]")
      
    }
    
  } else {
    
    threshold_percentile <- nrow(conf_data) / (nrow(conf_data) + nrow(holdout_data))
    
  }
  
  # combine records from the training data and holdout data
  blended_data <- dplyr::bind_rows(
    training = conf_data,
    holdout = holdout_data,
    .id = "source"
  ) %>%
    dplyr::mutate(source = factor(source, levels = c("training", "holdout")))

  # for each record in the blended data, calculate the distance to the closest 
  # record in the synthetic data
  distances <- gower::gower_topn(
    x = dplyr::select(blended_data, -source), 
    y = synth_data,
    n = 1
  )

  # convert distances into predictions for if the record from the blended data
  # was used to train the synthetic data
  threshold <- stats::quantile(distances$distance, probs = threshold_percentile)

  prediction <- ifelse(distances$distance[1, ] <= threshold, "training", "holdout")

  pseudo_probabilities <- 1 - (distances$distance[1, ] / max(distances$distance[1, ]))

  blended_data <- dplyr::bind_cols(
    blended_data,
    distance = distances$distance[1, ],
    pseudo_probability = pseudo_probabilities,
    prediction = prediction
  ) %>%
    dplyr::mutate(prediction = factor(prediction, levels = c("training", "holdout")))

  if (summary) {
    
    # calculate metrics
    membership_metrics <- list(
      precision = yardstick::precision(blended_data, truth = source, estimate = prediction)$.estimate,
      recall = yardstick::recall(blended_data, truth = source, estimate = prediction)$.estimate,
      auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
      conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
    )
    
    return(membership_metrics)
    
  } else {
    
    # calculate complete ROC 
    roc <- yardstick::roc_curve(
      data = blended_data,
      truth = .data[["source"]],
      .data[["pseudo_probability"]]
    )
    
    return(
      list(
        "results" = blended_data,
        "roc" = roc
      )
    )
    
  }
  
}

#' Run a nearest-neighbor membership inference test 
#'
#' @param eval_data An `eval_data` object.
#' @param threshold_percentile Distances below the value associated with this 
#' percentile will be predicted as in the training data. If the 
#' threshold_percentile is not provided, the function calculates it with the 
#' following formula: `nrow(data) / (nrow(data) + nrow(holdout_data))`
#' @param summary Boolean if TRUE, returns summary statistics, if FALSE, returns 
#' two disaggregated dataframes of individual distances and ROC curve points.
#'
#' @return A list with precision, recall, the confusion matrix, and ROC AUC
#' 
#' @family Disclosure risk metrics
#' 
#' @export
#'
disc_mit <- function(eval_data, 
                     threshold_percentile = NULL,
                     summary = TRUE) {
  
  # if single replicate supplied
  if (eval_data[["n_rep"]] == 1) {
    
    return(
      nn_membership_inference(
        synth_data = eval_data[["synth_data"]], 
        conf_data = eval_data[["conf_data"]], 
        holdout_data = eval_data[["holdout_data"]], 
        threshold_percentile = threshold_percentile,
        summary = summary
      )
    ) 
    
  # if multiple replicates supplied
  } else {
    
    # calculate threshold percentile for when the data are imbalanced
    if (!is.null(threshold_percentile)) {
      
      # test the threshold percentile
      if (threshold_percentile < 0 || threshold_percentile > 1) {
        
        stop("error: threshold_percentile must be in [0, 1]")
        
      }
      
    } else {
      
      threshold_percentile <- (
        nrow(eval_data[["conf_data"]]) / (
          nrow(eval_data[["conf_data"]]) + nrow(eval_data[["holdout_data"]])
        )
      )
      
    }
    
    # concatenate synthetic data and add synthesis id
    synths <- purrr::imap(
      .x = eval_data[["synth_data"]],
      .f = \(x, idx) {
        dplyr::mutate(x, synth_id = idx)
      }
    ) 
    
    conf_data_id <- eval_data[["conf_data"]] %>% 
      tibble::rowid_to_column("nn_mi_id")
    
    holdout_data_id <- eval_data[["holdout_data"]] %>%
      tibble::rowid_to_column("nn_mi_id")
    
    blended_data <- dplyr::bind_rows(
      training = conf_data_id,
      holdout = holdout_data_id,
      .id = "source"
    ) %>%
      dplyr::mutate(source = factor(source, levels = c("training", "holdout")))
    
    # for each record in the blended data, calculate the distance to the closest 
    # record in each synthetic dataset
    synth_distances <- purrr::map(
      .x = synths, 
      .f = \(.x) { 
        
          gower::gower_topn(
              x = dplyr::select(blended_data, -source),
              y = .x,
              n = 1
            )$distance[1, ]
        
      }
    ) 
    
    all_distances <- purrr::reduce(
      .x = synth_distances,
      .f = c
    )
    
    pseudo_probabilities <- 1 - (all_distances / max(all_distances))
    
    # convert distances into predictions for if the record from the blended data
    # was used to train the synthetic data
    threshold <- stats::quantile(all_distances, probs = threshold_percentile)
    prediction <- ifelse(all_distances <= threshold, "training", "holdout")
    
    blended_data <- dplyr::bind_cols(
      dplyr::bind_rows(rep(list(blended_data), eval_data[["n_rep"]])),
      distance = all_distances,
      pseudo_probability = pseudo_probabilities,
      prediction = prediction
    ) %>%
      dplyr::mutate(prediction = factor(prediction, levels = c("training", "holdout")))
    
    if (summary) {
      
      # calculate metrics
      membership_metrics <- list(
        precision = yardstick::precision(blended_data, truth = source, estimate = prediction)$.estimate,
        recall = yardstick::recall(blended_data, truth = source, estimate = prediction)$.estimate,
        auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
        conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
      )
      
      return(membership_metrics)
      
    } else {
      
      # calculate complete ROC 
      roc <- yardstick::roc_curve(
        data = blended_data,
        truth = .data[["source"]],
        .data[["pseudo_probability"]]
      )
      
      return(
        list(
          "results" = blended_data,
          "roc" = roc
        )
      )
      
    }
    
  }
  
}


